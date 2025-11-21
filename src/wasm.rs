use wasm_encoder::{
    CodeSection, ExportKind, ExportSection, FieldType, Function, FunctionSection, HeapType,
    Instruction, Module, RefType, StorageType, TypeSection, ValType,
};

use crate::{
    diagnostics::{Diagnostic, SourceSpan},
    interpret::{self, AnnotatedBinding},
    parsing::{
        BinaryIntrinsicOperator, BindingAnnotation, BindingPattern, Expression, ExpressionLiteral,
        IntrinsicOperation, IntrinsicType, TargetLiteral,
    },
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum WasmType {
    I32,
    Struct(Vec<(String, WasmType)>),
}

impl WasmType {
    fn to_val_type(&self, ctx: &TypeContext) -> ValType {
        match self {
            WasmType::I32 => ValType::I32,
            WasmType::Struct(fields) => {
                let type_index = ctx
                    .get_type_index(fields)
                    .expect("Type should be registered");
                ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Concrete(type_index),
                })
            }
        }
    }
}

struct TypeContext {
    struct_types: Vec<Vec<(String, WasmType)>>,
    type_map: std::collections::HashMap<Vec<(String, WasmType)>, u32>,
}

impl TypeContext {
    fn new() -> Self {
        Self {
            struct_types: Vec::new(),
            type_map: std::collections::HashMap::new(),
        }
    }

    fn get_or_register_type(&mut self, fields: Vec<(String, WasmType)>) -> u32 {
        if let Some(&index) = self.type_map.get(&fields) {
            return index;
        }

        for (_, field_type) in &fields {
            if let WasmType::Struct(inner_fields) = field_type {
                self.get_or_register_type(inner_fields.clone());
            }
        }

        let index = self.struct_types.len() as u32;
        self.struct_types.push(fields.clone());
        self.type_map.insert(fields, index);
        index
    }

    fn get_type_index(&self, fields: &Vec<(String, WasmType)>) -> Option<u32> {
        self.type_map.get(fields).copied()
    }
}

struct WasmFunctionParam {
    name: String,
    ty: WasmType,
}

struct WasmFunctionExport {
    name: String,
    params: Vec<WasmFunctionParam>,
    body: Expression,
    return_type: WasmType,
}

pub fn compile_exports(context: &interpret::Context) -> Result<Vec<u8>, Diagnostic> {
    let exports = collect_wasm_exports(context)?;
    if exports.is_empty() {
        return Ok(Vec::new());
    }

    let mut module = Module::new();
    let mut type_section = TypeSection::new();
    let mut function_section = FunctionSection::new();
    let mut export_section = ExportSection::new();
    let mut code_section = CodeSection::new();

    let mut type_ctx = TypeContext::new();

    // First pass: Collect all types
    for export in &exports {
        let mut locals_types = std::collections::HashMap::new();
        for param in &export.params {
            if let WasmType::Struct(fields) = &param.ty {
                type_ctx.get_or_register_type(fields.clone());
            }
            locals_types.insert(param.name.clone(), param.ty.clone());
        }
        if let WasmType::Struct(fields) = &export.return_type {
            type_ctx.get_or_register_type(fields.clone());
        }
        collect_types(&export.body, &mut type_ctx, &mut locals_types)?;
    }

    // Emit types
    for fields in &type_ctx.struct_types {
        let wasm_fields = fields.iter().map(|(_, ty)| {
            let val_type = ty.to_val_type(&type_ctx);
            FieldType {
                element_type: StorageType::Val(val_type),
                mutable: true,
            }
        });

        type_section.rec(vec![wasm_encoder::SubType {
            is_final: false,
            supertype_idx: None,
            composite_type: wasm_encoder::CompositeType {
                inner: wasm_encoder::CompositeInnerType::Struct(wasm_encoder::StructType {
                    fields: wasm_fields.collect::<Vec<_>>().into_boxed_slice(),
                }),
                shared: false,
            },
        }]);
    }

    let mut function_type_indices = Vec::new();
    let struct_type_count = type_ctx.struct_types.len() as u32;
    let mut next_type_index = struct_type_count;

    for export in &exports {
        let param_types: Vec<ValType> = export
            .params
            .iter()
            .map(|param| param.ty.to_val_type(&type_ctx))
            .collect();
        let return_types = vec![export.return_type.to_val_type(&type_ctx)];

        type_section.function(param_types, return_types);
        function_type_indices.push(next_type_index);
        function_section.function(next_type_index);
        next_type_index += 1;
    }

    let mut next_function_index = 0u32;

    for (_i, export) in exports.iter().enumerate() {
        let mut locals = Vec::new();
        let mut local_indices = std::collections::HashMap::new();
        let mut locals_types = std::collections::HashMap::new();

        // Add parameters
        for (j, param) in export.params.iter().enumerate() {
            local_indices.insert(param.name.clone(), j as u32);
            locals_types.insert(param.name.clone(), param.ty.clone());
        }

        // Collect additional locals from body
        let body_locals = collect_locals(&export.body, &mut locals_types)?;
        for (name, ty) in body_locals {
            if !local_indices.contains_key(&name) {
                local_indices.insert(name.clone(), (export.params.len() + locals.len()) as u32);
                locals.push(ty.clone());
            }
        }

        let mut func = Function::new(locals.iter().map(|ty| (1, ty.to_val_type(&type_ctx))));

        emit_expression(
            &export.body,
            &local_indices,
            &locals_types,
            &mut func,
            &type_ctx,
        )?;
        func.instruction(&Instruction::End);
        code_section.function(&func);

        export_section.export(&export.name, ExportKind::Func, next_function_index);
        next_function_index += 1;
    }

    module.section(&type_section);
    module.section(&function_section);
    module.section(&export_section);
    module.section(&code_section);

    Ok(module.finish())
}

fn collect_types(
    expr: &Expression,
    ctx: &mut TypeContext,
    locals_types: &mut std::collections::HashMap<String, WasmType>,
) -> Result<(), Diagnostic> {
    match expr {
        Expression::Struct(fields, _) => {
            let mut field_types = Vec::new();
            for (name, value) in fields {
                collect_types(value, ctx, locals_types)?;
                let ty = infer_type(value, locals_types)?;
                field_types.push((name.0.clone(), ty));
            }
            field_types.sort_by(|a, b| a.0.cmp(&b.0));
            ctx.get_or_register_type(field_types);
        }
        Expression::Block(exprs, _) => {
            for e in exprs {
                collect_types(e, ctx, locals_types)?;
            }
        }
        Expression::Binding(binding, _) => {
            collect_types(&binding.expr, ctx, locals_types)?;
            let ty = infer_type(&binding.expr, locals_types)?;
            let mut locals = Vec::new();
            collect_locals_for_pattern(binding.pattern.clone(), ty, locals_types, &mut locals)?;
        }
        Expression::FunctionCall {
            function, argument, ..
        } => {
            collect_types(function, ctx, locals_types)?;
            collect_types(argument, ctx, locals_types)?;
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_types(condition, ctx, locals_types)?;
            collect_types(then_branch, ctx, locals_types)?;
            if let Some(else_branch) = else_branch {
                collect_types(else_branch, ctx, locals_types)?;
            }
        }
        Expression::IntrinsicOperation(IntrinsicOperation::Binary(left, right, _), _) => {
            collect_types(left, ctx, locals_types)?;
            collect_types(right, ctx, locals_types)?;
        }
        Expression::PropertyAccess { object, .. } => {
            collect_types(object, ctx, locals_types)?;
        }
        _ => {}
    }
    Ok(())
}

fn infer_type(
    expr: &Expression,
    locals_types: &std::collections::HashMap<String, WasmType>,
) -> Result<WasmType, Diagnostic> {
    match expr {
        Expression::Literal(ExpressionLiteral::Number(_), _) => Ok(WasmType::I32),
        Expression::Literal(ExpressionLiteral::Boolean(_), _) => Ok(WasmType::I32),
        Expression::Struct(fields, _) => {
            let mut field_types = Vec::new();
            for (name, value) in fields {
                let ty = infer_type(value, locals_types)?;
                field_types.push((name.0.clone(), ty));
            }
            field_types.sort_by(|a, b| a.0.cmp(&b.0));
            Ok(WasmType::Struct(field_types))
        }
        Expression::Identifier(identifier, span) => {
            locals_types.get(&identifier.0).cloned().ok_or_else(|| {
                Diagnostic::new(format!("Unknown identifier `{}`", identifier.0)).with_span(*span)
            })
        }
        Expression::FunctionCall { .. } => Ok(WasmType::I32),
        Expression::PropertyAccess {
            object,
            property,
            span,
        } => {
            let object_type = infer_type(object, locals_types)?;
            if let WasmType::Struct(fields) = object_type {
                if let Some((_, field_type)) = fields.iter().find(|(n, _)| n == property) {
                    Ok(field_type.clone())
                } else {
                    Err(
                        Diagnostic::new(format!("Field `{}` not found in struct", property))
                            .with_span(*span),
                    )
                }
            } else {
                Err(Diagnostic::new("Property access on non-struct type").with_span(*span))
            }
        }
        Expression::IntrinsicOperation(..) => Ok(WasmType::I32),
        Expression::If { then_branch, .. } => infer_type(then_branch, locals_types),
        Expression::Block(exprs, _) => {
            if let Some(last) = exprs.last() {
                infer_type(last, locals_types)
            } else {
                Ok(WasmType::I32)
            }
        }
        Expression::Binding(..) => Ok(WasmType::I32),
        _ => Ok(WasmType::I32),
    }
}

fn collect_wasm_exports(
    context: &interpret::Context,
) -> Result<Vec<WasmFunctionExport>, Diagnostic> {
    let mut exports = Vec::new();
    for binding in context.annotated_bindings() {
        let AnnotatedBinding {
            name,
            annotations,
            value,
        } = binding;
        let Some(annotation_span) = wasm_annotation_span(&annotations) else {
            continue;
        };
        let export = lower_function_export(context, &name, value, annotation_span)?;
        exports.push(export);
    }
    exports.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(exports)
}

fn lower_function_export(
    context: &interpret::Context,
    binding_name: &str,
    value: Expression,
    annotation_span: SourceSpan,
) -> Result<WasmFunctionExport, Diagnostic> {
    let Expression::Function {
        parameter,
        return_type,
        body,
        ..
    } = value
    else {
        return Err(Diagnostic::new(format!(
            "Only functions can be exported to wasm (binding `{binding_name}`)"
        ))
        .with_span(annotation_span));
    };

    let return_type = resolve_type(context, &return_type)?;
    let params = extract_function_params(context, parameter)?;

    Ok(WasmFunctionExport {
        name: binding_name.to_string(),
        params,
        body: *body,
        return_type,
    })
}

fn resolve_type(context: &interpret::Context, expr: &Expression) -> Result<WasmType, Diagnostic> {
    match expr {
        Expression::IntrinsicType(IntrinsicType::I32, _) => Ok(WasmType::I32),
        Expression::IntrinsicType(IntrinsicType::Boolean, _) => Ok(WasmType::I32),
        Expression::Struct(fields, _) => {
            let mut field_types = Vec::new();
            for (name, value) in fields {
                let ty = resolve_type(context, value)?;
                field_types.push((name.0.clone(), ty));
            }
            field_types.sort_by(|a, b| a.0.cmp(&b.0));
            Ok(WasmType::Struct(field_types))
        }
        Expression::Identifier(identifier, span) => {
            if let Some((binding, _)) = context.bindings.get(&identifier.0) {
                match binding {
                    interpret::BindingContext::Bound(value)
                    | interpret::BindingContext::BoundPreserved(value) => {
                        resolve_type(context, value)
                    }
                    _ => Err(Diagnostic::new(format!(
                        "Type alias `{}` is not bound to a value",
                        identifier.0
                    ))
                    .with_span(*span)),
                }
            } else {
                Err(Diagnostic::new(format!("Unknown type `{}`", identifier.0)).with_span(*span))
            }
        }
        Expression::AttachImplementation { type_expr, .. } => resolve_type(context, type_expr),
        _ => Err(Diagnostic::new("Unsupported type for WASM").with_span(expr.span())),
    }
}

fn wasm_annotation_span(annotations: &[BindingAnnotation]) -> Option<SourceSpan> {
    annotations.iter().find_map(|annotation| match annotation {
        BindingAnnotation::Export(target_expr, span)
            if matches!(
                target_expr,
                Expression::Literal(ExpressionLiteral::Target(TargetLiteral::WasmTarget), _)
            ) =>
        {
            Some(*span)
        }
        _ => None,
    })
}

fn extract_function_params(
    context: &interpret::Context,
    pattern: BindingPattern,
) -> Result<Vec<WasmFunctionParam>, Diagnostic> {
    match pattern {
        BindingPattern::Struct(fields, _) => {
            let mut params = Vec::new();
            for (_name, sub_pattern) in fields {
                // We expect sub_pattern to be a TypeHint for the field
                // But wait, in `fn({x: i32})`, the pattern for field `x` is `TypeHint(Identifier(x), i32)`.
                // The name in `fields` is the field name in the struct literal if it were a literal?
                // No, in BindingPattern::Struct, it's `(Identifier, BindingPattern)`.
                // If I write `fn({x: i32})`, the parser produces `Struct` pattern.
                // The identifier is `x`. The sub_pattern is `TypeHint(Identifier(x), i32)`.
                // Actually, let's look at `parse_struct_binding_pattern_with_source`.
                // It parses `Identifier` then optional `:` pattern.
                // If `x: i32`, it parses `x` as identifier, then `i32` as type hint?
                // No, `parse_binding_pattern_with_source` handles type hint.

                // Let's assume sub_pattern handles the extraction.
                let sub_params = extract_function_params(context, sub_pattern)?;
                params.extend(sub_params);
            }
            Ok(params)
        }
        BindingPattern::Annotated { pattern, .. } => extract_function_params(context, *pattern),
        BindingPattern::TypeHint(inner, ty_expr, _) => {
            let ty = resolve_type(context, &ty_expr)?;
            let name = extract_identifier_from_pattern(*inner)?;
            Ok(vec![WasmFunctionParam { name, ty }])
        }
        BindingPattern::Literal(_, _) => Err(
            Diagnostic::new("Literal patterns cannot be used in function parameters")
                .with_span(pattern.span()),
        ),
        BindingPattern::Identifier(_, _) => Err(Diagnostic::new(
            "Wasm exports currently require parameter type hints",
        )
        .with_span(pattern.span())),
    }
}

fn extract_identifier_from_pattern(pattern: BindingPattern) -> Result<String, Diagnostic> {
    match pattern {
        BindingPattern::Identifier(identifier, _) => Ok(identifier.0),
        BindingPattern::Annotated { pattern, .. } => extract_identifier_from_pattern(*pattern),
        BindingPattern::TypeHint(pattern, _, _) => extract_identifier_from_pattern(*pattern),
        BindingPattern::Struct(ref items, _) => {
            for (_, pat) in items {
                if let Ok(name) = extract_identifier_from_pattern(pat.clone()) {
                    return Ok(name);
                }
            }
            Err(
                Diagnostic::new("Struct patterns require at least one identifier")
                    .with_span(pattern.span()),
            )
        }
        BindingPattern::Literal(_, _) => Err(
            Diagnostic::new("Literal patterns cannot be used in function parameters")
                .with_span(pattern.span()),
        ),
    }
}

fn collect_locals(
    expr: &Expression,
    locals_types: &mut std::collections::HashMap<String, WasmType>,
) -> Result<Vec<(String, WasmType)>, Diagnostic> {
    let mut locals = Vec::new();
    match expr {
        Expression::Binding(binding, _) => {
            let expr_type = infer_type(&binding.expr, locals_types)?;
            collect_locals_for_pattern(
                binding.pattern.clone(),
                expr_type,
                locals_types,
                &mut locals,
            )?;

            locals.extend(collect_locals(&binding.expr, locals_types)?);
        }
        Expression::Block(exprs, _) => {
            for e in exprs {
                locals.extend(collect_locals(e, locals_types)?);
            }
        }
        Expression::Function { body: _body, .. } => {}
        Expression::IntrinsicOperation(IntrinsicOperation::Binary(left, right, _), _) => {
            locals.extend(collect_locals(left, locals_types)?);
            locals.extend(collect_locals(right, locals_types)?);
        }
        Expression::FunctionCall {
            function, argument, ..
        } => {
            locals.extend(collect_locals(function, locals_types)?);
            locals.extend(collect_locals(argument, locals_types)?);
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            locals.extend(collect_locals(condition, locals_types)?);
            locals.extend(collect_locals(then_branch, locals_types)?);
            if let Some(else_branch) = else_branch {
                locals.extend(collect_locals(else_branch, locals_types)?);
            }
        }
        _ => {}
    }
    Ok(locals)
}

fn collect_locals_for_pattern(
    pattern: BindingPattern,
    expr_type: WasmType,
    locals_types: &mut std::collections::HashMap<String, WasmType>,
    locals: &mut Vec<(String, WasmType)>,
) -> Result<(), Diagnostic> {
    match pattern {
        BindingPattern::Identifier(identifier, _) => {
            locals.push((identifier.0.clone(), expr_type.clone()));
            locals_types.insert(identifier.0, expr_type);
            Ok(())
        }
        BindingPattern::Struct(items, span) => {
            let WasmType::Struct(field_types) = expr_type else {
                return Err(Diagnostic::new("Struct pattern requires struct value type").with_span(span));
            };

            for (field_identifier, field_pattern) in items {
                let field_type = field_types
                    .iter()
                    .find(|(name, _)| name == &field_identifier.0)
                    .map(|(_, ty)| ty.clone())
                    .ok_or_else(|| {
                        Diagnostic::new(format!("Missing field {} in struct type", field_identifier.0))
                            .with_span(field_pattern.span())
                    })?;
                collect_locals_for_pattern(field_pattern, field_type, locals_types, locals)?;
            }
            Ok(())
        }
        BindingPattern::TypeHint(inner, _, _) => {
            collect_locals_for_pattern(*inner, expr_type, locals_types, locals)
        }
        BindingPattern::Annotated { pattern, .. } => {
            collect_locals_for_pattern(*pattern, expr_type, locals_types, locals)
        }
        BindingPattern::Literal(_, _) => Ok(()),
    }
}

fn emit_expression(
    expr: &Expression,
    locals: &std::collections::HashMap<String, u32>,
    locals_types: &std::collections::HashMap<String, WasmType>,
    func: &mut Function,
    type_ctx: &TypeContext,
) -> Result<(), Diagnostic> {
    match expr {
        Expression::Literal(ExpressionLiteral::Number(value), _) => {
            func.instruction(&Instruction::I32Const(*value));
            Ok(())
        }
        Expression::Literal(ExpressionLiteral::Boolean(value), _) => {
            func.instruction(&Instruction::I32Const(if *value { 1 } else { 0 }));
            Ok(())
        }
        Expression::Identifier(identifier, span) => {
            let local_index = locals.get(&identifier.0).copied().ok_or_else(|| {
                Diagnostic::new(format!(
                    "Identifier `{}` is not a local variable or parameter",
                    identifier.0
                ))
                .with_span(*span)
            })?;
            func.instruction(&Instruction::LocalGet(local_index));
            Ok(())
        }
        Expression::IntrinsicOperation(IntrinsicOperation::Binary(left, right, op), _) => {
            emit_expression(left, locals, locals_types, func, type_ctx)?;
            emit_expression(right, locals, locals_types, func, type_ctx)?;
            match op {
                BinaryIntrinsicOperator::I32Add => func.instruction(&Instruction::I32Add),
                BinaryIntrinsicOperator::I32Subtract => func.instruction(&Instruction::I32Sub),
                BinaryIntrinsicOperator::I32Multiply => func.instruction(&Instruction::I32Mul),
                BinaryIntrinsicOperator::I32Divide => func.instruction(&Instruction::I32DivS),
                BinaryIntrinsicOperator::I32Equal => func.instruction(&Instruction::I32Eq),
                BinaryIntrinsicOperator::I32NotEqual => func.instruction(&Instruction::I32Ne),
                BinaryIntrinsicOperator::I32LessThan => func.instruction(&Instruction::I32LtS),
                BinaryIntrinsicOperator::I32GreaterThan => func.instruction(&Instruction::I32GtS),
                BinaryIntrinsicOperator::I32LessThanOrEqual => {
                    func.instruction(&Instruction::I32LeS)
                }
                BinaryIntrinsicOperator::I32GreaterThanOrEqual => {
                    func.instruction(&Instruction::I32GeS)
                }
                BinaryIntrinsicOperator::BooleanAnd => func.instruction(&Instruction::I32And),
                BinaryIntrinsicOperator::BooleanOr => func.instruction(&Instruction::I32Or),
                BinaryIntrinsicOperator::BooleanXor => func.instruction(&Instruction::I32Xor),
            };
            Ok(())
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            emit_expression(condition, locals, locals_types, func, type_ctx)?;
            let result_type = infer_type(then_branch, locals_types)?;
            let wasm_result_type = result_type.to_val_type(type_ctx);

            func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
                wasm_result_type,
            )));
            emit_expression(then_branch, locals, locals_types, func, type_ctx)?;
            func.instruction(&Instruction::Else);
            if let Some(else_expr) = else_branch {
                emit_expression(else_expr, locals, locals_types, func, type_ctx)?;
            } else {
                if matches!(wasm_result_type, ValType::I32) {
                    func.instruction(&Instruction::I32Const(0));
                } else {
                    return Err(Diagnostic::new(
                        "If expression returning struct must have else branch",
                    )
                    .with_span(expr.span()));
                }
            }
            func.instruction(&Instruction::End);
            Ok(())
        }
        Expression::Binding(binding, _) => {
            if let (
                BindingPattern::Struct(pattern_fields, _),
                Expression::Struct(value_fields, _),
            ) = (&binding.pattern, &binding.expr)
            {
                let mut comparisons_emitted = false;
                for (field_identifier, field_pattern) in pattern_fields {
                    let value_expr = value_fields
                        .iter()
                        .find(|(id, _)| id.0 == field_identifier.0)
                        .map(|(_, expr)| expr)
                        .ok_or_else(|| {
                            Diagnostic::new(format!(
                                "Missing field {} in struct binding",
                                field_identifier.0
                            ))
                            .with_span(field_pattern.span())
                        })?;

                    match field_pattern {
                        BindingPattern::Literal(ExpressionLiteral::Number(expected), _) => {
                            emit_expression(value_expr, locals, locals_types, func, type_ctx)?;
                            func.instruction(&Instruction::I32Const(*expected));
                            func.instruction(&Instruction::I32Eq);
                            if comparisons_emitted {
                                func.instruction(&Instruction::I32And);
                            }
                            comparisons_emitted = true;
                        }
                        BindingPattern::Identifier(identifier, _) => {
                            emit_expression(value_expr, locals, locals_types, func, type_ctx)?;
                            let local_index = locals
                                .get(&identifier.0)
                                .copied()
                                .expect("Local should have been collected");
                            func.instruction(&Instruction::LocalSet(local_index));
                        }
                        _ => {
                            return Err(
                                Diagnostic::new(
                                    "Unsupported pattern in struct binding for wasm emission",
                                )
                                .with_span(field_pattern.span()),
                            );
                        }
                    }
                }

                if !comparisons_emitted {
                    func.instruction(&Instruction::I32Const(1));
                }
                Ok(())
            } else {
                emit_expression(&binding.expr, locals, locals_types, func, type_ctx)?;
                let name = extract_identifier_from_pattern(binding.pattern.clone())?;
                let local_index = locals
                    .get(&name)
                    .copied()
                    .expect("Local should have been collected");
                func.instruction(&Instruction::LocalSet(local_index));
                Ok(())
            }
        }
        Expression::Block(exprs, _) => {
            for (i, e) in exprs.iter().enumerate() {
                emit_expression(e, locals, locals_types, func, type_ctx)?;
                if i < exprs.len() - 1 {
                    if !matches!(e, Expression::Binding(..)) {
                        func.instruction(&Instruction::Drop);
                    }
                }
            }
            Ok(())
        }
        Expression::Struct(items, _) => {
            let mut sorted_items = items.clone();
            sorted_items.sort_by(|a, b| a.0.0.cmp(&b.0.0));

            let mut field_types = Vec::new();
            for (name, value) in &sorted_items {
                let ty = infer_type(value, locals_types)?;
                field_types.push((name.0.clone(), ty));
            }

            let type_index = type_ctx.get_type_index(&field_types).ok_or_else(|| {
                Diagnostic::new("Struct type not found in context").with_span(expr.span())
            })?;

            for (_, value) in sorted_items {
                emit_expression(&value, locals, locals_types, func, type_ctx)?;
            }

            func.instruction(&Instruction::StructNew(type_index));
            Ok(())
        }
        Expression::PropertyAccess {
            object,
            property,
            span,
        } => {
            let object_type = infer_type(object, locals_types)?;
            if let WasmType::Struct(fields) = object_type {
                let field_index =
                    fields
                        .iter()
                        .position(|(n, _)| n == property)
                        .ok_or_else(|| {
                            Diagnostic::new(format!("Field `{}` not found in struct", property))
                                .with_span(*span)
                        })?;

                let type_index = type_ctx
                    .get_type_index(&fields)
                    .expect("Type should be registered");

                emit_expression(object, locals, locals_types, func, type_ctx)?;
                func.instruction(&Instruction::StructGet {
                    struct_type_index: type_index,
                    field_index: field_index as u32,
                });
                Ok(())
            } else {
                Err(Diagnostic::new("Property access on non-struct type").with_span(*span))
            }
        }
        other => Err(
            Diagnostic::new("Expression is not supported in wasm exports yet")
                .with_span(other.span()),
        ),
    }
}
