use wasm_encoder::{
    CodeSection, ExportKind, ExportSection, FieldType, Function, FunctionSection, HeapType,
    Instruction, Module, RefType, StorageType, TypeSection, ValType,
};

use crate::{
    diagnostics::{Diagnostic, SourceSpan},
    interpret::{self, AnnotatedBinding},
    parsing::{
        BinaryIntrinsicOperator, Binding, BindingAnnotation, BindingPattern, Expression,
        ExpressionLiteral, Identifier, IntrinsicOperation, IntrinsicType, LValue, TargetLiteral,
    },
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum WasmType {
    I32,
    Struct(Vec<(String, WasmType)>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ControlFrame {
    Block,
    Loop,
    If,
}

#[derive(Clone, Debug)]
struct LoopContext {
    break_target_index: usize,
    result_type: Option<WasmType>,
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

fn format_wasm_type(ty: &WasmType) -> String {
    match ty {
        WasmType::I32 => "i32".to_string(),
        WasmType::Struct(fields) => {
            let inner = fields
                .iter()
                .map(|(name, ty)| format!("{}: {}", name, format_wasm_type(ty)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("struct {{{}}}", inner)
        }
    }
}

fn default_value_for_type(ty: &Expression) -> Option<Expression> {
    match ty {
        Expression::IntrinsicType(IntrinsicType::I32, span)
        | Expression::IntrinsicType(IntrinsicType::Boolean, span) => {
            Some(Expression::Literal(ExpressionLiteral::Number(0), *span))
        }
        Expression::Struct(fields, span) => {
            let mut values = Vec::new();
            for (id, field_ty) in fields {
                values.push((id.clone(), default_value_for_type(field_ty)?));
            }
            Some(Expression::Struct(values, *span))
        }
        Expression::AttachImplementation { type_expr, .. } => default_value_for_type(type_expr),
        Expression::EnumType(variants, span) => {
            if variants.is_empty() {
                return None;
            }
            let (first_variant, first_ty) = &variants[0];
            Some(Expression::EnumValue {
                enum_type: Box::new(Expression::EnumType(variants.clone(), *span)),
                variant: first_variant.clone(),
                variant_index: 0,
                payload: Some(Box::new(default_value_for_type(first_ty)?)),
                span: *span,
            })
        }
        _ => None,
    }
}

fn materialize_enum_value(enum_value: &Expression) -> Option<Expression> {
    if let Expression::EnumValue {
        enum_type,
        variant: _variant,
        variant_index,
        payload,
        span,
    } = enum_value
        && let Expression::EnumType(variants, _) = enum_type.as_ref()
    {
        let mut payload_fields = Vec::new();
        for (idx, (name, ty)) in variants.iter().enumerate() {
            let value = if idx == *variant_index {
                match payload {
                    Some(val) => *val.clone(),
                    None => default_value_for_type(ty)?,
                }
            } else {
                default_value_for_type(ty)?
            };
            payload_fields.push((Identifier(name.0.clone()), value));
        }

        let payload_struct = Expression::Struct(payload_fields, *span);
        let tag_expr = Expression::Literal(ExpressionLiteral::Number(*variant_index as i32), *span);
        return Some(Expression::Struct(
            vec![
                (Identifier("payload".to_string()), payload_struct),
                (Identifier("tag".to_string()), tag_expr),
            ],
            *span,
        ));
    }
    None
}

fn lvalue_to_expression(target: &LValue) -> Expression {
    match target {
        LValue::Identifier(identifier, span) => Expression::Identifier(identifier.clone(), *span),
        LValue::PropertyAccess {
            object,
            property,
            span,
        } => Expression::PropertyAccess {
            object: Box::new(lvalue_to_expression(object)),
            property: property.clone(),
            span: *span,
        },
    }
}

fn ensure_lvalue_local(
    target: &LValue,
    locals_types: &std::collections::HashMap<String, WasmType>,
    span: SourceSpan,
) -> Result<(), Diagnostic> {
    match target {
        LValue::Identifier(identifier, _) => {
            if locals_types.contains_key(&identifier.0) {
                Ok(())
            } else {
                Err(Diagnostic::new(format!(
                    "Identifier `{}` is not a local variable or parameter",
                    identifier.0
                ))
                .with_span(span))
            }
        }
        LValue::PropertyAccess { object, .. } => ensure_lvalue_local(object, locals_types, span),
    }
}

fn enum_wasm_type(
    enum_type: &Expression,
    ctx: &interpret::Context,
) -> Result<WasmType, Diagnostic> {
    if let Expression::EnumType(variants, _span) = enum_type {
        let mut payload_fields = Vec::new();
        for (id, ty) in variants {
            let wasm_ty = resolve_type(ctx, ty)?;
            payload_fields.push((id.0.clone(), wasm_ty));
        }
        payload_fields.sort_by(|a, b| a.0.cmp(&b.0));
        let mut outer_fields = vec![
            (
                "payload".to_string(),
                WasmType::Struct(payload_fields.clone()),
            ),
            ("tag".to_string(), WasmType::I32),
        ];
        outer_fields.sort_by(|a, b| a.0.cmp(&b.0));
        Ok(WasmType::Struct(outer_fields))
    } else {
        Err(Diagnostic::new("Expected enum type expression").with_span(enum_type.span()))
    }
}

fn enum_variant_index_from_context(
    enum_expr: &Expression,
    variant: &Identifier,
    context: &interpret::Context,
    span: SourceSpan,
) -> Result<usize, Diagnostic> {
    let mut ctx = context.clone();
    if let Some(Expression::EnumType(variants, _)) =
        interpret::resolve_enum_type_expression(enum_expr, &mut ctx)
        && let Some((index, _)) = variants
            .iter()
            .enumerate()
            .find(|(_, (id, _))| id.0 == variant.0)
    {
        return Ok(index);
    }

    Err(Diagnostic::new("Unknown enum or variant in wasm lowering").with_span(span))
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
        collect_types(&export.body, &mut type_ctx, &mut locals_types, context)?;
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

    for (function_index, export) in exports.iter().enumerate() {
        let mut locals = Vec::new();
        let mut local_indices = std::collections::HashMap::new();
        let mut locals_types = std::collections::HashMap::new();

        // Add parameters
        for (j, param) in export.params.iter().enumerate() {
            local_indices.insert(param.name.clone(), j as u32);
            locals_types.insert(param.name.clone(), param.ty.clone());
        }

        // Collect additional locals from body
        let body_locals = collect_locals(&export.body, &mut locals_types, context)?;
        for (name, ty) in body_locals {
            if !local_indices.contains_key(&name) {
                local_indices.insert(name.clone(), (export.params.len() + locals.len()) as u32);
                locals.push(ty.clone());
            }
        }

        let mut func = Function::new(locals.iter().map(|ty| (1, ty.to_val_type(&type_ctx))));

        let mut control_stack = Vec::new();
        let mut loop_stack = Vec::new();

        emit_expression(
            &export.body,
            &local_indices,
            &locals_types,
            &mut func,
            &type_ctx,
            context,
            &mut control_stack,
            &mut loop_stack,
        )?;

        let body_produces_value = expression_produces_value(&export.body);
        let body_transfers_control = expression_contains_return(&export.body);
        if body_produces_value {
            func.instruction(&Instruction::Return);
        } else if body_transfers_control {
            func.instruction(&Instruction::Unreachable);
        }
        func.instruction(&Instruction::End);
        code_section.function(&func);

        export_section.export(&export.name, ExportKind::Func, function_index as u32);
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
    context: &interpret::Context,
) -> Result<(), Diagnostic> {
    if let Some(struct_expr) = materialize_enum_value(expr) {
        return collect_types(&struct_expr, ctx, locals_types, context);
    }
    match expr {
        Expression::Struct(fields, _) => {
            let mut field_types = Vec::new();
            for (name, value) in fields {
                collect_types(value, ctx, locals_types, context)?;
                let ty = infer_type(value, locals_types, context)?;
                field_types.push((name.0.clone(), ty));
            }
            field_types.sort_by(|a, b| a.0.cmp(&b.0));
            ctx.get_or_register_type(field_types);
        }
        Expression::Block(exprs, _) => {
            for e in exprs {
                collect_types(e, ctx, locals_types, context)?;
            }
        }
        Expression::Binding(binding, _) => {
            collect_types(&binding.expr, ctx, locals_types, context)?;
            let ty = infer_type(&binding.expr, locals_types, context)?;
            let mut locals = Vec::new();
            collect_locals_for_pattern(
                binding.pattern.clone(),
                ty,
                locals_types,
                &mut locals,
            )?;
        }
        Expression::FunctionCall {
            function, argument, ..
        } => {
            collect_types(function, ctx, locals_types, context)?;
            collect_types(argument, ctx, locals_types, context)?;
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_types(condition, ctx, locals_types, context)?;
            collect_types(then_branch, ctx, locals_types, context)?;
            if let Some(else_branch) = else_branch {
                collect_types(else_branch, ctx, locals_types, context)?;
            }
        }
        Expression::Match {
            value, branches, ..
        } => {
            collect_types(value, ctx, locals_types, context)?;
            for (_, branch) in branches {
                collect_types(branch, ctx, locals_types, context)?;
            }
        }
        Expression::IntrinsicOperation(IntrinsicOperation::Binary(left, right, _), _) => {
            collect_types(left, ctx, locals_types, context)?;
            collect_types(right, ctx, locals_types, context)?;
        }
        Expression::IntrinsicOperation(IntrinsicOperation::EnumFromStruct, span) => {
            return Err(
                Diagnostic::new("enum intrinsic should be resolved before wasm lowering")
                    .with_span(*span),
            );
        }
        Expression::Loop { body, .. } => {
            collect_types(body, ctx, locals_types, context)?;
        }
        Expression::PropertyAccess { object, .. } => {
            collect_types(object, ctx, locals_types, context)?;
        }
        Expression::EnumType(variants, _) => {
            for (_, ty) in variants {
                collect_types(ty, ctx, locals_types, context)?;
            }
            if let WasmType::Struct(fields) = enum_wasm_type(expr, context)? {
                ctx.get_or_register_type(fields);
            }
        }
        Expression::EnumConstructor {
            enum_type,
            payload_type,
            ..
        } => {
            collect_types(enum_type, ctx, locals_types, context)?;
            collect_types(payload_type, ctx, locals_types, context)?;
        }
        Expression::EnumAccess { enum_expr, .. } => {
            collect_types(enum_expr, ctx, locals_types, context)?;
        }
        _ => {}
    }
    Ok(())
}

fn expression_contains_return(expr: &Expression) -> bool {
    match expr {
        Expression::Return { .. } | Expression::Break { .. } => true,
        Expression::Block(exprs, _) => exprs.iter().any(expression_contains_return),
        Expression::If {
            then_branch,
            else_branch,
            ..
        } => {
            expression_contains_return(then_branch)
                || else_branch
                    .as_ref()
                    .map(|branch| expression_contains_return(branch))
                    .unwrap_or(false)
        }
        Expression::Binding(binding, _) => expression_contains_return(&binding.expr),
        Expression::Assignment { expr, .. } => expression_contains_return(expr),
        Expression::FunctionCall {
            function, argument, ..
        } => expression_contains_return(function) || expression_contains_return(argument),
        Expression::Loop { body, .. } => {
            loop_contains_break(body) || expression_contains_return(body)
        }
        Expression::PropertyAccess { object, .. } => expression_contains_return(object),
        Expression::IntrinsicOperation(IntrinsicOperation::Binary(left, right, _), _span) => {
            expression_contains_return(left) || expression_contains_return(right)
        }
        Expression::EnumAccess { enum_expr, .. } => expression_contains_return(enum_expr),
        Expression::EnumConstructor {
            enum_type,
            payload_type,
            ..
        } => expression_contains_return(enum_type) || expression_contains_return(payload_type),
        Expression::AttachImplementation {
            type_expr,
            implementation,
            ..
        } => expression_contains_return(type_expr) || expression_contains_return(implementation),
        _ => false,
    }
}

fn loop_contains_break(expr: &Expression) -> bool {
    match expr {
        Expression::Break { .. } => true,
        Expression::Block(exprs, _) => exprs.iter().any(loop_contains_break),
        Expression::If {
            then_branch,
            else_branch,
            ..
        } => {
            loop_contains_break(then_branch)
                || else_branch
                    .as_ref()
                    .map(|branch| loop_contains_break(branch))
                    .unwrap_or(false)
        }
        Expression::Binding(binding, _) => loop_contains_break(&binding.expr),
        Expression::Assignment { expr, .. } => loop_contains_break(expr),
        Expression::FunctionCall {
            function, argument, ..
        } => loop_contains_break(function) || loop_contains_break(argument),
        Expression::Loop { .. } => false,
        Expression::PropertyAccess { object, .. } => loop_contains_break(object),
        Expression::IntrinsicOperation(IntrinsicOperation::Binary(left, right, _), _span) => {
            loop_contains_break(left) || loop_contains_break(right)
        }
        Expression::EnumAccess { enum_expr, .. } => loop_contains_break(enum_expr),
        Expression::EnumConstructor {
            enum_type,
            payload_type,
            ..
        } => loop_contains_break(enum_type) || loop_contains_break(payload_type),
        Expression::AttachImplementation {
            type_expr,
            implementation,
            ..
        } => loop_contains_break(type_expr) || loop_contains_break(implementation),
        Expression::EnumValue {
            enum_type, payload, ..
        } => {
            loop_contains_break(enum_type)
                || payload
                    .as_ref()
                    .map(|payload| loop_contains_break(payload))
                    .unwrap_or(false)
        }
        _ => false,
    }
}

fn collect_break_types(
    expr: &Expression,
    locals_types: &std::collections::HashMap<String, WasmType>,
    context: &interpret::Context,
    types: &mut Vec<WasmType>,
) -> Result<(), Diagnostic> {
    match expr {
        Expression::Break { value, span } => {
            let inner_expr = value
                .as_ref()
                .map(|expr| expr.as_ref().clone())
                .unwrap_or_else(|| Expression::Struct(vec![], *span));
            let ty = infer_type(&inner_expr, locals_types, context)?;
            types.push(ty);
        }
        Expression::Block(exprs, _) => {
            for e in exprs {
                collect_break_types(e, locals_types, context, types)?;
            }
        }
        Expression::If {
            then_branch,
            else_branch,
            ..
        } => {
            collect_break_types(then_branch, locals_types, context, types)?;
            if let Some(else_branch) = else_branch {
                collect_break_types(else_branch, locals_types, context, types)?;
            }
        }
        Expression::Binding(binding, _) => {
            collect_break_types(&binding.expr, locals_types, context, types)?;
        }
        Expression::Assignment { expr, .. } => {
            collect_break_types(expr, locals_types, context, types)?;
        }
        Expression::FunctionCall {
            function, argument, ..
        } => {
            collect_break_types(function, locals_types, context, types)?;
            collect_break_types(argument, locals_types, context, types)?;
        }
        Expression::PropertyAccess { object, .. } => {
            collect_break_types(object, locals_types, context, types)?;
        }
        Expression::IntrinsicOperation(IntrinsicOperation::Binary(left, right, _), _span) => {
            collect_break_types(left, locals_types, context, types)?;
            collect_break_types(right, locals_types, context, types)?;
        }
        Expression::EnumAccess { enum_expr, .. } => {
            collect_break_types(enum_expr, locals_types, context, types)?;
        }
        Expression::EnumConstructor {
            enum_type,
            payload_type,
            ..
        } => {
            collect_break_types(enum_type, locals_types, context, types)?;
            collect_break_types(payload_type, locals_types, context, types)?;
        }
        Expression::AttachImplementation {
            type_expr,
            implementation,
            ..
        } => {
            collect_break_types(type_expr, locals_types, context, types)?;
            collect_break_types(implementation, locals_types, context, types)?;
        }
        // Do not descend into nested loops because their breaks should not affect the
        // surrounding loop's result type.
        Expression::Loop { .. } => {}
        _ => {}
    }

    Ok(())
}

fn determine_loop_result_type(
    body: &Expression,
    locals_types: &std::collections::HashMap<String, WasmType>,
    context: &interpret::Context,
) -> Result<Option<WasmType>, Diagnostic> {
    let mut break_types = Vec::new();
    collect_break_types(body, locals_types, context, &mut break_types)?;

    if let Some(first_type) = break_types.first() {
        let mut mismatched = None;
        for ty in &break_types {
            if ty != first_type {
                mismatched = Some(ty.clone());
                break;
            }
        }

        if let Some(other) = mismatched {
            return Err(Diagnostic::new(format!(
                "Inconsistent break value types in loop: `{}` vs `{}`",
                format_wasm_type(first_type),
                format_wasm_type(&other)
            ))
            .with_span(body.span()));
        }

        Ok(Some(first_type.clone()))
    } else {
        Ok(None)
    }
}

fn expression_produces_value(expr: &Expression) -> bool {
    match expr {
        Expression::Return { .. } | Expression::Break { .. } => false,
        Expression::Loop { body, .. } => loop_contains_break(body),
        Expression::Block(exprs, _) => exprs.last().map(expression_produces_value).unwrap_or(true),
        Expression::If {
            then_branch,
            else_branch,
            ..
        } => {
            let then_returns = expression_contains_return(then_branch);
            let else_returns = else_branch
                .as_ref()
                .map(|branch| expression_contains_return(branch))
                .unwrap_or(false);

            let then_produces = expression_produces_value(then_branch);
            let else_produces = else_branch
                .as_ref()
                .map(|branch| expression_produces_value(branch))
                .unwrap_or(true);

            (then_produces || then_returns) && (else_produces || else_returns)
        }
        Expression::Binding(binding, _) => expression_produces_value(&binding.expr),
        _ => true,
    }
}

fn expression_always_transfers_control(expr: &Expression) -> bool {
    match expr {
        Expression::Return { .. } | Expression::Break { .. } => true,
        Expression::Block(exprs, _) => exprs
            .last()
            .map(expression_always_transfers_control)
            .unwrap_or(false),
        Expression::If {
            then_branch,
            else_branch,
            ..
        } => {
            let then_exits = expression_always_transfers_control(then_branch);
            let else_exits = else_branch
                .as_ref()
                .map(|branch| expression_always_transfers_control(branch))
                .unwrap_or(false);
            then_exits && else_exits
        }
        Expression::Binding(binding, _) => expression_always_transfers_control(&binding.expr),
        _ => false,
    }
}

fn expression_emits_value(
    expr: &Expression,
    locals_types: &std::collections::HashMap<String, WasmType>,
    context: &interpret::Context,
    type_ctx: &TypeContext,
) -> Result<bool, Diagnostic> {
    match expr {
        Expression::Return { .. } | Expression::Break { .. } => Ok(false),
        Expression::If {
            then_branch,
            else_branch,
            ..
        } => {
            let result_type = infer_type(then_branch, locals_types, context)?;
            let wasm_result_type = result_type.to_val_type(type_ctx);
            let then_returns = expression_contains_return(then_branch);
            let else_returns = else_branch
                .as_ref()
                .map(|branch| expression_contains_return(branch))
                .unwrap_or(false);

            if else_branch.is_none() && (then_returns || !matches!(wasm_result_type, ValType::I32))
            {
                return Ok(false);
            }

            if let Some(else_branch) = else_branch {
                if then_returns && else_returns {
                    return Ok(false);
                }

                let then_produces = expression_produces_value(then_branch);
                let else_produces = expression_produces_value(else_branch);
                return Ok(then_produces || else_produces);
            }

            Ok(true)
        }
        Expression::Block(exprs, _) => {
            if let Some(last) = exprs.last() {
                expression_emits_value(last, locals_types, context, type_ctx)
            } else {
                Ok(false)
            }
        }
        Expression::Loop { body, .. } => {
            Ok(determine_loop_result_type(body, locals_types, context)?.is_some())
        }
        _ => Ok(true),
    }
}

fn infer_type(
    expr: &Expression,
    locals_types: &std::collections::HashMap<String, WasmType>,
    context: &interpret::Context,
) -> Result<WasmType, Diagnostic> {
    if let Some(struct_expr) = materialize_enum_value(expr) {
        return infer_type(&struct_expr, locals_types, context);
    }
    match expr {
        Expression::Literal(ExpressionLiteral::Number(_), _) => Ok(WasmType::I32),
        Expression::Literal(ExpressionLiteral::Boolean(_), _) => Ok(WasmType::I32),
        Expression::Struct(fields, _) => {
            let mut field_types = Vec::new();
            for (name, value) in fields {
                let ty = infer_type(value, locals_types, context)?;
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
        Expression::Assignment { target, expr, span } => {
            let value_type = infer_type(expr, locals_types, context)?;
            let target_expr = lvalue_to_expression(target);
            let existing_type = infer_type(&target_expr, locals_types, context)?;

            if value_type != existing_type {
                return Err(Diagnostic::new(
                    "Cannot assign value of different type to target".to_string(),
                )
                .with_span(*span));
            }

            Ok(value_type)
        }
        Expression::Return { value, span } => {
            let inner_expr = value
                .as_ref()
                .map(|expr| expr.as_ref().clone())
                .unwrap_or_else(|| Expression::Struct(vec![], *span));
            infer_type(&inner_expr, locals_types, context)
        }
        Expression::Break { value, span } => {
            let inner_expr = value
                .as_ref()
                .map(|expr| expr.as_ref().clone())
                .unwrap_or_else(|| Expression::Struct(vec![], *span));
            infer_type(&inner_expr, locals_types, context)
        }
        Expression::Loop { body, .. } => {
            if let Some(result_type) = determine_loop_result_type(body, locals_types, context)? {
                return Ok(result_type);
            }

            infer_type(body, locals_types, context)
        }
        Expression::FunctionCall { .. } => Ok(WasmType::I32),
        Expression::PropertyAccess {
            object,
            property,
            span,
        } => {
            let object_type = infer_type(object, locals_types, context)?;
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
        Expression::If { then_branch, .. } => infer_type(then_branch, locals_types, context),
        Expression::Block(exprs, _) => {
            if let Some(last) = exprs.last() {
                infer_type(last, locals_types, context)
            } else {
                Ok(WasmType::I32)
            }
        }
        Expression::Binding(..) => Ok(WasmType::I32),
        Expression::Match { branches, .. } => {
            if let Some((_, branch)) = branches.first() {
                infer_type(branch, locals_types, context)
            } else {
                Ok(WasmType::I32) // todo: should be never type
            }
        }
        Expression::EnumType(_, _) => enum_wasm_type(expr, context),
        Expression::EnumConstructor { enum_type, .. }
        | Expression::EnumAccess {
            enum_expr: enum_type,
            ..
        } => enum_wasm_type(enum_type, context),
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

    let return_type = resolve_type(context, &return_type.unwrap())?;
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
                    interpret::BindingContext::Bound(value, _) => resolve_type(context, value),
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
        Expression::EnumType(_, _) => enum_wasm_type(expr, context),
        _ => Err(Diagnostic::new("Unsupported type for WASM").with_span(expr.span())),
    }
}

fn wasm_annotation_span(annotations: &[BindingAnnotation]) -> Option<SourceSpan> {
    annotations.iter().find_map(|annotation| match annotation {
        BindingAnnotation::Export(
            Expression::Literal(ExpressionLiteral::Target(TargetLiteral::WasmTarget), _),
            span,
        ) => Some(*span),
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
        BindingPattern::EnumVariant { .. } => Err(Diagnostic::new(
            "Enum patterns are not supported in exported parameters",
        )
        .with_span(pattern.span())),
        BindingPattern::Literal(_, _) => Err(Diagnostic::new(
            "Literal patterns cannot be used in function parameters",
        )
        .with_span(pattern.span())),
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
        BindingPattern::EnumVariant {
            payload: Some(payload),
            ..
        } => extract_identifier_from_pattern(*payload),
        BindingPattern::EnumVariant { .. } => Err(Diagnostic::new(
            "Cannot extract identifier from enum pattern",
        )
        .with_span(pattern.span())),
        BindingPattern::Literal(_, _) => Err(Diagnostic::new(
            "Literal patterns cannot be used in function parameters",
        )
        .with_span(pattern.span())),
    }
}

fn unwrap_pattern(mut pattern: BindingPattern) -> BindingPattern {
    loop {
        pattern = match pattern {
            BindingPattern::Annotated { pattern, .. } => *pattern,
            BindingPattern::TypeHint(inner, _, _) => *inner,
            other => return other,
        };
    }
}

fn collect_locals(
    expr: &Expression,
    locals_types: &mut std::collections::HashMap<String, WasmType>,
    context: &interpret::Context,
) -> Result<Vec<(String, WasmType)>, Diagnostic> {
    let mut locals = Vec::new();
    match expr {
        Expression::Binding(binding, _) => {
            let expr_type = infer_type(&binding.expr, locals_types, context)?;
            collect_locals_for_pattern(
                binding.pattern.clone(),
                expr_type,
                locals_types,
                &mut locals,
            )?;

            locals.extend(collect_locals(&binding.expr, locals_types, context)?);
        }
        Expression::Assignment { target, expr, span } => {
            ensure_lvalue_local(target, locals_types, *span)?;
            locals.extend(collect_locals(expr, locals_types, context)?);
        }
        Expression::Block(exprs, _) => {
            for e in exprs {
                locals.extend(collect_locals(e, locals_types, context)?);
            }
        }
        Expression::Function { body: _body, .. } => {}
        Expression::IntrinsicOperation(IntrinsicOperation::Binary(left, right, _), _) => {
            locals.extend(collect_locals(left, locals_types, context)?);
            locals.extend(collect_locals(right, locals_types, context)?);
        }
        Expression::FunctionCall {
            function, argument, ..
        } => {
            locals.extend(collect_locals(function, locals_types, context)?);
            locals.extend(collect_locals(argument, locals_types, context)?);
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            locals.extend(collect_locals(condition, locals_types, context)?);
            locals.extend(collect_locals(then_branch, locals_types, context)?);
            if let Some(else_branch) = else_branch {
                locals.extend(collect_locals(else_branch, locals_types, context)?);
            }
        }
        Expression::Match {
            value,
            branches,
            span,
        } => {
            let value_type = infer_type(value, locals_types, context)?;
            locals.push((format!("__match_val_{}", span.start()), value_type));
            locals.extend(collect_locals(value, locals_types, context)?);
            for (pattern, branch) in branches {
                let value_type = infer_type(value, locals_types, context)?;
                collect_locals_for_pattern(
                    pattern.clone(),
                    value_type,
                    locals_types,
                    &mut locals,
                )?;
                locals.extend(collect_locals(branch, locals_types, context)?);
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
                return Err(
                    Diagnostic::new("Struct pattern requires struct value type").with_span(span)
                );
            };

            for (field_identifier, field_pattern) in items {
                let field_type = field_types
                    .iter()
                    .find(|(name, _)| name == &field_identifier.0)
                    .map(|(_, ty)| ty.clone())
                    .ok_or_else(|| {
                        Diagnostic::new(format!(
                            "Missing field {} in struct type",
                            field_identifier.0
                        ))
                        .with_span(field_pattern.span())
                    })?;
                collect_locals_for_pattern(
                    field_pattern,
                    field_type,
                    locals_types,
                    locals,
                )?;
            }
            Ok(())
        }
        BindingPattern::TypeHint(inner, _, _) => {
            collect_locals_for_pattern(*inner, expr_type, locals_types, locals)
        }
        BindingPattern::Annotated { pattern, .. } => {
            collect_locals_for_pattern(*pattern, expr_type, locals_types, locals)
        }
        BindingPattern::EnumVariant {
            variant,
            payload,
            span,
            ..
        } => {
            let payload_type = match expr_type {
                WasmType::Struct(ref fields) => {
                    let payload_struct = fields
                        .iter()
                        .find(|(name, _)| name == "payload")
                        .map(|(_, ty)| ty)
                        .ok_or_else(|| {
                            Diagnostic::new("Enum payload field missing in type context")
                                .with_span(span)
                        })?;
                    let WasmType::Struct(payload_fields) = payload_struct else {
                        return Err(Diagnostic::new(
                            "Enum payload is not a struct in wasm lowering",
                        )
                        .with_span(span));
                    };
                    payload_fields
                        .iter()
                        .find(|(name, _)| name == &variant.0)
                        .map(|(_, ty)| ty.clone())
                        .ok_or_else(|| {
                            Diagnostic::new("Enum variant missing in payload struct")
                                .with_span(span)
                        })?
                }
                _ => {
                    return Err(
                        Diagnostic::new("Enum pattern requires struct-backed enum type")
                            .with_span(span),
                    );
                }
            };

            if let Some(payload_pattern) = payload {
                collect_locals_for_pattern(
                    *payload_pattern,
                    payload_type,
                    locals_types,
                    locals,
                )?;
            }
            Ok(())
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
    context: &interpret::Context,
    control_stack: &mut Vec<ControlFrame>,
    loop_stack: &mut Vec<LoopContext>,
) -> Result<(), Diagnostic> {
    if let Some(struct_expr) = materialize_enum_value(expr) {
        return emit_expression(
            &struct_expr,
            locals,
            locals_types,
            func,
            type_ctx,
            context,
            control_stack,
            loop_stack,
        );
    }
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
        Expression::Assignment { target, expr, span } => match target {
            LValue::Identifier(identifier, _) => {
                let local_index = locals.get(&identifier.0).copied().ok_or_else(|| {
                    Diagnostic::new(format!(
                        "Identifier `{}` is not a local variable or parameter",
                        identifier.0
                    ))
                    .with_span(*span)
                })?;

                emit_expression(
                    expr,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    context,
                    control_stack,
                    loop_stack,
                )?;
                func.instruction(&Instruction::LocalTee(local_index));
                Ok(())
            }
            LValue::PropertyAccess {
                object,
                property,
                span: target_span,
            } => {
                let object_expr = lvalue_to_expression(object);
                let object_type = infer_type(&object_expr, locals_types, context)?;
                let WasmType::Struct(fields) = object_type else {
                    return Err(Diagnostic::new("Property assignment on non-struct type")
                        .with_span(*target_span));
                };

                let field_index = fields
                    .iter()
                    .position(|(name, _)| name == property)
                    .ok_or_else(|| {
                        Diagnostic::new(format!("Field `{}` not found in struct", property))
                            .with_span(*target_span)
                    })? as u32;

                let type_index = type_ctx
                    .get_type_index(&fields)
                    .expect("Type should be registered");

                emit_expression(
                    &object_expr,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    context,
                    control_stack,
                    loop_stack,
                )?;
                emit_expression(
                    expr,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    context,
                    control_stack,
                    loop_stack,
                )?;
                func.instruction(&Instruction::StructSet {
                    struct_type_index: type_index,
                    field_index,
                });

                let full_target_expr = lvalue_to_expression(target);
                emit_expression(
                    &full_target_expr,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    context,
                    control_stack,
                    loop_stack,
                )
            }
        },
        Expression::IntrinsicOperation(IntrinsicOperation::Binary(left, right, op), _) => {
            match op {
                BinaryIntrinsicOperator::BooleanAnd => {
                    emit_expression(
                        left,
                        locals,
                        locals_types,
                        func,
                        type_ctx,
                        context,
                        control_stack,
                        loop_stack,
                    )?;
                    control_stack.push(ControlFrame::If);
                    func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
                        ValType::I32,
                    )));
                    emit_expression(
                        right,
                        locals,
                        locals_types,
                        func,
                        type_ctx,
                        context,
                        control_stack,
                        loop_stack,
                    )?;
                    func.instruction(&Instruction::Else);
                    func.instruction(&Instruction::I32Const(0));
                    func.instruction(&Instruction::End);
                    control_stack.pop();
                }
                BinaryIntrinsicOperator::BooleanOr => {
                    emit_expression(
                        left,
                        locals,
                        locals_types,
                        func,
                        type_ctx,
                        context,
                        control_stack,
                        loop_stack,
                    )?;
                    control_stack.push(ControlFrame::If);
                    func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
                        ValType::I32,
                    )));
                    func.instruction(&Instruction::I32Const(1));
                    func.instruction(&Instruction::Else);
                    emit_expression(
                        right,
                        locals,
                        locals_types,
                        func,
                        type_ctx,
                        context,
                        control_stack,
                        loop_stack,
                    )?;
                    func.instruction(&Instruction::End);
                    control_stack.pop();
                }
                _ => {
                    emit_expression(
                        left,
                        locals,
                        locals_types,
                        func,
                        type_ctx,
                        context,
                        control_stack,
                        loop_stack,
                    )?;
                    emit_expression(
                        right,
                        locals,
                        locals_types,
                        func,
                        type_ctx,
                        context,
                        control_stack,
                        loop_stack,
                    )?;
                    match op {
                        BinaryIntrinsicOperator::I32Add => {
                            func.instruction(&Instruction::I32Add);
                        }
                        BinaryIntrinsicOperator::I32Subtract => {
                            func.instruction(&Instruction::I32Sub);
                        }
                        BinaryIntrinsicOperator::I32Multiply => {
                            func.instruction(&Instruction::I32Mul);
                        }
                        BinaryIntrinsicOperator::I32Divide => {
                            func.instruction(&Instruction::I32DivS);
                        }
                        BinaryIntrinsicOperator::I32Equal => {
                            func.instruction(&Instruction::I32Eq);
                        }
                        BinaryIntrinsicOperator::I32NotEqual => {
                            func.instruction(&Instruction::I32Ne);
                        }
                        BinaryIntrinsicOperator::I32LessThan => {
                            func.instruction(&Instruction::I32LtS);
                        }
                        BinaryIntrinsicOperator::I32GreaterThan => {
                            func.instruction(&Instruction::I32GtS);
                        }
                        BinaryIntrinsicOperator::I32LessThanOrEqual => {
                            func.instruction(&Instruction::I32LeS);
                        }
                        BinaryIntrinsicOperator::I32GreaterThanOrEqual => {
                            func.instruction(&Instruction::I32GeS);
                        }
                        BinaryIntrinsicOperator::BooleanXor => {
                            func.instruction(&Instruction::I32Xor);
                        }
                        _ => unreachable!(),
                    }
                }
            }
            Ok(())
        }
        Expression::IntrinsicOperation(IntrinsicOperation::EnumFromStruct, span) => Err(
            Diagnostic::new("enum intrinsic should be resolved before wasm lowering")
                .with_span(*span),
        ),
        Expression::Break { value, span } => {
            let loop_ctx = loop_stack.last().cloned().ok_or_else(|| {
                Diagnostic::new("`break` used outside of a loop").with_span(*span)
            })?;

            let break_value = value
                .as_ref()
                .map(|expr| expr.as_ref().clone())
                .unwrap_or_else(|| Expression::Struct(vec![], *span));

            let expected_type = loop_ctx.result_type.as_ref().ok_or_else(|| {
                Diagnostic::new("`break` cannot be used in a loop without a break value")
                    .with_span(*span)
            })?;

            let value_type = infer_type(&break_value, locals_types, context)?;
            if &value_type != expected_type {
                return Err(
                    Diagnostic::new("break value does not match loop result type").with_span(*span),
                );
            }

            emit_expression(
                &break_value,
                locals,
                locals_types,
                func,
                type_ctx,
                context,
                control_stack,
                loop_stack,
            )?;

            let break_depth = control_stack
                .len()
                .saturating_sub(loop_ctx.break_target_index + 1)
                as u32;
            func.instruction(&Instruction::Br(break_depth));
            Ok(())
        }
        Expression::Loop { body, .. } => {
            let loop_result_type = determine_loop_result_type(body, locals_types, context)?;
            if let Some(result_type) = loop_result_type {
                let block_type = wasm_encoder::BlockType::Result(result_type.to_val_type(type_ctx));

                control_stack.push(ControlFrame::Block);
                func.instruction(&Instruction::Block(block_type));

                control_stack.push(ControlFrame::Loop);
                loop_stack.push(LoopContext {
                    break_target_index: control_stack.len() - 2,
                    result_type: Some(result_type.clone()),
                });
                func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));

                emit_expression(
                    body,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    context,
                    control_stack,
                    loop_stack,
                )?;
                if expression_emits_value(body, locals_types, context, type_ctx)? {
                    func.instruction(&Instruction::Drop);
                }
                func.instruction(&Instruction::Br(0));

                control_stack.pop();
                func.instruction(&Instruction::End);

                loop_stack.pop();
                control_stack.pop();
                func.instruction(&Instruction::Unreachable);
                func.instruction(&Instruction::End);
            } else {
                control_stack.push(ControlFrame::Loop);
                func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
                emit_expression(
                    body,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    context,
                    control_stack,
                    loop_stack,
                )?;
                if expression_emits_value(body, locals_types, context, type_ctx)? {
                    func.instruction(&Instruction::Drop);
                }
                func.instruction(&Instruction::Br(0));
                control_stack.pop();
                func.instruction(&Instruction::End);
            }
            Ok(())
        }
        Expression::FunctionCall {
            function,
            argument,
            span,
        } => {
            if let Expression::EnumConstructor {
                enum_type,
                variant,
                variant_index,
                ..
            } = function.as_ref()
            {
                let enum_value = Expression::EnumValue {
                    enum_type: enum_type.clone(),
                    variant: variant.clone(),
                    variant_index: *variant_index,
                    payload: Some(argument.clone()),
                    span: *span,
                };
                return emit_expression(
                    &enum_value,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    context,
                    control_stack,
                    loop_stack,
                );
            }

            Err(
                Diagnostic::new("Function calls are not supported in wasm exports yet")
                    .with_span(*span),
            )
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            emit_expression(
                condition,
                locals,
                locals_types,
                func,
                type_ctx,
                context,
                control_stack,
                loop_stack,
            )?;
            let result_type = infer_type(then_branch, locals_types, context)?;
            let wasm_result_type = result_type.to_val_type(type_ctx);
            let then_returns = expression_contains_return(then_branch);
            let else_returns = else_branch
                .as_ref()
                .map(|branch| expression_contains_return(branch))
                .unwrap_or(false);

            if else_branch.is_none() && (then_returns || !matches!(wasm_result_type, ValType::I32))
            {
                control_stack.push(ControlFrame::If);
                func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                emit_expression(
                    then_branch,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    context,
                    control_stack,
                    loop_stack,
                )?;
                func.instruction(&Instruction::End);
                control_stack.pop();
            } else {
                let block_type = if then_returns && else_returns {
                    wasm_encoder::BlockType::Empty
                } else {
                    wasm_encoder::BlockType::Result(wasm_result_type)
                };
                control_stack.push(ControlFrame::If);
                func.instruction(&Instruction::If(block_type));
                emit_expression(
                    then_branch,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    context,
                    control_stack,
                    loop_stack,
                )?;
                func.instruction(&Instruction::Else);
                if let Some(else_expr) = else_branch {
                    emit_expression(
                        else_expr,
                        locals,
                        locals_types,
                        func,
                        type_ctx,
                        context,
                        control_stack,
                        loop_stack,
                    )?;
                } else {
                    func.instruction(&Instruction::I32Const(0));
                }
                func.instruction(&Instruction::End);
                control_stack.pop();
            }
            Ok(())
        }
        Expression::Binding(binding, _) => {
            let pattern = unwrap_pattern(binding.pattern.clone());
            if let BindingPattern::EnumVariant {
                enum_type,
                variant,
                payload,
                span: pattern_span,
            } = &pattern
            {
                let value_type = infer_type(&binding.expr, locals_types, context)?;
                let WasmType::Struct(fields) = value_type else {
                    return Err(
                        Diagnostic::new("Enum bindings require struct-backed enum value")
                            .with_span(*pattern_span),
                    );
                };

                let tag_field_index = fields
                    .iter()
                    .position(|(name, _)| name == "tag")
                    .ok_or_else(|| {
                        Diagnostic::new("Enum tag missing in wasm lowering")
                            .with_span(*pattern_span)
                    })? as u32;

                let type_index = type_ctx.get_type_index(&fields).ok_or_else(|| {
                    Diagnostic::new("Enum type not registered").with_span(*pattern_span)
                })?;

                let payload_struct = fields
                    .iter()
                    .find(|(name, _)| name == "payload")
                    .map(|(_, ty)| ty)
                    .expect("Payload field should exist");
                let WasmType::Struct(_payload_fields) = payload_struct else {
                    return Err(Diagnostic::new("Enum payload expected to be a struct")
                        .with_span(*pattern_span));
                };

                let variant_index = enum_variant_index_from_context(
                    enum_type.as_ref(),
                    variant,
                    context,
                    *pattern_span,
                )? as i32;

                emit_expression(
                    &binding.expr,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    context,
                    control_stack,
                    loop_stack,
                )?;
                func.instruction(&Instruction::StructGet {
                    struct_type_index: type_index,
                    field_index: tag_field_index,
                });
                func.instruction(&Instruction::I32Const(variant_index));
                func.instruction(&Instruction::I32Eq);

                // If tag matches (1), we proceed to check payload.
                // We use `if (result i32)` block.
                control_stack.push(ControlFrame::If);
                func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
                    ValType::I32,
                )));

                if let Some(payload_pattern) = payload.clone() {
                    // Extract payload
                    // We construct a property access expression for the payload
                    let payload_access_expr = Expression::PropertyAccess {
                        object: Box::new(Expression::PropertyAccess {
                            object: Box::new(binding.expr.clone()),
                            property: "payload".to_string(),
                            span: *pattern_span,
                        }),
                        property: variant.0.clone(),
                        span: *pattern_span,
                    };

                    let payload_binding = Expression::Binding(
                        Box::new(Binding {
                            pattern: *payload_pattern,
                            expr: payload_access_expr,
                        }),
                        *pattern_span,
                    );

                    emit_expression(
                        &payload_binding,
                        locals,
                        locals_types,
                        func,
                        type_ctx,
                        context,
                        control_stack,
                        loop_stack,
                    )?;
                    // The binding expression returns 1 (i32) on success.
                    // We drop it and push 1 to be explicit and safe.
                    func.instruction(&Instruction::Drop);
                    func.instruction(&Instruction::I32Const(1));
                } else {
                    // No payload to check, so if tag matched, we are good.
                    func.instruction(&Instruction::I32Const(1));
                }

                func.instruction(&Instruction::Else);
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::End);
                control_stack.pop();

                return Ok(());
            }

            if let (
                BindingPattern::Struct(pattern_fields, _),
                Expression::Struct(value_fields, _),
            ) = (&pattern, &binding.expr)
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

                    match unwrap_pattern(field_pattern.clone()) {
                        BindingPattern::Literal(ExpressionLiteral::Number(expected), _) => {
                            emit_expression(
                                value_expr,
                                locals,
                                locals_types,
                                func,
                                type_ctx,
                                context,
                                control_stack,
                                loop_stack,
                            )?;
                            func.instruction(&Instruction::I32Const(expected));
                            func.instruction(&Instruction::I32Eq);
                            if comparisons_emitted {
                                func.instruction(&Instruction::I32And);
                            }
                            comparisons_emitted = true;
                        }
                        BindingPattern::Identifier(identifier, _) => {
                            emit_expression(
                                value_expr,
                                locals,
                                locals_types,
                                func,
                                type_ctx,
                                context,
                                control_stack,
                                loop_stack,
                            )?;
                            let local_index = locals
                                .get(&identifier.0)
                                .copied()
                                .expect("Local should have been collected");
                            func.instruction(&Instruction::LocalSet(local_index));
                        }
                        _ => {
                            return Err(Diagnostic::new(
                                "Unsupported pattern in struct binding for wasm emission",
                            )
                            .with_span(field_pattern.span()));
                        }
                    }
                }

                if !comparisons_emitted {
                    func.instruction(&Instruction::I32Const(1));
                }
                Ok(())
            } else if let BindingPattern::Literal(literal, _) = &pattern {
                emit_expression(
                    &binding.expr,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    context,
                    control_stack,
                    loop_stack,
                )?;
                match literal {
                    ExpressionLiteral::Number(val) => {
                        func.instruction(&Instruction::I32Const(*val));
                        func.instruction(&Instruction::I32Eq);
                    }
                    ExpressionLiteral::Boolean(val) => {
                        func.instruction(&Instruction::I32Const(if *val { 1 } else { 0 }));
                        func.instruction(&Instruction::I32Eq);
                    }
                    _ => {
                        return Err(Diagnostic::new(
                            "Unsupported literal pattern in binding for wasm emission",
                        )
                        .with_span(pattern.span()));
                    }
                }
                Ok(())
            } else {
                emit_expression(
                    &binding.expr,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    context,
                    control_stack,
                    loop_stack,
                )?;
                let name = extract_identifier_from_pattern(pattern)?;
                let local_index = locals
                    .get(&name)
                    .copied()
                    .expect("Local should have been collected");
                func.instruction(&Instruction::LocalSet(local_index));
                // Binding to an identifier always succeeds
                func.instruction(&Instruction::I32Const(1));
                Ok(())
            }
        }
        Expression::EnumAccess {
            enum_expr,
            variant,
            span,
        } => {
            if let Expression::EnumType(variants, _) = enum_expr.as_ref()
                && let Some((variant_index, (_, payload_type))) = variants
                    .iter()
                    .enumerate()
                    .find(|(_, (id, _))| id.0 == variant.0)
            {
                if let Expression::Struct(fields, _) = payload_type
                    && fields.is_empty()
                {
                    let value = Expression::EnumValue {
                        enum_type: enum_expr.clone(),
                        variant: variant.clone(),
                        variant_index,
                        payload: None,
                        span: *span,
                    };
                    return emit_expression(
                        &value,
                        locals,
                        locals_types,
                        func,
                        type_ctx,
                        context,
                        control_stack,
                        loop_stack,
                    );
                }

                let constructor = Expression::EnumConstructor {
                    enum_type: enum_expr.clone(),
                    variant: variant.clone(),
                    variant_index,
                    payload_type: Box::new(payload_type.clone()),
                    span: *span,
                };
                return emit_expression(
                    &constructor,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    context,
                    control_stack,
                    loop_stack,
                );
            }

            Err(
                Diagnostic::new("Enum access could not be resolved in wasm export")
                    .with_span(*span),
            )
        }
        Expression::Block(exprs, _) => {
            for (i, e) in exprs.iter().enumerate() {
                emit_expression(
                    e,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    context,
                    control_stack,
                    loop_stack,
                )?;
                if i < exprs.len() - 1 {
                    if expression_always_transfers_control(e) {
                        break;
                    }

                    if expression_emits_value(e, locals_types, context, type_ctx)? {
                        func.instruction(&Instruction::Drop);
                    }
                }
            }
            Ok(())
        }
        Expression::Return { value, .. } => {
            if value.is_none() {
                eprintln!("return without value");
            }
            let inner_expr = value
                .as_ref()
                .map(|expr| expr.as_ref().clone())
                .unwrap_or_else(|| Expression::Struct(vec![], expr.span()));
            emit_expression(
                &inner_expr,
                locals,
                locals_types,
                func,
                type_ctx,
                context,
                control_stack,
                loop_stack,
            )?;
            func.instruction(&Instruction::Return);
            Ok(())
        }
        Expression::Struct(items, _span) => {
            let mut sorted_items = items.clone();
            sorted_items.sort_by(|a, b| a.0.0.cmp(&b.0.0));

            let mut field_types = Vec::new();
            for (name, value) in &sorted_items {
                let ty = infer_type(value, locals_types, context)?;
                field_types.push((name.0.clone(), ty));
            }

            let type_index = type_ctx.get_type_index(&field_types).ok_or_else(|| {
                Diagnostic::new("Struct type not found in context").with_span(expr.span())
            })?;

            for (_, value) in sorted_items {
                emit_expression(
                    &value,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    context,
                    control_stack,
                    loop_stack,
                )?;
            }

            func.instruction(&Instruction::StructNew(type_index));
            Ok(())
        }
        Expression::PropertyAccess {
            object,
            property,
            span,
        } => {
            let object_type = infer_type(object, locals_types, context)?;
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

                emit_expression(
                    object,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    context,
                    control_stack,
                    loop_stack,
                )?;
                func.instruction(&Instruction::StructGet {
                    struct_type_index: type_index,
                    field_index: field_index as u32,
                });
                Ok(())
            } else {
                Err(Diagnostic::new("Property access on non-struct type").with_span(*span))
            }
        }
        Expression::Match {
            value,
            branches,
            span,
        } => {
            emit_expression(
                value,
                locals,
                locals_types,
                func,
                type_ctx,
                context,
                control_stack,
                loop_stack,
            )?;

            let temp_local_name = format!("__match_val_{}", span.start());
            let temp_local_index = locals
                .get(&temp_local_name)
                .copied()
                .expect("Temp local should exist");
            func.instruction(&Instruction::LocalSet(temp_local_index));

            let result_type = infer_type(
                branches
                    .first()
                    .map(|(_, b)| b)
                    .expect("Match must have branches"),
                locals_types,
                context,
            )?;
            let wasm_result_type = result_type.to_val_type(type_ctx);
            let block_type = wasm_encoder::BlockType::Result(wasm_result_type);

            control_stack.push(ControlFrame::Block);
            func.instruction(&Instruction::Block(block_type));

            for (pattern, branch) in branches {
                let check_expr = Expression::Binding(
                    Box::new(Binding {
                        pattern: pattern.clone(),
                        expr: Expression::Identifier(Identifier(temp_local_name.clone()), *span),
                    }),
                    *span,
                );

                emit_expression(
                    &check_expr,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    context,
                    control_stack,
                    loop_stack,
                )?;

                func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

                emit_expression(
                    branch,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    context,
                    control_stack,
                    loop_stack,
                )?;

                func.instruction(&Instruction::Br(1));
                func.instruction(&Instruction::End);
            }

            func.instruction(&Instruction::Unreachable);

            func.instruction(&Instruction::End);
            control_stack.pop();

            Ok(())
        }
        other => Err(
            Diagnostic::new("Expression is not supported in wasm exports yet")
                .with_span(other.span()),
        ),
    }
}
