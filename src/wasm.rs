use wasm_encoder::{
    CodeSection, ConstExpr, ExportKind, ExportSection, FieldType, Function, FunctionSection,
    GlobalSection, GlobalType, HeapType, Instruction, Module, RefType, StorageType, TypeSection,
    ValType,
};

use crate::{
    diagnostics::{Diagnostic, SourceSpan},
    intermediate::{
        IntermediateBinding, IntermediateBindingPattern, IntermediateExportType, IntermediateKind,
        IntermediateIntrinsicOperation, IntermediateResult, IntermediateType,
    },
    parsing::{
        BinaryIntrinsicOperator, DivergeExpressionType, ExpressionLiteral, Identifier, LValue,
        TargetLiteral, UnaryIntrinsicOperator,
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

#[derive(Default)]
struct MatchCounter {
    next_id: usize,
}

impl MatchCounter {
    fn next_name(&mut self) -> String {
        let name = format!("__match_val_{}", self.next_id);
        self.next_id += 1;
        name
    }
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

fn intermediate_type_to_wasm(ty: &IntermediateType) -> WasmType {
    match ty {
        IntermediateType::I32 => WasmType::I32,
        IntermediateType::Struct(fields) => {
            let mut wasm_fields = fields
                .iter()
                .map(|(name, ty)| (name.clone(), intermediate_type_to_wasm(ty)))
                .collect::<Vec<_>>();
            wasm_fields.sort_by(|a, b| a.0.cmp(&b.0));
            WasmType::Struct(wasm_fields)
        }
    }
}

fn lvalue_to_intermediate(target: &LValue) -> IntermediateKind {
    match target {
        LValue::Identifier(identifier, _) => IntermediateKind::Identifier(identifier.clone()),
        LValue::PropertyAccess {
            object, property, ..
        } => IntermediateKind::PropertyAccess {
            object: Box::new(lvalue_to_intermediate(object)),
            property: property.clone(),
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
            if locals_types.contains_key(&identifier.name) {
                Ok(())
            } else {
                Err(Diagnostic::new(format!(
                    "Identifier `{}` is not a local variable or parameter",
                    identifier.name
                ))
                .with_span(span))
            }
        }
        LValue::PropertyAccess { object, .. } => ensure_lvalue_local(object, locals_types, span),
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
    body: IntermediateKind,
    return_type: WasmType,
}

struct WasmGlobalExport {
    name: String,
    ty: WasmType,
    init: IntermediateKind,
}

struct WasmExports {
    functions: Vec<WasmFunctionExport>,
    globals: Vec<WasmGlobalExport>,
}

pub fn compile_exports(intermediate: &IntermediateResult) -> Result<Vec<u8>, Diagnostic> {
    let exports = collect_wasm_exports(intermediate)?;
    if exports.functions.is_empty() && exports.globals.is_empty() {
        return Ok(Vec::new());
    }

    let mut module = Module::new();
    let mut type_section = TypeSection::new();
    let mut function_section = FunctionSection::new();
    let mut global_section = GlobalSection::new();
    let mut export_section = ExportSection::new();
    let mut code_section = CodeSection::new();

    let mut type_ctx = TypeContext::new();

    let function_return_types = intermediate
        .functions
        .iter()
        .map(|function| intermediate_type_to_wasm(&function.return_type))
        .collect::<Vec<_>>();

    for export in &exports.functions {
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
        collect_types(
            &export.body,
            &mut type_ctx,
            &mut locals_types,
            &function_return_types,
        )?;
    }

    for global in &exports.globals {
        if let WasmType::Struct(fields) = &global.ty {
            type_ctx.get_or_register_type(fields.clone());
        }
    }

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

    let struct_type_count = type_ctx.struct_types.len() as u32;
    let mut next_type_index = struct_type_count;

    for export in &exports.functions {
        let param_types: Vec<ValType> = export
            .params
            .iter()
            .map(|param| param.ty.to_val_type(&type_ctx))
            .collect();
        let return_types = vec![export.return_type.to_val_type(&type_ctx)];

        type_section.function(param_types, return_types);
        function_section.function(next_type_index);
        next_type_index += 1;
    }

    for global in &exports.globals {
        let init_expr = const_expr_for_global(&global.init, &global.ty)?;
        global_section.global(
            GlobalType {
                val_type: global.ty.to_val_type(&type_ctx),
                mutable: false,
                shared: false,
            },
            &init_expr,
        );
    }

    for (function_index, export) in exports.functions.iter().enumerate() {
        let mut locals = Vec::new();
        let mut local_indices = std::collections::HashMap::new();
        let mut locals_types = std::collections::HashMap::new();

        for (j, param) in export.params.iter().enumerate() {
            local_indices.insert(param.name.clone(), j as u32);
            locals_types.insert(param.name.clone(), param.ty.clone());
        }

        let mut match_counter = MatchCounter::default();
        let body_locals = collect_locals(
            &export.body,
            &mut locals_types,
            &function_return_types,
            &mut match_counter,
        )?;
        for (name, ty) in body_locals {
            if !local_indices.contains_key(&name) {
                local_indices.insert(name.clone(), (export.params.len() + locals.len()) as u32);
                locals.push(ty.clone());
            }
        }

        let mut func = Function::new(locals.iter().map(|ty| (1, ty.to_val_type(&type_ctx))));

        let mut control_stack = Vec::new();
        let mut loop_stack = Vec::new();

        let mut match_counter = MatchCounter::default();
        emit_expression(
            &export.body,
            &local_indices,
            &locals_types,
            &mut func,
            &type_ctx,
            &function_return_types,
            &mut control_stack,
            &mut loop_stack,
            &mut match_counter,
        )?;

        let body_produces_value = expression_produces_value(
            &export.body,
            &locals_types,
            &function_return_types,
            &type_ctx,
        )?;
        let body_transfers_control = expression_does_diverge(&export.body, false, false);
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
    if !exports.globals.is_empty() {
        module.section(&global_section);
    }
    for (global_index, global) in exports.globals.iter().enumerate() {
        export_section.export(&global.name, ExportKind::Global, global_index as u32);
    }
    module.section(&export_section);
    module.section(&code_section);

    Ok(module.finish())
}

fn collect_wasm_exports(intermediate: &IntermediateResult) -> Result<WasmExports, Diagnostic> {
    let mut functions = Vec::new();
    let mut globals = Vec::new();

    for export in &intermediate.exports {
        if export.target != TargetLiteral::WasmTarget {
            continue;
        }
        match export.export_type {
            IntermediateExportType::Function => {
                let function = intermediate.functions.get(export.index).ok_or_else(|| {
                    Diagnostic::new(format!(
                        "Export `{}` references unknown function index {}",
                        export.name, export.index
                    ))
                    .with_span(SourceSpan::default())
                })?;

                let return_type = intermediate_type_to_wasm(&function.return_type);
                let params = extract_function_params(&function.parameter, &function.input_type)?;

                functions.push(WasmFunctionExport {
                    name: export.name.clone(),
                    params,
                    body: (*function.body).clone(),
                    return_type,
                });
            }
            IntermediateExportType::Global => {
                let global = intermediate.globals.get(export.index).ok_or_else(|| {
                    Diagnostic::new(format!(
                        "Export `{}` references unknown global index {}",
                        export.name, export.index
                    ))
                    .with_span(SourceSpan::default())
                })?;
                globals.push(WasmGlobalExport {
                    name: global.name.clone(),
                    ty: intermediate_type_to_wasm(&global.ty),
                    init: global.value.clone(),
                });
            }
        }
    }

    functions.sort_by(|a, b| a.name.cmp(&b.name));
    globals.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(WasmExports { functions, globals })
}

fn const_expr_for_global(expr: &IntermediateKind, ty: &WasmType) -> Result<ConstExpr, Diagnostic> {
    match (ty, expr) {
        (WasmType::I32, IntermediateKind::Literal(ExpressionLiteral::Number(value))) => {
            Ok(ConstExpr::i32_const(*value))
        }
        (WasmType::I32, IntermediateKind::Literal(ExpressionLiteral::Boolean(value))) => {
            Ok(ConstExpr::i32_const(if *value { 1 } else { 0 }))
        }
        _ => Err(Diagnostic::new(
            "Unsupported global initializer for wasm export".to_string(),
        )
        .with_span(SourceSpan::default())),
    }
}

fn collect_types(
    expr: &IntermediateKind,
    ctx: &mut TypeContext,
    locals_types: &mut std::collections::HashMap<String, WasmType>,
    function_return_types: &[WasmType],
) -> Result<(), Diagnostic> {
    match expr {
        IntermediateKind::Struct(fields) => {
            let mut field_types = Vec::new();
            for (name, value) in fields {
                collect_types(value, ctx, locals_types, function_return_types)?;
                let ty = infer_type(value, locals_types, function_return_types)?;
                field_types.push((name.name.clone(), ty));
            }
            field_types.sort_by(|a, b| a.0.cmp(&b.0));
            ctx.get_or_register_type(field_types);
        }
        IntermediateKind::Block(exprs) => {
            for e in exprs {
                collect_types(e, ctx, locals_types, function_return_types)?;
            }
        }
        IntermediateKind::Binding(binding) => {
            collect_types(&binding.expr, ctx, locals_types, function_return_types)?;
            let ty = infer_type(&binding.expr, locals_types, function_return_types)?;
            let mut locals = Vec::new();
            collect_locals_for_pattern(binding.pattern.clone(), ty, locals_types, &mut locals)?;
        }
        IntermediateKind::FunctionCall { argument, .. } => {
            collect_types(argument, ctx, locals_types, function_return_types)?;
        }
        IntermediateKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_types(condition, ctx, locals_types, function_return_types)?;
            collect_types(then_branch, ctx, locals_types, function_return_types)?;
            collect_types(else_branch, ctx, locals_types, function_return_types)?;
        }
        IntermediateKind::Match { value, branches } => {
            collect_types(value, ctx, locals_types, function_return_types)?;
            for (_, branch) in branches {
                collect_types(branch, ctx, locals_types, function_return_types)?;
            }
        }
        IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Binary(
            left,
            right,
            _,
        )) => {
            collect_types(left, ctx, locals_types, function_return_types)?;
            collect_types(right, ctx, locals_types, function_return_types)?;
        }
        IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Unary(operand, _)) => {
            collect_types(operand, ctx, locals_types, function_return_types)?;
        }
        IntermediateKind::Loop { body } => {
            collect_types(body, ctx, locals_types, function_return_types)?;
        }
        IntermediateKind::PropertyAccess { object, .. } => {
            collect_types(object, ctx, locals_types, function_return_types)?;
        }
        _ => {}
    }
    Ok(())
}

fn determine_loop_result_type(
    body: &IntermediateKind,
    locals_types: &std::collections::HashMap<String, WasmType>,
    function_return_types: &[WasmType],
) -> Result<Option<WasmType>, Diagnostic> {
    let break_values = collect_break_values(body);
    let mut break_types = Vec::new();

    for val in break_values {
        let ty = infer_type(&val, locals_types, function_return_types)?;
        break_types.push(ty);
    }

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
            .with_span(SourceSpan::default()));
        }

        Ok(Some(first_type.clone()))
    } else {
        Ok(None)
    }
}

fn expression_produces_value(
    expr: &IntermediateKind,
    locals_types: &std::collections::HashMap<String, WasmType>,
    function_return_types: &[WasmType],
    type_ctx: &TypeContext,
) -> Result<bool, Diagnostic> {
    match expr {
        IntermediateKind::Diverge { .. } => Ok(false),
        IntermediateKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            let then_produces_value =
                expression_produces_value(then_branch, locals_types, function_return_types, type_ctx)?;
            let else_produces_value =
                expression_produces_value(else_branch, locals_types, function_return_types, type_ctx)?;
            let then_diverges = expression_does_diverge(then_branch, false, false);
            let else_diverges = expression_does_diverge(else_branch, false, false);
            Ok(then_produces_value && else_produces_value
                || ((then_diverges || else_diverges)
                    && (then_produces_value || else_produces_value)))
        }
        IntermediateKind::Block(exprs) => {
            let Some(last) = exprs.last() else {
                panic!("Empty block shouldn't exist")
            };
            let diverges = exprs
                .iter()
                .any(|e| expression_does_diverge(e, false, false));
            Ok(!diverges
                && expression_produces_value(last, locals_types, function_return_types, type_ctx)?)
        }
        IntermediateKind::Loop { body } => Ok(
            determine_loop_result_type(body, locals_types, function_return_types)?.is_some(),
        ),
        _ => Ok(true),
    }
}

fn infer_type(
    expr: &IntermediateKind,
    locals_types: &std::collections::HashMap<String, WasmType>,
    function_return_types: &[WasmType],
) -> Result<WasmType, Diagnostic> {
    match expr {
        IntermediateKind::Literal(ExpressionLiteral::Number(_)) => Ok(WasmType::I32),
        IntermediateKind::Literal(ExpressionLiteral::Boolean(_)) => Ok(WasmType::I32),
        IntermediateKind::Struct(fields) => {
            let mut field_types = Vec::new();
            for (name, value) in fields {
                let ty = infer_type(value, locals_types, function_return_types)?;
                field_types.push((name.name.clone(), ty));
            }
            field_types.sort_by(|a, b| a.0.cmp(&b.0));
            Ok(WasmType::Struct(field_types))
        }
        IntermediateKind::Identifier(identifier) => {
            locals_types.get(&identifier.name).cloned().ok_or_else(|| {
                Diagnostic::new(format!("Unknown identifier `{}`", identifier.name))
                    .with_span(SourceSpan::default())
            })
        }
        IntermediateKind::Assignment { target, expr: rhs } => {
            let value_type = infer_type(rhs, locals_types, function_return_types)?;
            let target_expr = lvalue_to_intermediate(target);
            let existing_type = infer_type(&target_expr, locals_types, function_return_types)?;

            if value_type != existing_type {
                return Err(Diagnostic::new(
                    "Cannot assign value of different type to target".to_string(),
                )
                .with_span(SourceSpan::default()));
            }

            Ok(value_type)
        }
        IntermediateKind::Diverge { value, .. } => {
            infer_type(value, locals_types, function_return_types)
        }
        IntermediateKind::Loop { body } => {
            if let Some(result_type) =
                determine_loop_result_type(body, locals_types, function_return_types)?
            {
                return Ok(result_type);
            }

            infer_type(body, locals_types, function_return_types)
        }
        IntermediateKind::FunctionCall { function, .. } => function_return_types
            .get(*function)
            .cloned()
            .ok_or_else(|| {
                Diagnostic::new("Unknown function call target".to_string())
                    .with_span(SourceSpan::default())
            }),
        IntermediateKind::PropertyAccess { object, property } => {
            let object_type = infer_type(object, locals_types, function_return_types)?;
            if let WasmType::Struct(fields) = object_type {
                if let Some((_, field_type)) = fields.iter().find(|(n, _)| n == property) {
                    Ok(field_type.clone())
                } else {
                    Err(Diagnostic::new(format!(
                        "Field `{}` not found in struct",
                        property
                    ))
                    .with_span(SourceSpan::default()))
                }
            } else {
                Err(Diagnostic::new("Property access on non-struct type").with_span(
                    SourceSpan::default(),
                ))
            }
        }
        IntermediateKind::IntrinsicOperation(..) => Ok(WasmType::I32),
        IntermediateKind::If { then_branch, .. } => {
            infer_type(then_branch, locals_types, function_return_types)
        }
        IntermediateKind::Block(exprs) => {
            if let Some(last) = exprs.last() {
                infer_type(last, locals_types, function_return_types)
            } else {
                Ok(WasmType::I32)
            }
        }
        IntermediateKind::Binding(..) => Ok(WasmType::I32),
        IntermediateKind::Match { branches, .. } => {
            if let Some((_, branch)) = branches.first() {
                infer_type(branch, locals_types, function_return_types)
            } else {
                Ok(WasmType::I32)
            }
        }
        _ => Ok(WasmType::I32),
    }
}

fn extract_function_params(
    pattern: &IntermediateBindingPattern,
    ty: &IntermediateType,
) -> Result<Vec<WasmFunctionParam>, Diagnostic> {
    match pattern {
        IntermediateBindingPattern::Struct(fields, _) => {
            let IntermediateType::Struct(field_types) = ty else {
                return Err(Diagnostic::new(
                    "Struct pattern requires struct parameter type",
                )
                .with_span(pattern_span(pattern)));
            };
            let mut params = Vec::new();
            for (field_id, sub_pattern) in fields {
                let field_ty = field_types
                    .iter()
                    .find(|(name, _)| name == &field_id.name)
                    .map(|(_, ty)| ty)
                    .ok_or_else(|| {
                        Diagnostic::new(format!(
                            "Missing field {} in struct parameter type",
                            field_id.name
                        ))
                        .with_span(pattern_span(sub_pattern))
                    })?;
                let sub_params = extract_function_params(sub_pattern, field_ty)?;
                params.extend(sub_params);
            }
            Ok(params)
        }
        IntermediateBindingPattern::Identifier(identifier, _) => {
            Ok(vec![WasmFunctionParam {
                name: identifier.name.clone(),
                ty: intermediate_type_to_wasm(ty),
            }])
        }
        IntermediateBindingPattern::Literal(_, _) => Err(Diagnostic::new(
            "Literal patterns cannot be used in function parameters",
        )
        .with_span(pattern_span(pattern))),
        IntermediateBindingPattern::EnumVariant { .. } => Err(Diagnostic::new(
            "Enum patterns are not supported in exported parameters",
        )
        .with_span(pattern_span(pattern))),
    }
}

fn extract_identifier_from_pattern(
    pattern: &IntermediateBindingPattern,
) -> Result<String, Diagnostic> {
    match pattern {
        IntermediateBindingPattern::Identifier(identifier, _) => Ok(identifier.name.clone()),
        IntermediateBindingPattern::Struct(items, _) => {
            for (_, pat) in items {
                if let Ok(name) = extract_identifier_from_pattern(pat) {
                    return Ok(name);
                }
            }
            Err(Diagnostic::new("Struct patterns require at least one identifier")
                .with_span(pattern_span(pattern)))
        }
        IntermediateBindingPattern::EnumVariant {
            payload: Some(payload),
            ..
        } => extract_identifier_from_pattern(payload),
        IntermediateBindingPattern::EnumVariant { .. } => Err(Diagnostic::new(
            "Cannot extract identifier from enum pattern",
        )
        .with_span(pattern_span(pattern))),
        IntermediateBindingPattern::Literal(_, _) => Err(Diagnostic::new(
            "Literal patterns cannot be used in function parameters",
        )
        .with_span(pattern_span(pattern))),
    }
}

fn pattern_span(pattern: &IntermediateBindingPattern) -> SourceSpan {
    match pattern {
        IntermediateBindingPattern::Identifier(_, span)
        | IntermediateBindingPattern::Literal(_, span)
        | IntermediateBindingPattern::Struct(_, span)
        | IntermediateBindingPattern::EnumVariant { span, .. } => *span,
    }
}

fn collect_locals(
    expr: &IntermediateKind,
    locals_types: &mut std::collections::HashMap<String, WasmType>,
    function_return_types: &[WasmType],
    match_counter: &mut MatchCounter,
) -> Result<Vec<(String, WasmType)>, Diagnostic> {
    let mut locals = Vec::new();
    match expr {
        IntermediateKind::Binding(binding) => {
            let expr_type = infer_type(&binding.expr, locals_types, function_return_types)?;
            collect_locals_for_pattern(
                binding.pattern.clone(),
                expr_type,
                locals_types,
                &mut locals,
            )?;

            locals.extend(collect_locals(
                &binding.expr,
                locals_types,
                function_return_types,
                match_counter,
            )?);
        }
        IntermediateKind::Assignment { target, expr } => {
            ensure_lvalue_local(target, locals_types, SourceSpan::default())?;
            locals.extend(collect_locals(
                expr,
                locals_types,
                function_return_types,
                match_counter,
            )?);
        }
        IntermediateKind::Block(exprs) => {
            for e in exprs {
                locals.extend(collect_locals(
                    e,
                    locals_types,
                    function_return_types,
                    match_counter,
                )?);
            }
        }
        IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Binary(
            left,
            right,
            _,
        )) => {
            locals.extend(collect_locals(
                left,
                locals_types,
                function_return_types,
                match_counter,
            )?);
            locals.extend(collect_locals(
                right,
                locals_types,
                function_return_types,
                match_counter,
            )?);
        }
        IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Unary(
            operand,
            _,
        )) => {
            locals.extend(collect_locals(
                operand,
                locals_types,
                function_return_types,
                match_counter,
            )?);
        }
        IntermediateKind::FunctionCall { argument, .. } => {
            locals.extend(collect_locals(
                argument,
                locals_types,
                function_return_types,
                match_counter,
            )?);
        }
        IntermediateKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            locals.extend(collect_locals(
                condition,
                locals_types,
                function_return_types,
                match_counter,
            )?);
            locals.extend(collect_locals(
                then_branch,
                locals_types,
                function_return_types,
                match_counter,
            )?);
            locals.extend(collect_locals(
                else_branch,
                locals_types,
                function_return_types,
                match_counter,
            )?);
        }
        IntermediateKind::Match { value, branches } => {
            let value_type = infer_type(value, locals_types, function_return_types)?;
            let temp_local_name = match_counter.next_name();
            locals.push((temp_local_name.clone(), value_type.clone()));
            locals_types.insert(temp_local_name, value_type);
            locals.extend(collect_locals(
                value,
                locals_types,
                function_return_types,
                match_counter,
            )?);
            for (pattern, branch) in branches {
                let value_type = infer_type(value, locals_types, function_return_types)?;
                collect_locals_for_pattern(
                    pattern.clone(),
                    value_type,
                    locals_types,
                    &mut locals,
                )?;
                locals.extend(collect_locals(
                    branch,
                    locals_types,
                    function_return_types,
                    match_counter,
                )?);
            }
        }
        _ => {}
    }
    Ok(locals)
}

fn collect_locals_for_pattern(
    pattern: IntermediateBindingPattern,
    expr_type: WasmType,
    locals_types: &mut std::collections::HashMap<String, WasmType>,
    locals: &mut Vec<(String, WasmType)>,
) -> Result<(), Diagnostic> {
    match pattern {
        IntermediateBindingPattern::Identifier(identifier, _) => {
            locals.push((identifier.name.clone(), expr_type.clone()));
            locals_types.insert(identifier.name, expr_type);
            Ok(())
        }
        IntermediateBindingPattern::Struct(items, span) => {
            let WasmType::Struct(field_types) = expr_type else {
                return Err(
                    Diagnostic::new("Struct pattern requires struct value type").with_span(span),
                );
            };

            for (field_identifier, field_pattern) in items {
                let field_type = field_types
                    .iter()
                    .find(|(name, _)| name == &field_identifier.name)
                    .map(|(_, ty)| ty.clone())
                    .ok_or_else(|| {
                        Diagnostic::new(format!(
                            "Missing field {} in struct type",
                            field_identifier.name
                        ))
                        .with_span(pattern_span(&field_pattern))
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
        IntermediateBindingPattern::EnumVariant {
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
                        .find(|(name, _)| name == &variant.name)
                        .map(|(_, ty)| ty.clone())
                        .ok_or_else(|| {
                            Diagnostic::new("Enum variant missing in payload struct")
                                .with_span(span)
                        })?
                }
                _ => {
                    return Err(Diagnostic::new(
                        "Enum pattern requires struct-backed enum value",
                    )
                    .with_span(span));
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
        IntermediateBindingPattern::Literal(_, _) => Ok(()),
    }
}

fn emit_expression(
    expr: &IntermediateKind,
    locals: &std::collections::HashMap<String, u32>,
    locals_types: &std::collections::HashMap<String, WasmType>,
    func: &mut Function,
    type_ctx: &TypeContext,
    function_return_types: &[WasmType],
    control_stack: &mut Vec<ControlFrame>,
    loop_stack: &mut Vec<LoopContext>,
    match_counter: &mut MatchCounter,
) -> Result<(), Diagnostic> {
    match expr {
        IntermediateKind::Literal(ExpressionLiteral::Number(value)) => {
            func.instruction(&Instruction::I32Const(*value));
            Ok(())
        }
        IntermediateKind::Literal(ExpressionLiteral::Boolean(value)) => {
            func.instruction(&Instruction::I32Const(if *value { 1 } else { 0 }));
            Ok(())
        }
        IntermediateKind::Identifier(identifier) => {
            let local_index = locals.get(&identifier.name).copied().ok_or_else(|| {
                Diagnostic::new(format!(
                    "Identifier `{}` is not a local variable or parameter",
                    identifier.name
                ))
                .with_span(SourceSpan::default())
            })?;
            func.instruction(&Instruction::LocalGet(local_index));
            Ok(())
        }
        IntermediateKind::Assignment { target, expr: value } => match target {
            LValue::Identifier(identifier, _) => {
                let local_index = locals.get(&identifier.name).copied().ok_or_else(|| {
                    Diagnostic::new(format!(
                        "Identifier `{}` is not a local variable or parameter",
                        identifier.name
                    ))
                    .with_span(SourceSpan::default())
                })?;

                emit_expression(
                    value,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    function_return_types,
                    control_stack,
                    loop_stack,
                    match_counter,
                )?;
                func.instruction(&Instruction::LocalTee(local_index));
                Ok(())
            }
            LValue::PropertyAccess {
                object,
                property,
                ..
            } => {
                let object_expr = lvalue_to_intermediate(object);
                let object_type = infer_type(&object_expr, locals_types, function_return_types)?;
                let WasmType::Struct(fields) = object_type else {
                    return Err(Diagnostic::new("Property assignment on non-struct type")
                        .with_span(SourceSpan::default()));
                };

                let field_index = fields
                    .iter()
                    .position(|(name, _)| name == property)
                    .ok_or_else(|| {
                        Diagnostic::new(format!(
                            "Field `{}` not found in struct",
                            property
                        ))
                        .with_span(SourceSpan::default())
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
                    function_return_types,
                    control_stack,
                    loop_stack,
                    match_counter,
                )?;
                emit_expression(
                    value,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    function_return_types,
                    control_stack,
                    loop_stack,
                    match_counter,
                )?;
                func.instruction(&Instruction::StructSet {
                    struct_type_index: type_index,
                    field_index,
                });

                let full_target_expr = lvalue_to_intermediate(target);
                emit_expression(
                    &full_target_expr,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    function_return_types,
                    control_stack,
                    loop_stack,
                    match_counter,
                )
            }
        },
        IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Binary(
            left,
            right,
            op,
        )) => {
            match op {
                BinaryIntrinsicOperator::BooleanAnd => {
                    emit_expression(
                        left,
                        locals,
                        locals_types,
                        func,
                        type_ctx,
                        function_return_types,
                        control_stack,
                        loop_stack,
                        match_counter,
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
                        function_return_types,
                        control_stack,
                        loop_stack,
                        match_counter,
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
                        function_return_types,
                        control_stack,
                        loop_stack,
                        match_counter,
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
                        function_return_types,
                        control_stack,
                        loop_stack,
                        match_counter,
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
                        function_return_types,
                        control_stack,
                        loop_stack,
                        match_counter,
                    )?;
                    emit_expression(
                        right,
                        locals,
                        locals_types,
                        func,
                        type_ctx,
                        function_return_types,
                        control_stack,
                        loop_stack,
                        match_counter,
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
        IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Unary(
            _,
            UnaryIntrinsicOperator::EnumFromStruct,
        )) => Err(
            Diagnostic::new("enum intrinsic should be resolved before wasm lowering")
                .with_span(SourceSpan::default()),
        ),
        IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Unary(
            operand,
            UnaryIntrinsicOperator::BooleanNot,
        )) => {
            emit_expression(
                operand,
                locals,
                locals_types,
                func,
                type_ctx,
                function_return_types,
                control_stack,
                loop_stack,
                match_counter,
            )?;
            func.instruction(&Instruction::I32Eqz);
            Ok(())
        }
        IntermediateKind::Diverge {
            value,
            divergance_type: DivergeExpressionType::Break,
        } => {
            let loop_ctx = loop_stack
                .last()
                .cloned()
                .ok_or_else(|| {
                    Diagnostic::new("`break` used outside of a loop")
                        .with_span(SourceSpan::default())
                })?;

            let expected_type = loop_ctx.result_type.as_ref().ok_or_else(|| {
                Diagnostic::new("`break` cannot be used in a loop without a break value")
                    .with_span(SourceSpan::default())
            })?;

            let value_type = infer_type(value, locals_types, function_return_types)?;
            if &value_type != expected_type {
                return Err(Diagnostic::new("break value does not match loop result type")
                    .with_span(SourceSpan::default()));
            }

            emit_expression(
                value,
                locals,
                locals_types,
                func,
                type_ctx,
                function_return_types,
                control_stack,
                loop_stack,
                match_counter,
            )?;

            let break_depth = control_stack
                .len()
                .saturating_sub(loop_ctx.break_target_index + 1)
                as u32;
            func.instruction(&Instruction::Br(break_depth));
            Ok(())
        }
        IntermediateKind::Loop { body } => {
            let loop_result_type =
                determine_loop_result_type(body, locals_types, function_return_types)?;
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
                    function_return_types,
                    control_stack,
                    loop_stack,
                    match_counter,
                )?;
                if expression_produces_value(
                    body,
                    locals_types,
                    function_return_types,
                    type_ctx,
                )? {
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
                    function_return_types,
                    control_stack,
                    loop_stack,
                    match_counter,
                )?;
                if expression_produces_value(
                    body,
                    locals_types,
                    function_return_types,
                    type_ctx,
                )? {
                    func.instruction(&Instruction::Drop);
                }
                func.instruction(&Instruction::Br(0));
                control_stack.pop();
                func.instruction(&Instruction::End);
            }
            Ok(())
        }
        IntermediateKind::FunctionCall { .. } => Err(Diagnostic::new(
            "Function calls are not supported in wasm exports yet",
        )
        .with_span(SourceSpan::default())),
        IntermediateKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            emit_expression(
                condition,
                locals,
                locals_types,
                func,
                type_ctx,
                function_return_types,
                control_stack,
                loop_stack,
                match_counter,
            )?;
            let then_produces_value = expression_produces_value(
                then_branch,
                locals_types,
                function_return_types,
                type_ctx,
            )?;
            let else_produces_value = expression_produces_value(
                else_branch,
                locals_types,
                function_return_types,
                type_ctx,
            )?;

            let then_diverges = expression_does_diverge(then_branch, false, false);
            let else_diverges = expression_does_diverge(else_branch, false, false);

            control_stack.push(ControlFrame::If);
            let block_type = if (then_produces_value && else_produces_value)
                || ((then_diverges || else_diverges)
                    && (then_produces_value || else_produces_value))
            {
                let result_type = if then_produces_value {
                    infer_type(then_branch, locals_types, function_return_types)?
                } else {
                    infer_type(else_branch, locals_types, function_return_types)?
                };
                let wasm_result_type = result_type.to_val_type(type_ctx);
                wasm_encoder::BlockType::Result(wasm_result_type)
            } else {
                wasm_encoder::BlockType::Empty
            };
            func.instruction(&Instruction::If(block_type));
            emit_expression(
                then_branch,
                locals,
                locals_types,
                func,
                type_ctx,
                function_return_types,
                control_stack,
                loop_stack,
                match_counter,
            )?;
            if then_produces_value && matches!(block_type, wasm_encoder::BlockType::Empty) {
                func.instruction(&Instruction::Drop);
            }
            func.instruction(&Instruction::Else);
            emit_expression(
                else_branch,
                locals,
                locals_types,
                func,
                type_ctx,
                function_return_types,
                control_stack,
                loop_stack,
                match_counter,
            )?;
            if else_produces_value && matches!(block_type, wasm_encoder::BlockType::Empty) {
                func.instruction(&Instruction::Drop);
            }
            func.instruction(&Instruction::End);
            control_stack.pop();
            Ok(())
        }
        IntermediateKind::Binding(binding) => {
            let pattern = &binding.pattern;
            if let IntermediateBindingPattern::EnumVariant {
                variant,
                variant_index,
                payload,
                span,
            } = pattern
            {
                let value_type =
                    infer_type(&binding.expr, locals_types, function_return_types)?;
                let WasmType::Struct(fields) = value_type else {
                    return Err(Diagnostic::new(
                        "Enum bindings require struct-backed enum value",
                    )
                    .with_span(*span));
                };

                let tag_field_index = fields
                    .iter()
                    .position(|(name, _)| name == "tag")
                    .ok_or_else(|| {
                        Diagnostic::new("Enum tag missing in wasm lowering").with_span(*span)
                    })? as u32;

                let type_index = type_ctx.get_type_index(&fields).ok_or_else(|| {
                    Diagnostic::new("Enum type not registered").with_span(*span)
                })?;

                let payload_struct = fields
                    .iter()
                    .find(|(name, _)| name == "payload")
                    .map(|(_, ty)| ty)
                    .expect("Payload field should exist");
                let WasmType::Struct(_payload_fields) = payload_struct else {
                    return Err(Diagnostic::new("Enum payload expected to be a struct")
                        .with_span(*span));
                };

                emit_expression(
                    &binding.expr,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    function_return_types,
                    control_stack,
                    loop_stack,
                    match_counter,
                )?;
                func.instruction(&Instruction::StructGet {
                    struct_type_index: type_index,
                    field_index: tag_field_index,
                });
                func.instruction(&Instruction::I32Const(*variant_index as i32));
                func.instruction(&Instruction::I32Eq);

                control_stack.push(ControlFrame::If);
                func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
                    ValType::I32,
                )));

                if let Some(payload_pattern) = payload.clone() {
                    let payload_access_expr = IntermediateKind::PropertyAccess {
                        object: Box::new(IntermediateKind::PropertyAccess {
                            object: Box::new(binding.expr.clone()),
                            property: "payload".to_string(),
                        }),
                        property: variant.name.clone(),
                    };

                    let payload_binding = IntermediateKind::Binding(Box::new(IntermediateBinding {
                        pattern: *payload_pattern,
                        binding_type: binding.binding_type.clone(),
                        expr: payload_access_expr,
                    }));

                    emit_expression(
                        &payload_binding,
                        locals,
                        locals_types,
                        func,
                        type_ctx,
                        function_return_types,
                        control_stack,
                        loop_stack,
                        match_counter,
                    )?;
                    func.instruction(&Instruction::Drop);
                    func.instruction(&Instruction::I32Const(1));
                } else {
                    func.instruction(&Instruction::I32Const(1));
                }

                func.instruction(&Instruction::Else);
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::End);
                control_stack.pop();

                return Ok(());
            }

            if let (
                IntermediateBindingPattern::Struct(pattern_fields, _),
                IntermediateKind::Struct(value_fields),
            ) = (pattern, &binding.expr)
            {
                let mut comparisons_emitted = false;
                for (field_identifier, field_pattern) in pattern_fields {
                    let value_expr = value_fields
                        .iter()
                        .find(|(id, _)| id.name == field_identifier.name)
                        .map(|(_, expr)| expr)
                        .ok_or_else(|| {
                            Diagnostic::new(format!(
                                "Missing field {} in struct binding",
                                field_identifier.name
                            ))
                            .with_span(pattern_span(field_pattern))
                        })?;

                    match field_pattern {
                        IntermediateBindingPattern::Literal(
                            ExpressionLiteral::Number(expected),
                            _,
                        ) => {
                            emit_expression(
                                value_expr,
                                locals,
                                locals_types,
                                func,
                                type_ctx,
                                function_return_types,
                                control_stack,
                                loop_stack,
                                match_counter,
                            )?;
                            func.instruction(&Instruction::I32Const(*expected));
                            func.instruction(&Instruction::I32Eq);
                            if comparisons_emitted {
                                func.instruction(&Instruction::I32And);
                            }
                            comparisons_emitted = true;
                        }
                        IntermediateBindingPattern::Identifier(identifier, _) => {
                            emit_expression(
                                value_expr,
                                locals,
                                locals_types,
                                func,
                                type_ctx,
                                function_return_types,
                                control_stack,
                                loop_stack,
                                match_counter,
                            )?;
                            let local_index = locals
                                .get(&identifier.name)
                                .copied()
                                .expect("Local should have been collected");
                            func.instruction(&Instruction::LocalSet(local_index));
                        }
                        _ => {
                            return Err(Diagnostic::new(
                                "Unsupported pattern in struct binding for wasm emission",
                            )
                            .with_span(pattern_span(field_pattern)));
                        }
                    }
                }

                if !comparisons_emitted {
                    func.instruction(&Instruction::I32Const(1));
                }
                Ok(())
            } else if let IntermediateBindingPattern::Literal(literal, span) = pattern {
                emit_expression(
                    &binding.expr,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    function_return_types,
                    control_stack,
                    loop_stack,
                    match_counter,
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
                        .with_span(*span));
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
                    function_return_types,
                    control_stack,
                    loop_stack,
                    match_counter,
                )?;
                let name = extract_identifier_from_pattern(pattern)?;
                let local_index = locals
                    .get(&name)
                    .copied()
                    .unwrap_or_else(|| panic!("Local '{}' should have been collected", name));
                func.instruction(&Instruction::LocalSet(local_index));
                func.instruction(&Instruction::I32Const(1));
                Ok(())
            }
        }
        IntermediateKind::Block(exprs) => {
            for (i, e) in exprs.iter().enumerate() {
                emit_expression(
                    e,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    function_return_types,
                    control_stack,
                    loop_stack,
                    match_counter,
                )?;
                if i < exprs.len() - 1 {
                    if expression_does_diverge(e, false, false) {
                        break;
                    }

                    if expression_produces_value(
                        e,
                        locals_types,
                        function_return_types,
                        type_ctx,
                    )? {
                        func.instruction(&Instruction::Drop);
                    }
                }
            }
            Ok(())
        }
        IntermediateKind::Diverge {
            value,
            divergance_type: DivergeExpressionType::Return,
        } => {
            emit_expression(
                value,
                locals,
                locals_types,
                func,
                type_ctx,
                function_return_types,
                control_stack,
                loop_stack,
                match_counter,
            )?;
            func.instruction(&Instruction::Return);
            Ok(())
        }
        IntermediateKind::Struct(items) => {
            let mut sorted_items = items.clone();
            sorted_items.sort_by(|(a_name, _), (b_name, _)| a_name.name.cmp(&b_name.name));

            let mut field_types = Vec::new();
            for (name, value) in &sorted_items {
                let ty = infer_type(value, locals_types, function_return_types)?;
                field_types.push((name.name.clone(), ty));
            }

            let type_index = type_ctx.get_type_index(&field_types).ok_or_else(|| {
                Diagnostic::new("Struct type not found in context")
                    .with_span(SourceSpan::default())
            })?;

            for (_, value) in sorted_items {
                emit_expression(
                    &value,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    function_return_types,
                    control_stack,
                    loop_stack,
                    match_counter,
                )?;
            }

            func.instruction(&Instruction::StructNew(type_index));
            Ok(())
        }
        IntermediateKind::PropertyAccess { object, property } => {
            let object_type = infer_type(object, locals_types, function_return_types)?;
            if let WasmType::Struct(fields) = object_type {
                let field_index =
                    fields
                        .iter()
                        .position(|(n, _)| n == property)
                        .ok_or_else(|| {
                            Diagnostic::new(format!(
                                "Field `{}` not found in struct",
                                property
                            ))
                            .with_span(SourceSpan::default())
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
                    function_return_types,
                    control_stack,
                    loop_stack,
                    match_counter,
                )?;
                func.instruction(&Instruction::StructGet {
                    struct_type_index: type_index,
                    field_index: field_index as u32,
                });
                Ok(())
            } else {
                Err(Diagnostic::new("Property access on non-struct type")
                    .with_span(SourceSpan::default()))
            }
        }
        IntermediateKind::Match { value, branches } => {
            emit_expression(
                value,
                locals,
                locals_types,
                func,
                type_ctx,
                function_return_types,
                control_stack,
                loop_stack,
                match_counter,
            )?;

            let temp_local_name = match_counter.next_name();
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
                function_return_types,
            )?;
            let wasm_result_type = result_type.to_val_type(type_ctx);
            let block_type = wasm_encoder::BlockType::Result(wasm_result_type);

            control_stack.push(ControlFrame::Block);
            func.instruction(&Instruction::Block(block_type));

            for (pattern, branch) in branches {
                let check_expr = IntermediateKind::Binding(Box::new(IntermediateBinding {
                    pattern: pattern.clone(),
                    binding_type: IntermediateType::I32,
                    expr: IntermediateKind::Identifier(Identifier::new(temp_local_name.clone())),
                }));

                emit_expression(
                    &check_expr,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    function_return_types,
                    control_stack,
                    loop_stack,
                    match_counter,
                )?;

                func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

                emit_expression(
                    branch,
                    locals,
                    locals_types,
                    func,
                    type_ctx,
                    function_return_types,
                    control_stack,
                    loop_stack,
                    match_counter,
                )?;

                func.instruction(&Instruction::Br(1));
                func.instruction(&Instruction::End);
            }

            func.instruction(&Instruction::Unreachable);

            func.instruction(&Instruction::End);
            control_stack.pop();

            Ok(())
        }
        _ => Err(Diagnostic::new("Expression is not supported in wasm exports yet")
            .with_span(SourceSpan::default())),
    }
}

fn expression_does_diverge(expr: &IntermediateKind, possibility: bool, in_inner_loop: bool) -> bool {
    match expr {
        IntermediateKind::Diverge {
            divergance_type: DivergeExpressionType::Break,
            ..
        } => !in_inner_loop,
        IntermediateKind::Diverge {
            divergance_type: DivergeExpressionType::Return,
            ..
        } => true,
        IntermediateKind::Block(exprs) => exprs
            .iter()
            .any(|expr| expression_does_diverge(expr, possibility, in_inner_loop)),
        IntermediateKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            let then_diverges = expression_does_diverge(then_branch, possibility, in_inner_loop);
            let else_diverges = expression_does_diverge(else_branch, possibility, in_inner_loop);
            if possibility {
                then_diverges || else_diverges
            } else {
                then_diverges && else_diverges
            }
        }
        IntermediateKind::Binding(binding) => {
            expression_does_diverge(&binding.expr, possibility, in_inner_loop)
        }
        IntermediateKind::Assignment { expr, .. } => {
            expression_does_diverge(expr, possibility, in_inner_loop)
        }
        IntermediateKind::FunctionCall { argument, .. } => {
            expression_does_diverge(argument, possibility, in_inner_loop)
        }
        IntermediateKind::Loop { body } => expression_does_diverge(body, possibility, true),
        IntermediateKind::PropertyAccess { object, .. } => {
            expression_does_diverge(object, possibility, in_inner_loop)
        }
        IntermediateKind::Match { value, branches } => {
            expression_does_diverge(value, possibility, in_inner_loop)
                || branches
                    .iter()
                    .all(|(_, branch)| expression_does_diverge(branch, possibility, in_inner_loop))
        }
        IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Binary(
            left,
            right,
            _,
        )) => {
            expression_does_diverge(left, possibility, in_inner_loop)
                || expression_does_diverge(right, possibility, in_inner_loop)
        }
        IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Unary(operand, _)) => {
            expression_does_diverge(operand, possibility, in_inner_loop)
        }
        IntermediateKind::Struct(fields) => fields
            .iter()
            .any(|(_, expr)| expression_does_diverge(expr, possibility, in_inner_loop)),
        IntermediateKind::Literal(_) | IntermediateKind::Identifier(_) => false,
    }
}

fn collect_break_values(expr: &IntermediateKind) -> Vec<IntermediateKind> {
    let mut values = Vec::new();
    collect_break_values_inner(expr, &mut values);
    values
}

fn collect_break_values_inner(expr: &IntermediateKind, values: &mut Vec<IntermediateKind>) {
    match expr {
        IntermediateKind::Diverge {
            value,
            divergance_type: DivergeExpressionType::Break,
        } => {
            values.push((**value).clone());
        }
        IntermediateKind::Block(exprs) => {
            for expr in exprs {
                collect_break_values_inner(expr, values);
            }
        }
        IntermediateKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            collect_break_values_inner(then_branch, values);
            collect_break_values_inner(else_branch, values);
        }
        IntermediateKind::Binding(binding) => collect_break_values_inner(&binding.expr, values),
        IntermediateKind::Assignment { expr, .. } => collect_break_values_inner(expr, values),
        IntermediateKind::FunctionCall { argument, .. } => {
            collect_break_values_inner(argument, values);
        }
        IntermediateKind::Loop { body } => collect_break_values_inner(body, values),
        IntermediateKind::PropertyAccess { object, .. } => {
            collect_break_values_inner(object, values);
        }
        IntermediateKind::Match { value, branches } => {
            collect_break_values_inner(value, values);
            for (_, branch) in branches {
                collect_break_values_inner(branch, values);
            }
        }
        IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Binary(
            left,
            right,
            _,
        )) => {
            collect_break_values_inner(left, values);
            collect_break_values_inner(right, values);
        }
        IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Unary(operand, _)) => {
            collect_break_values_inner(operand, values);
        }
        IntermediateKind::Struct(fields) => {
            for (_, expr) in fields {
                collect_break_values_inner(expr, values);
            }
        }
        _ => {}
    }
}
