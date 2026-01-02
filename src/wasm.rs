use wasm_encoder::{
    CodeSection, ConstExpr, ExportKind, ExportSection, FieldType, Function, FunctionSection,
    GlobalSection, GlobalType, HeapType, Instruction, Module, RefType, StorageType, TypeSection,
    ValType,
};

use crate::{
    diagnostics::{Diagnostic, SourceSpan},
    intermediate::{
        IntermediateBinding, IntermediateExportType, IntermediateFunction,
        IntermediateIntrinsicOperation, IntermediateKind, IntermediateResult, IntermediateType,
    },
    parsing::{
        BinaryIntrinsicOperator, BindingPattern, DivergeExpressionType, ExpressionLiteral,
        Identifier, LValue, TargetLiteral, UnaryIntrinsicOperator,
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
        let name = format!("__match_temp_{}", self.next_id);
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
    let mut stack = vec![(ty, false)];
    let mut results: Vec<String> = Vec::new();

    while let Some((node, visited)) = stack.pop() {
        if !visited {
            stack.push((node, true));
            if let WasmType::Struct(fields) = node {
                for (_, field_ty) in fields.iter().rev() {
                    stack.push((field_ty, false));
                }
            }
            continue;
        }

        match node {
            WasmType::I32 => results.push("i32".to_string()),
            WasmType::Struct(fields) => {
                let mut parts = Vec::with_capacity(fields.len());
                for (name, _) in fields.iter().rev() {
                    let ty_str = results
                        .pop()
                        .expect("format_wasm_type should have field types");
                    parts.push(format!("{}: {}", name, ty_str));
                }
                parts.reverse();
                results.push(format!("struct {{{}}}", parts.join(", ")));
            }
        }
    }

    results
        .pop()
        .expect("format_wasm_type should produce one result")
}

fn intermediate_type_to_wasm(ty: &IntermediateType) -> WasmType {
    let mut stack = vec![(ty, false)];
    let mut results: Vec<WasmType> = Vec::new();

    while let Some((node, visited)) = stack.pop() {
        if !visited {
            stack.push((node, true));
            if let IntermediateType::Struct(fields) = node {
                for (_, field_ty) in fields.iter().rev() {
                    stack.push((field_ty, false));
                }
            }
            continue;
        }

        match node {
            IntermediateType::I32 => results.push(WasmType::I32),
            IntermediateType::Struct(fields) => {
                let mut wasm_fields = Vec::with_capacity(fields.len());
                for (name, _) in fields.iter().rev() {
                    let field_ty = results
                        .pop()
                        .expect("intermediate_type_to_wasm should have field types");
                    wasm_fields.push((name.clone(), field_ty));
                }
                wasm_fields.reverse();
                wasm_fields.sort_by(|a, b| a.0.cmp(&b.0));
                results.push(WasmType::Struct(wasm_fields));
            }
        }
    }

    results
        .pop()
        .expect("intermediate_type_to_wasm should produce one result")
}

fn lvalue_to_intermediate(target: &LValue) -> IntermediateKind {
    let mut current = target;
    let mut properties = Vec::new();
    let base_identifier = loop {
        match current {
            LValue::Identifier(identifier, _) => break identifier.clone(),
            LValue::PropertyAccess {
                object, property, ..
            } => {
                properties.push(property.clone());
                current = object;
            }
        }
    };

    let mut expr = IntermediateKind::Identifier(base_identifier);
    for property in properties.iter().rev() {
        expr = IntermediateKind::PropertyAccess {
            object: Box::new(expr),
            property: property.clone(),
        };
    }
    expr
}

fn ensure_lvalue_local(
    target: &LValue,
    locals_types: &std::collections::HashMap<String, WasmType>,
    span: SourceSpan,
) -> Result<(), Diagnostic> {
    let mut current = target;
    loop {
        match current {
            LValue::Identifier(identifier, _) => {
                return if locals_types.contains_key(&identifier.name) {
                    Ok(())
                } else {
                    Err(Diagnostic::new(format!(
                        "Identifier `{}` is not a local variable or parameter",
                        identifier.name
                    ))
                    .with_span(span))
                };
            }
            LValue::PropertyAccess { object, .. } => {
                current = object;
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
        let mut stack = vec![(fields, false)];
        let mut last_index = None;

        while let Some((current, visited)) = stack.pop() {
            if let Some(&index) = self.type_map.get(&current) {
                last_index = Some(index);
                continue;
            }

            if !visited {
                stack.push((current.clone(), true));
                for (_, field_type) in current.iter().rev() {
                    if let WasmType::Struct(inner_fields) = field_type {
                        if !self.type_map.contains_key(inner_fields) {
                            stack.push((inner_fields.clone(), false));
                        }
                    }
                }
                continue;
            }

            if let Some(&index) = self.type_map.get(&current) {
                last_index = Some(index);
                continue;
            }

            let index = self.struct_types.len() as u32;
            self.struct_types.push(current.clone());
            self.type_map.insert(current, index);
            last_index = Some(index);
        }

        last_index.expect("Type should be registered")
    }

    fn get_type_index(&self, fields: &Vec<(String, WasmType)>) -> Option<u32> {
        self.type_map.get(fields).copied()
    }
}

struct WasmFunctionParam {
    name: String,
    ty: WasmType,
}

struct WasmFunctionDef {
    params: Vec<WasmFunctionParam>,
    body: IntermediateKind,
    return_type: WasmType,
}

struct WasmFunctionExport {
    name: String,
    index: usize,
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

    let all_functions = intermediate
        .functions
        .iter()
        .map(|function| {
            let params = extract_function_params(&function.parameter, &function.input_type)?;
            Ok(WasmFunctionDef {
                params,
                body: (*function.body).clone(),
                return_type: intermediate_type_to_wasm(&function.return_type),
            })
        })
        .collect::<Result<Vec<_>, Diagnostic>>()?;

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

    for function in &all_functions {
        let mut locals_types = std::collections::HashMap::new();
        for param in &function.params {
            if let WasmType::Struct(fields) = &param.ty {
                type_ctx.get_or_register_type(fields.clone());
            }
            locals_types.insert(param.name.clone(), param.ty.clone());
        }
        if let WasmType::Struct(fields) = &function.return_type {
            type_ctx.get_or_register_type(fields.clone());
        }
        collect_types(
            &function.body,
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

    for function in &all_functions {
        let param_types: Vec<ValType> = function
            .params
            .iter()
            .map(|param| param.ty.to_val_type(&type_ctx))
            .collect();
        let return_types = vec![function.return_type.to_val_type(&type_ctx)];

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

    for function in &all_functions {
        let mut locals = Vec::new();
        let mut local_indices = std::collections::HashMap::new();
        let mut locals_types = std::collections::HashMap::new();

        for (j, param) in function.params.iter().enumerate() {
            local_indices.insert(param.name.clone(), j as u32);
            locals_types.insert(param.name.clone(), param.ty.clone());
        }

        let mut match_counter = MatchCounter::default();
        let body_locals = collect_locals(
            &function.body,
            &mut locals_types,
            &function_return_types,
            &mut match_counter,
        )?;
        for (name, ty) in body_locals {
            if !local_indices.contains_key(&name) {
                local_indices.insert(name.clone(), (function.params.len() + locals.len()) as u32);
                locals.push(ty.clone());
            }
        }

        let mut func = Function::new(locals.iter().map(|ty| (1, ty.to_val_type(&type_ctx))));

        let mut control_stack = Vec::new();
        let mut loop_stack = Vec::new();

        let mut match_counter = MatchCounter::default();
        emit_expression(
            &function.body,
            &local_indices,
            &locals_types,
            &mut func,
            &type_ctx,
            &function_return_types,
            &intermediate.functions,
            &mut control_stack,
            &mut loop_stack,
            &mut match_counter,
        )?;

        let body_produces_value = expression_produces_value(
            &function.body,
            &locals_types,
            &function_return_types,
            &type_ctx,
        )?;
        let body_transfers_control = expression_does_diverge(&function.body, false, false);
        if body_produces_value {
            func.instruction(&Instruction::Return);
        } else if body_transfers_control {
            func.instruction(&Instruction::Unreachable);
        }
        func.instruction(&Instruction::End);
        code_section.function(&func);
    }

    module.section(&type_section);
    module.section(&function_section);
    if !exports.globals.is_empty() {
        module.section(&global_section);
    }
    for export in &exports.functions {
        export_section.export(&export.name, ExportKind::Func, export.index as u32);
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
                intermediate.functions.get(export.index).ok_or_else(|| {
                    Diagnostic::new(format!(
                        "Export `{}` references unknown function index {}",
                        export.name, export.index
                    ))
                    .with_span(SourceSpan::default())
                })?;

                functions.push(WasmFunctionExport {
                    name: export.name.clone(),
                    index: export.index,
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
        _ => Err(
            Diagnostic::new("Unsupported global initializer for wasm export".to_string())
                .with_span(SourceSpan::default()),
        ),
    }
}

fn collect_types(
    expr: &IntermediateKind,
    ctx: &mut TypeContext,
    locals_types: &mut std::collections::HashMap<String, WasmType>,
    function_return_types: &[WasmType],
) -> Result<(), Diagnostic> {
    let mut stack = vec![(expr, false)];

    while let Some((node, visited)) = stack.pop() {
        if !visited {
            stack.push((node, true));
            match node {
                IntermediateKind::Struct(fields) => {
                    for (_, value) in fields.iter().rev() {
                        stack.push((value, false));
                    }
                }
                IntermediateKind::Block(exprs) => {
                    for e in exprs.iter().rev() {
                        stack.push((e, false));
                    }
                }
                IntermediateKind::Binding(binding) => {
                    stack.push((&binding.expr, false));
                }
                IntermediateKind::FunctionCall { argument, .. } => {
                    stack.push((argument, false));
                }
                IntermediateKind::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    stack.push((else_branch, false));
                    stack.push((then_branch, false));
                    stack.push((condition, false));
                }
                IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Binary(
                    left,
                    right,
                    _,
                )) => {
                    stack.push((right, false));
                    stack.push((left, false));
                }
                IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Unary(
                    operand,
                    _,
                )) => {
                    stack.push((operand, false));
                }
                IntermediateKind::Loop { body } => {
                    stack.push((body, false));
                }
                IntermediateKind::PropertyAccess { object, .. } => {
                    stack.push((object, false));
                }
                _ => {}
            }
            continue;
        }

        match node {
            IntermediateKind::Struct(fields) => {
                let mut field_types = Vec::new();
                for (name, value) in fields {
                    let ty = infer_type(value, locals_types, function_return_types)?;
                    field_types.push((name.name.clone(), ty));
                }
                field_types.sort_by(|a, b| a.0.cmp(&b.0));
                ctx.get_or_register_type(field_types);
            }
            IntermediateKind::Binding(binding) => {
                let binding = binding.as_ref();
                let ty = infer_type(&binding.expr, locals_types, function_return_types)?;
                let mut locals = Vec::new();
                collect_locals_for_pattern(binding, ty, locals_types, &mut locals)?;
            }
            _ => {}
        }
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
        let ty = infer_type_basic(&val, locals_types, function_return_types)?;
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
    _type_ctx: &TypeContext,
) -> Result<bool, Diagnostic> {
    enum ValueFrame<'a> {
        Start(&'a IntermediateKind),
        FinishIf {
            then_branch: &'a IntermediateKind,
            else_branch: &'a IntermediateKind,
        },
        FinishBlock(&'a Vec<IntermediateKind>),
    }

    let mut stack = vec![ValueFrame::Start(expr)];
    let mut results: Vec<bool> = Vec::new();

    while let Some(frame) = stack.pop() {
        match frame {
            ValueFrame::Start(node) => match node {
                IntermediateKind::Diverge { .. } | IntermediateKind::Unreachable => {
                    results.push(false);
                }
                IntermediateKind::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    stack.push(ValueFrame::FinishIf {
                        then_branch,
                        else_branch,
                    });
                    stack.push(ValueFrame::Start(else_branch));
                    stack.push(ValueFrame::Start(then_branch));
                }
                IntermediateKind::Block(exprs) => {
                    let Some(last) = exprs.last() else {
                        panic!("Empty block shouldn't exist")
                    };
                    stack.push(ValueFrame::FinishBlock(exprs));
                    stack.push(ValueFrame::Start(last));
                }
                IntermediateKind::Loop { body } => {
                    results.push(
                        determine_loop_result_type(body, locals_types, function_return_types)?
                            .is_some(),
                    );
                }
                _ => results.push(true),
            },
            ValueFrame::FinishIf {
                then_branch,
                else_branch,
            } => {
                let else_produces = results
                    .pop()
                    .expect("expression_produces_value should have else value");
                let then_produces = results
                    .pop()
                    .expect("expression_produces_value should have then value");
                let then_diverges = expression_does_diverge(then_branch, false, false);
                let else_diverges = expression_does_diverge(else_branch, false, false);
                results.push(
                    then_produces && else_produces
                        || ((then_diverges || else_diverges) && (then_produces || else_produces)),
                );
            }
            ValueFrame::FinishBlock(exprs) => {
                let last_produces = results
                    .pop()
                    .expect("expression_produces_value should have block value");
                let diverges = exprs
                    .iter()
                    .any(|e| expression_does_diverge(e, false, false));
                results.push(!diverges && last_produces);
            }
        }
    }

    results
        .pop()
        .ok_or_else(|| Diagnostic::new("Failed to compute expression value".to_string()))
}

fn infer_type(
    expr: &IntermediateKind,
    locals_types: &std::collections::HashMap<String, WasmType>,
    function_return_types: &[WasmType],
) -> Result<WasmType, Diagnostic> {
    infer_type_impl(expr, locals_types, function_return_types)
}

fn infer_type_basic(
    expr: &IntermediateKind,
    locals_types: &std::collections::HashMap<String, WasmType>,
    function_return_types: &[WasmType],
) -> Result<WasmType, Diagnostic> {
    enum InferTask {
        Eval(IntermediateKind),
        FinishStruct { field_names: Vec<String> },
        FinishAssignment,
        FinishPropertyAccess { property: String },
    }

    let mut stack: Vec<InferTask> = Vec::new();
    stack.push(InferTask::Eval(expr.clone()));
    let mut results: Vec<WasmType> = Vec::new();

    while let Some(task) = stack.pop() {
        match task {
            InferTask::Eval(node) => match node {
                IntermediateKind::Literal(ExpressionLiteral::Number(_))
                | IntermediateKind::Literal(ExpressionLiteral::Boolean(_)) => {
                    results.push(WasmType::I32);
                }
                IntermediateKind::Struct(fields) => {
                    let field_names = fields.iter().map(|(name, _)| name.name.clone()).collect();
                    stack.push(InferTask::FinishStruct { field_names });
                    for (_, value) in fields.into_iter().rev() {
                        stack.push(InferTask::Eval(value));
                    }
                }
                IntermediateKind::Identifier(identifier) => {
                    let ty = locals_types.get(&identifier.name).cloned().ok_or_else(|| {
                        Diagnostic::new(format!("Unknown identifier `{}`", identifier.name))
                            .with_span(SourceSpan::default())
                    })?;
                    results.push(ty);
                }
                IntermediateKind::Assignment { target, expr: rhs } => {
                    let target_expr = lvalue_to_intermediate(&target);
                    stack.push(InferTask::FinishAssignment);
                    stack.push(InferTask::Eval(target_expr));
                    stack.push(InferTask::Eval((*rhs).clone()));
                }
                IntermediateKind::Diverge { value, .. } => {
                    stack.push(InferTask::Eval((*value).clone()));
                }
                IntermediateKind::Loop { body } => {
                    stack.push(InferTask::Eval((*body).clone()));
                }
                IntermediateKind::FunctionCall { function, .. } => {
                    let ty = function_return_types
                        .get(function)
                        .cloned()
                        .ok_or_else(|| {
                            Diagnostic::new("Unknown function call target".to_string())
                                .with_span(SourceSpan::default())
                        })?;
                    results.push(ty);
                }
                IntermediateKind::PropertyAccess { object, property } => {
                    stack.push(InferTask::FinishPropertyAccess { property });
                    stack.push(InferTask::Eval((*object).clone()));
                }
                IntermediateKind::IntrinsicOperation(..)
                | IntermediateKind::Binding(..)
                | IntermediateKind::Unreachable => {
                    results.push(WasmType::I32);
                }
                IntermediateKind::If { then_branch, .. } => {
                    stack.push(InferTask::Eval((*then_branch).clone()));
                }
                IntermediateKind::Block(exprs) => {
                    if let Some(last) = exprs.last() {
                        stack.push(InferTask::Eval(last.clone()));
                    } else {
                        results.push(WasmType::I32);
                    }
                }
                _ => results.push(WasmType::I32),
            },
            InferTask::FinishStruct { field_names } => {
                let mut field_types = Vec::with_capacity(field_names.len());
                for name in field_names.into_iter().rev() {
                    let ty = results
                        .pop()
                        .expect("infer_type_basic should have field types");
                    field_types.push((name, ty));
                }
                field_types.reverse();
                field_types.sort_by(|a, b| a.0.cmp(&b.0));
                results.push(WasmType::Struct(field_types));
            }
            InferTask::FinishAssignment => {
                let existing_type = results
                    .pop()
                    .expect("infer_type_basic should have assignment target type");
                let value_type = results
                    .pop()
                    .expect("infer_type_basic should have assignment value type");

                if value_type != existing_type {
                    return Err(Diagnostic::new(
                        "Cannot assign value of different type to target".to_string(),
                    )
                    .with_span(SourceSpan::default()));
                }
                results.push(value_type);
            }
            InferTask::FinishPropertyAccess { property } => {
                let object_type = results
                    .pop()
                    .expect("infer_type_basic should have object type");
                if let WasmType::Struct(fields) = object_type {
                    if let Some((_, field_type)) = fields.iter().find(|(n, _)| n == &property) {
                        results.push(field_type.clone());
                    } else {
                        return Err(Diagnostic::new(format!(
                            "Field `{}` not found in struct",
                            property
                        ))
                        .with_span(SourceSpan::default()));
                    }
                } else {
                    return Err(Diagnostic::new("Property access on non-struct type")
                        .with_span(SourceSpan::default()));
                }
            }
        }
    }

    results
        .pop()
        .ok_or_else(|| Diagnostic::new("Failed to infer type".to_string()))
}

fn infer_type_impl(
    expr: &IntermediateKind,
    locals_types: &std::collections::HashMap<String, WasmType>,
    function_return_types: &[WasmType],
) -> Result<WasmType, Diagnostic> {
    enum InferTask {
        Eval(IntermediateKind),
        FinishStruct { field_names: Vec<String> },
        FinishAssignment,
        FinishPropertyAccess { property: String },
        FinishLoop { body: IntermediateKind },
    }

    let mut stack: Vec<InferTask> = Vec::new();
    stack.push(InferTask::Eval(expr.clone()));
    let mut results: Vec<WasmType> = Vec::new();

    while let Some(task) = stack.pop() {
        match task {
            InferTask::Eval(node) => match node {
                IntermediateKind::Literal(ExpressionLiteral::Number(_))
                | IntermediateKind::Literal(ExpressionLiteral::Boolean(_)) => {
                    results.push(WasmType::I32);
                }
                IntermediateKind::Struct(fields) => {
                    let field_names = fields.iter().map(|(name, _)| name.name.clone()).collect();
                    stack.push(InferTask::FinishStruct { field_names });
                    for (_, value) in fields.into_iter().rev() {
                        stack.push(InferTask::Eval(value));
                    }
                }
                IntermediateKind::Identifier(identifier) => {
                    let ty = locals_types.get(&identifier.name).cloned().ok_or_else(|| {
                        Diagnostic::new(format!("Unknown identifier `{}`", identifier.name))
                            .with_span(SourceSpan::default())
                    })?;
                    results.push(ty);
                }
                IntermediateKind::Assignment { target, expr: rhs } => {
                    let target_expr = lvalue_to_intermediate(&target);
                    stack.push(InferTask::FinishAssignment);
                    stack.push(InferTask::Eval(target_expr));
                    stack.push(InferTask::Eval((*rhs).clone()));
                }
                IntermediateKind::Diverge { value, .. } => {
                    stack.push(InferTask::Eval((*value).clone()));
                }
                IntermediateKind::Loop { body } => {
                    stack.push(InferTask::FinishLoop {
                        body: (*body).clone(),
                    });
                    stack.push(InferTask::Eval((*body).clone()));
                }
                IntermediateKind::FunctionCall { function, .. } => {
                    let ty = function_return_types
                        .get(function)
                        .cloned()
                        .ok_or_else(|| {
                            Diagnostic::new("Unknown function call target".to_string())
                                .with_span(SourceSpan::default())
                        })?;
                    results.push(ty);
                }
                IntermediateKind::PropertyAccess { object, property } => {
                    stack.push(InferTask::FinishPropertyAccess { property });
                    stack.push(InferTask::Eval((*object).clone()));
                }
                IntermediateKind::IntrinsicOperation(..)
                | IntermediateKind::Binding(..)
                | IntermediateKind::Unreachable => {
                    results.push(WasmType::I32);
                }
                IntermediateKind::If { then_branch, .. } => {
                    stack.push(InferTask::Eval((*then_branch).clone()));
                }
                IntermediateKind::Block(exprs) => {
                    if let Some(last) = exprs.last() {
                        stack.push(InferTask::Eval(last.clone()));
                    } else {
                        results.push(WasmType::I32);
                    }
                }
                _ => results.push(WasmType::I32),
            },
            InferTask::FinishStruct { field_names } => {
                let mut field_types = Vec::with_capacity(field_names.len());
                for name in field_names.into_iter().rev() {
                    let ty = results
                        .pop()
                        .expect("infer_type_impl should have field types");
                    field_types.push((name, ty));
                }
                field_types.reverse();
                field_types.sort_by(|a, b| a.0.cmp(&b.0));
                results.push(WasmType::Struct(field_types));
            }
            InferTask::FinishAssignment => {
                let existing_type = results
                    .pop()
                    .expect("infer_type_impl should have assignment target type");
                let value_type = results
                    .pop()
                    .expect("infer_type_impl should have assignment value type");

                if value_type != existing_type {
                    return Err(Diagnostic::new(
                        "Cannot assign value of different type to target".to_string(),
                    )
                    .with_span(SourceSpan::default()));
                }
                results.push(value_type);
            }
            InferTask::FinishPropertyAccess { property } => {
                let object_type = results
                    .pop()
                    .expect("infer_type_impl should have object type");
                if let WasmType::Struct(fields) = object_type {
                    if let Some((_, field_type)) = fields.iter().find(|(n, _)| n == &property) {
                        results.push(field_type.clone());
                    } else {
                        return Err(Diagnostic::new(format!(
                            "Field `{}` not found in struct",
                            property
                        ))
                        .with_span(SourceSpan::default()));
                    }
                } else {
                    return Err(Diagnostic::new("Property access on non-struct type")
                        .with_span(SourceSpan::default()));
                }
            }
            InferTask::FinishLoop { body } => {
                let body_type = results
                    .pop()
                    .ok_or_else(|| Diagnostic::new("Failed to infer type".to_string()))?;
                if let Some(result_type) =
                    determine_loop_result_type(&body, locals_types, function_return_types)?
                {
                    results.push(result_type);
                } else {
                    results.push(body_type);
                }
            }
        }
    }

    results
        .pop()
        .ok_or_else(|| Diagnostic::new("Failed to infer type".to_string()))
}

fn extract_function_params(
    pattern: &BindingPattern,
    ty: &IntermediateType,
) -> Result<Vec<WasmFunctionParam>, Diagnostic> {
    let mut params = Vec::new();
    let mut stack = vec![(pattern, ty)];

    while let Some((current_pattern, current_type)) = stack.pop() {
        match stripped_binding_pattern(current_pattern) {
            BindingPattern::Struct(fields, span) => {
                let field_types = if let IntermediateType::Struct(field_types) = current_type {
                    field_types
                } else {
                    return Err(
                        Diagnostic::new("Struct pattern requires struct parameter type")
                            .with_span(*span),
                    );
                };

                for (field_id, sub_pattern) in fields.iter().rev() {
                    let field_ty = field_types
                        .iter()
                        .find(|(name, _)| name == &field_id.name)
                        .map(|(_, ty)| ty)
                        .ok_or_else(|| {
                            Diagnostic::new(format!(
                                "Missing field {} in struct parameter type",
                                field_id.name
                            ))
                            .with_span(sub_pattern.span())
                        })?;
                    stack.push((sub_pattern, field_ty));
                }
            }
            BindingPattern::Identifier(identifier, _) => params.push(WasmFunctionParam {
                name: identifier.name.clone(),
                ty: intermediate_type_to_wasm(current_type),
            }),
            BindingPattern::Literal(_, span) => {
                return Err(Diagnostic::new(
                    "Literal patterns cannot be used in function parameters",
                )
                .with_span(*span));
            }
            BindingPattern::EnumVariant { span, .. } => {
                return Err(
                    Diagnostic::new("Enum patterns cannot be used in function parameters")
                        .with_span(*span),
                );
            }
            BindingPattern::TypeHint(_, _, span) => {
                return Err(
                    Diagnostic::new("Unsupported pattern in function parameter").with_span(*span)
                );
            }
            BindingPattern::Annotated { span, .. } => {
                return Err(
                    Diagnostic::new("Unsupported pattern in function parameter").with_span(*span)
                );
            }
        }
    }

    Ok(params)
}

fn stripped_binding_pattern(pattern: &BindingPattern) -> &BindingPattern {
    let mut current = pattern;
    loop {
        match current {
            BindingPattern::TypeHint(inner, _, _) => current = inner,
            BindingPattern::Annotated { pattern: inner, .. } => current = inner,
            other => return other,
        }
    }
}

fn collect_locals(
    expr: &IntermediateKind,
    locals_types: &mut std::collections::HashMap<String, WasmType>,
    function_return_types: &[WasmType],
    match_counter: &mut MatchCounter,
) -> Result<Vec<(String, WasmType)>, Diagnostic> {
    let mut locals = Vec::new();
    let mut stack = vec![expr];

    while let Some(node) = stack.pop() {
        match node {
            IntermediateKind::Binding(binding) => {
                let binding = binding.as_ref();
                let expr_type = infer_type(&binding.expr, locals_types, function_return_types)?;
                collect_locals_for_pattern(binding, expr_type, locals_types, &mut locals)?;
                stack.push(&binding.expr);
            }
            IntermediateKind::Assignment { target, expr } => {
                ensure_lvalue_local(target, locals_types, SourceSpan::default())?;
                stack.push(expr);
            }
            IntermediateKind::Block(exprs) => {
                for e in exprs.iter().rev() {
                    stack.push(e);
                }
            }
            IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Binary(
                left,
                right,
                _,
            )) => {
                stack.push(right);
                stack.push(left);
            }
            IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Unary(
                operand,
                _,
            )) => {
                stack.push(operand);
            }
            IntermediateKind::FunctionCall { argument, .. } => {
                let arg_type = infer_type(argument, locals_types, function_return_types)?;
                let temp_local_name = match_counter.next_name();
                locals.push((temp_local_name.clone(), arg_type.clone()));
                locals_types.insert(temp_local_name, arg_type);
                stack.push(argument);
            }
            IntermediateKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                stack.push(else_branch);
                stack.push(then_branch);
                stack.push(condition);
            }
            IntermediateKind::Loop { body } => {
                stack.push(body);
            }
            _ => {}
        }
    }
    Ok(locals)
}

fn collect_locals_for_pattern(
    binding: &IntermediateBinding,
    expr_type: WasmType,
    locals_types: &mut std::collections::HashMap<String, WasmType>,
    locals: &mut Vec<(String, WasmType)>,
) -> Result<(), Diagnostic> {
    let name = binding.identifier.name.clone();
    locals.push((name.clone(), expr_type.clone()));
    locals_types.insert(name, expr_type);
    Ok(())
}

fn flatten_call_arguments(
    pattern: &BindingPattern,
    input_type: &IntermediateType,
    argument_expr: IntermediateKind,
) -> Result<Vec<IntermediateKind>, Diagnostic> {
    let mut results = Vec::new();
    let mut stack = vec![(pattern, input_type, argument_expr)];

    while let Some((current_pattern, current_type, current_expr)) = stack.pop() {
        match stripped_binding_pattern(current_pattern) {
            BindingPattern::Identifier(_, _) => results.push(current_expr),
            BindingPattern::Struct(fields, span) => {
                let field_types = if let IntermediateType::Struct(field_types) = current_type {
                    field_types
                } else {
                    return Err(
                        Diagnostic::new("Struct pattern requires struct parameter type")
                            .with_span(*span),
                    );
                };

                for (field_id, sub_pattern) in fields.iter().rev() {
                    let field_ty = field_types
                        .iter()
                        .find(|(name, _)| name == &field_id.name)
                        .map(|(_, ty)| ty)
                        .ok_or_else(|| {
                            Diagnostic::new(format!(
                                "Missing field {} in struct parameter type",
                                field_id.name
                            ))
                            .with_span(sub_pattern.span())
                        })?;
                    let field_expr = IntermediateKind::PropertyAccess {
                        object: Box::new(current_expr.clone()),
                        property: field_id.name.clone(),
                    };
                    stack.push((sub_pattern, field_ty, field_expr));
                }
            }
            BindingPattern::Literal(_, span) => {
                return Err(Diagnostic::new(
                    "Literal patterns cannot be used in function parameters",
                )
                .with_span(*span));
            }
            BindingPattern::EnumVariant { span, .. } => {
                return Err(
                    Diagnostic::new("Enum patterns cannot be used in function parameters")
                        .with_span(*span),
                );
            }
            BindingPattern::TypeHint(_, _, span) => {
                return Err(
                    Diagnostic::new("Unsupported pattern in function parameter").with_span(*span)
                );
            }
            BindingPattern::Annotated { span, .. } => {
                return Err(
                    Diagnostic::new("Unsupported pattern in function parameter").with_span(*span)
                );
            }
        }
    }

    Ok(results)
}

fn emit_expression(
    expr: &IntermediateKind,
    locals: &std::collections::HashMap<String, u32>,
    locals_types: &std::collections::HashMap<String, WasmType>,
    func: &mut Function,
    type_ctx: &TypeContext,
    function_return_types: &[WasmType],
    functions: &[IntermediateFunction],
    control_stack: &mut Vec<ControlFrame>,
    loop_stack: &mut Vec<LoopContext>,
    match_counter: &mut MatchCounter,
) -> Result<(), Diagnostic> {
    enum EmitTask<'a> {
        Eval(IntermediateKind),
        Instr(Instruction<'a>),
        PushControl(ControlFrame),
        PopControl,
        PushLoopContext {
            result_type: Option<WasmType>,
        },
        PopLoopContext,
        CheckCallArgType {
            arg_type: WasmType,
            expected_type: WasmType,
        },
    }

    let mut tasks = vec![EmitTask::Eval(expr.clone())];

    while let Some(task) = tasks.pop() {
        match task {
            EmitTask::Instr(instr) => {
                func.instruction(&instr);
            }
            EmitTask::PushControl(frame) => control_stack.push(frame),
            EmitTask::PopControl => {
                control_stack.pop();
            }
            EmitTask::PushLoopContext { result_type } => {
                let break_target_index = control_stack.len().saturating_sub(2);
                loop_stack.push(LoopContext {
                    break_target_index,
                    result_type,
                });
            }
            EmitTask::PopLoopContext => {
                loop_stack.pop();
            }
            EmitTask::CheckCallArgType {
                arg_type,
                expected_type,
            } => {
                if arg_type != expected_type {
                    return Err(Diagnostic::new(
                        "Function call argument does not match function input type".to_string(),
                    )
                    .with_span(SourceSpan::default()));
                }
            }
            EmitTask::Eval(node) => match node {
                IntermediateKind::Literal(ExpressionLiteral::Number(value)) => {
                    tasks.push(EmitTask::Instr(Instruction::I32Const(value)));
                }
                IntermediateKind::Literal(ExpressionLiteral::Boolean(value)) => {
                    tasks.push(EmitTask::Instr(Instruction::I32Const(if value {
                        1
                    } else {
                        0
                    })));
                }
                IntermediateKind::Identifier(identifier) => {
                    let local_index = locals.get(&identifier.name).copied().ok_or_else(|| {
                        Diagnostic::new(format!(
                            "Identifier `{}` is not a local variable or parameter",
                            identifier.name
                        ))
                        .with_span(SourceSpan::default())
                    })?;
                    tasks.push(EmitTask::Instr(Instruction::LocalGet(local_index)));
                }
                IntermediateKind::Assignment {
                    target,
                    expr: value,
                } => match target {
                    LValue::Identifier(identifier, _) => {
                        let local_index =
                            locals.get(&identifier.name).copied().ok_or_else(|| {
                                Diagnostic::new(format!(
                                    "Identifier `{}` is not a local variable or parameter",
                                    identifier.name
                                ))
                                .with_span(SourceSpan::default())
                            })?;
                        tasks.push(EmitTask::Instr(Instruction::LocalTee(local_index)));
                        tasks.push(EmitTask::Eval((*value).clone()));
                    }
                    LValue::PropertyAccess {
                        ref object,
                        ref property,
                        ..
                    } => {
                        let object_expr = lvalue_to_intermediate(object);
                        let object_type =
                            infer_type(&object_expr, locals_types, function_return_types)?;
                        let WasmType::Struct(fields) = object_type else {
                            return Err(Diagnostic::new("Property assignment on non-struct type")
                                .with_span(SourceSpan::default()));
                        };

                        let field_index = fields
                            .iter()
                            .position(|(name, _)| name == property)
                            .ok_or_else(|| {
                                Diagnostic::new(format!("Field `{}` not found in struct", property))
                                    .with_span(SourceSpan::default())
                            })? as u32;

                        let type_index = type_ctx
                            .get_type_index(&fields)
                            .expect("Type should be registered");

                        let full_target_expr = lvalue_to_intermediate(&target);

                        tasks.push(EmitTask::Eval(full_target_expr));
                        tasks.push(EmitTask::Instr(Instruction::StructSet {
                            struct_type_index: type_index,
                            field_index,
                        }));
                        tasks.push(EmitTask::Eval((*value).clone()));
                        tasks.push(EmitTask::Eval(object_expr));
                    }
                },
                IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Binary(
                    left,
                    right,
                    op,
                )) => match op {
                    BinaryIntrinsicOperator::BooleanAnd => {
                        tasks.push(EmitTask::PopControl);
                        tasks.push(EmitTask::Instr(Instruction::End));
                        tasks.push(EmitTask::Instr(Instruction::I32Const(0)));
                        tasks.push(EmitTask::Instr(Instruction::Else));
                        tasks.push(EmitTask::Eval((*right).clone()));
                        tasks.push(EmitTask::Instr(Instruction::If(
                            wasm_encoder::BlockType::Result(ValType::I32),
                        )));
                        tasks.push(EmitTask::PushControl(ControlFrame::If));
                        tasks.push(EmitTask::Eval((*left).clone()));
                    }
                    BinaryIntrinsicOperator::BooleanOr => {
                        tasks.push(EmitTask::PopControl);
                        tasks.push(EmitTask::Instr(Instruction::End));
                        tasks.push(EmitTask::Eval((*right).clone()));
                        tasks.push(EmitTask::Instr(Instruction::Else));
                        tasks.push(EmitTask::Instr(Instruction::I32Const(1)));
                        tasks.push(EmitTask::Instr(Instruction::If(
                            wasm_encoder::BlockType::Result(ValType::I32),
                        )));
                        tasks.push(EmitTask::PushControl(ControlFrame::If));
                        tasks.push(EmitTask::Eval((*left).clone()));
                    }
                    _ => {
                        let op_instr = match op {
                            BinaryIntrinsicOperator::I32Add => Instruction::I32Add,
                            BinaryIntrinsicOperator::I32Subtract => Instruction::I32Sub,
                            BinaryIntrinsicOperator::I32Multiply => Instruction::I32Mul,
                            BinaryIntrinsicOperator::I32Divide => Instruction::I32DivS,
                            BinaryIntrinsicOperator::I32Equal => Instruction::I32Eq,
                            BinaryIntrinsicOperator::I32NotEqual => Instruction::I32Ne,
                            BinaryIntrinsicOperator::I32LessThan => Instruction::I32LtS,
                            BinaryIntrinsicOperator::I32GreaterThan => Instruction::I32GtS,
                            BinaryIntrinsicOperator::I32LessThanOrEqual => Instruction::I32LeS,
                            BinaryIntrinsicOperator::I32GreaterThanOrEqual => Instruction::I32GeS,
                            BinaryIntrinsicOperator::BooleanXor => Instruction::I32Xor,
                            BinaryIntrinsicOperator::BooleanAnd
                            | BinaryIntrinsicOperator::BooleanOr => {
                                unreachable!("boolean ops handled before op_instr")
                            }
                        };
                        tasks.push(EmitTask::Instr(op_instr));
                        tasks.push(EmitTask::Eval((*right).clone()));
                        tasks.push(EmitTask::Eval((*left).clone()));
                    }
                },
                IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Unary(
                    _,
                    UnaryIntrinsicOperator::MatchFromStruct,
                )) => {
                    return Err(Diagnostic::new(
                        "match intrinsic should be resolved before wasm lowering",
                    )
                    .with_span(SourceSpan::default()));
                }
                IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Unary(
                    operand,
                    UnaryIntrinsicOperator::BooleanNot,
                )) => {
                    tasks.push(EmitTask::Instr(Instruction::I32Eqz));
                    tasks.push(EmitTask::Eval((*operand).clone()));
                }
                IntermediateKind::Diverge {
                    value,
                    divergance_type: DivergeExpressionType::Break,
                } => {
                    let loop_ctx = loop_stack.last().cloned().ok_or_else(|| {
                        Diagnostic::new("`break` used outside of a loop")
                            .with_span(SourceSpan::default())
                    })?;

                    let expected_type = loop_ctx.result_type.as_ref().ok_or_else(|| {
                        Diagnostic::new("`break` cannot be used in a loop without a break value")
                            .with_span(SourceSpan::default())
                    })?;

                    let value_type = infer_type(&value, locals_types, function_return_types)?;
                    if &value_type != expected_type {
                        return Err(
                            Diagnostic::new("break value does not match loop result type")
                                .with_span(SourceSpan::default()),
                        );
                    }

                    let break_depth = control_stack
                        .len()
                        .saturating_sub(loop_ctx.break_target_index + 1)
                        as u32;

                    tasks.push(EmitTask::Instr(Instruction::Br(break_depth)));
                    tasks.push(EmitTask::Eval((*value).clone()));
                }
                IntermediateKind::Loop { body } => {
                    let loop_result_type =
                        determine_loop_result_type(&body, locals_types, function_return_types)?;
                    if let Some(result_type) = loop_result_type {
                        let block_type =
                            wasm_encoder::BlockType::Result(result_type.to_val_type(type_ctx));
                        let body_produces_value = expression_produces_value(
                            &body,
                            locals_types,
                            function_return_types,
                            type_ctx,
                        )?;

                        tasks.push(EmitTask::Instr(Instruction::End));
                        tasks.push(EmitTask::Instr(Instruction::Unreachable));
                        tasks.push(EmitTask::PopControl);
                        tasks.push(EmitTask::PopLoopContext);
                        tasks.push(EmitTask::Instr(Instruction::End));
                        tasks.push(EmitTask::PopControl);
                        tasks.push(EmitTask::Instr(Instruction::Br(0)));
                        if body_produces_value {
                            tasks.push(EmitTask::Instr(Instruction::Drop));
                        }
                        tasks.push(EmitTask::Eval((*body).clone()));
                        tasks.push(EmitTask::Instr(Instruction::Loop(
                            wasm_encoder::BlockType::Empty,
                        )));
                        tasks.push(EmitTask::PushLoopContext {
                            result_type: Some(result_type.clone()),
                        });
                        tasks.push(EmitTask::PushControl(ControlFrame::Loop));
                        tasks.push(EmitTask::Instr(Instruction::Block(block_type)));
                        tasks.push(EmitTask::PushControl(ControlFrame::Block));
                    } else {
                        let body_produces_value = expression_produces_value(
                            &body,
                            locals_types,
                            function_return_types,
                            type_ctx,
                        )?;

                        tasks.push(EmitTask::Instr(Instruction::End));
                        tasks.push(EmitTask::PopControl);
                        tasks.push(EmitTask::Instr(Instruction::Br(0)));
                        if body_produces_value {
                            tasks.push(EmitTask::Instr(Instruction::Drop));
                        }
                        tasks.push(EmitTask::Eval((*body).clone()));
                        tasks.push(EmitTask::Instr(Instruction::Loop(
                            wasm_encoder::BlockType::Empty,
                        )));
                        tasks.push(EmitTask::PushControl(ControlFrame::Loop));
                    }
                }
                IntermediateKind::FunctionCall { function, argument } => {
                    let callee = functions.get(function).ok_or_else(|| {
                        Diagnostic::new("Unknown function call target".to_string())
                            .with_span(SourceSpan::default())
                    })?;

                    let arg_type = infer_type(&argument, locals_types, function_return_types)?;
                    let temp_local_name = match_counter.next_name();
                    let temp_local_index = locals
                        .get(&temp_local_name)
                        .copied()
                        .expect("Temp local should exist");

                    let temp_identifier =
                        IntermediateKind::Identifier(Identifier::new(temp_local_name));
                    let argument_exprs = flatten_call_arguments(
                        &callee.parameter,
                        &callee.input_type,
                        temp_identifier,
                    )?;

                    let expected_type = intermediate_type_to_wasm(&callee.input_type);

                    tasks.push(EmitTask::Instr(Instruction::Call(function as u32)));
                    tasks.push(EmitTask::CheckCallArgType {
                        arg_type,
                        expected_type,
                    });

                    for expr in argument_exprs.into_iter().rev() {
                        tasks.push(EmitTask::Eval(expr));
                    }

                    tasks.push(EmitTask::Instr(Instruction::LocalSet(temp_local_index)));
                    tasks.push(EmitTask::Eval((*argument).clone()));
                }
                IntermediateKind::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    let then_produces_value = expression_produces_value(
                        &then_branch,
                        locals_types,
                        function_return_types,
                        type_ctx,
                    )?;
                    let else_produces_value = expression_produces_value(
                        &else_branch,
                        locals_types,
                        function_return_types,
                        type_ctx,
                    )?;

                    let then_diverges = expression_does_diverge(&then_branch, false, false);
                    let else_diverges = expression_does_diverge(&else_branch, false, false);

                    let block_type = if (then_produces_value && else_produces_value)
                        || ((then_diverges || else_diverges)
                            && (then_produces_value || else_produces_value))
                    {
                        let result_type = if then_produces_value {
                            infer_type(&then_branch, locals_types, function_return_types)?
                        } else {
                            infer_type(&else_branch, locals_types, function_return_types)?
                        };
                        let wasm_result_type = result_type.to_val_type(type_ctx);
                        wasm_encoder::BlockType::Result(wasm_result_type)
                    } else {
                        wasm_encoder::BlockType::Empty
                    };

                    tasks.push(EmitTask::PopControl);
                    tasks.push(EmitTask::Instr(Instruction::End));
                    if else_produces_value && matches!(block_type, wasm_encoder::BlockType::Empty) {
                        tasks.push(EmitTask::Instr(Instruction::Drop));
                    }
                    tasks.push(EmitTask::Eval((*else_branch).clone()));
                    tasks.push(EmitTask::Instr(Instruction::Else));
                    if then_produces_value && matches!(block_type, wasm_encoder::BlockType::Empty) {
                        tasks.push(EmitTask::Instr(Instruction::Drop));
                    }
                    tasks.push(EmitTask::Eval((*then_branch).clone()));
                    tasks.push(EmitTask::Instr(Instruction::If(block_type)));
                    tasks.push(EmitTask::PushControl(ControlFrame::If));
                    tasks.push(EmitTask::Eval((*condition).clone()));
                }
                IntermediateKind::Binding(binding) => {
                    let binding = binding.as_ref();
                    let name = binding.identifier.name.clone();
                    let local_index = locals
                        .get(&name)
                        .copied()
                        .unwrap_or_else(|| panic!("Local '{}' should have been collected", name));
                    tasks.push(EmitTask::Instr(Instruction::I32Const(1)));
                    tasks.push(EmitTask::Instr(Instruction::LocalSet(local_index)));
                    tasks.push(EmitTask::Eval(binding.expr.clone()));
                }
                IntermediateKind::Block(exprs) => {
                    let mut end_index = exprs.len();
                    for (i, e) in exprs.iter().enumerate() {
                        if expression_does_diverge(e, false, false) {
                            end_index = i + 1;
                            break;
                        }
                    }

                    for (i, e) in exprs.iter().take(end_index).enumerate().rev() {
                        if i < end_index - 1
                            && expression_produces_value(
                                e,
                                locals_types,
                                function_return_types,
                                type_ctx,
                            )?
                        {
                            tasks.push(EmitTask::Instr(Instruction::Drop));
                        }
                        tasks.push(EmitTask::Eval(e.clone()));
                    }
                }
                IntermediateKind::Diverge {
                    value,
                    divergance_type: DivergeExpressionType::Return,
                } => {
                    tasks.push(EmitTask::Instr(Instruction::Return));
                    tasks.push(EmitTask::Eval((*value).clone()));
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

                    tasks.push(EmitTask::Instr(Instruction::StructNew(type_index)));

                    for (_, value) in sorted_items.into_iter().rev() {
                        tasks.push(EmitTask::Eval(value));
                    }
                }
                IntermediateKind::PropertyAccess { object, property } => {
                    let object_type = infer_type(&object, locals_types, function_return_types)?;
                    if let WasmType::Struct(fields) = object_type {
                        let field_index = fields
                            .iter()
                            .position(|(n, _)| n == &property)
                            .ok_or_else(|| {
                                Diagnostic::new(format!("Field `{}` not found in struct", property))
                                    .with_span(SourceSpan::default())
                            })?;

                        let type_index = type_ctx
                            .get_type_index(&fields)
                            .expect("Type should be registered");

                        tasks.push(EmitTask::Instr(Instruction::StructGet {
                            struct_type_index: type_index,
                            field_index: field_index as u32,
                        }));
                        tasks.push(EmitTask::Eval((*object).clone()));
                    } else {
                        return Err(Diagnostic::new("Property access on non-struct type")
                            .with_span(SourceSpan::default()));
                    }
                }
                IntermediateKind::Unreachable => {
                    tasks.push(EmitTask::Instr(Instruction::Unreachable));
                }
                _ => {
                    return Err(
                        Diagnostic::new("Expression is not supported in wasm exports yet")
                            .with_span(SourceSpan::default()),
                    );
                }
            },
        }
    }

    Ok(())
}
fn expression_does_diverge(
    expr: &IntermediateKind,
    possibility: bool,
    in_inner_loop: bool,
) -> bool {
    struct Frame<'a> {
        expr: &'a IntermediateKind,
        possibility: bool,
        in_inner_loop: bool,
        visited: bool,
    }

    let mut stack = vec![Frame {
        expr,
        possibility,
        in_inner_loop,
        visited: false,
    }];
    let mut results: Vec<bool> = Vec::new();

    while let Some(frame) = stack.pop() {
        if !frame.visited {
            stack.push(Frame {
                visited: true,
                ..frame
            });
            match frame.expr {
                IntermediateKind::Block(exprs) => {
                    for child in exprs.iter().rev() {
                        stack.push(Frame {
                            expr: child,
                            possibility: frame.possibility,
                            in_inner_loop: frame.in_inner_loop,
                            visited: false,
                        });
                    }
                }
                IntermediateKind::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    stack.push(Frame {
                        expr: else_branch,
                        possibility: frame.possibility,
                        in_inner_loop: frame.in_inner_loop,
                        visited: false,
                    });
                    stack.push(Frame {
                        expr: then_branch,
                        possibility: frame.possibility,
                        in_inner_loop: frame.in_inner_loop,
                        visited: false,
                    });
                }
                IntermediateKind::Binding(binding) => {
                    stack.push(Frame {
                        expr: &binding.expr,
                        possibility: frame.possibility,
                        in_inner_loop: frame.in_inner_loop,
                        visited: false,
                    });
                }
                IntermediateKind::Assignment { expr, .. } => {
                    stack.push(Frame {
                        expr,
                        possibility: frame.possibility,
                        in_inner_loop: frame.in_inner_loop,
                        visited: false,
                    });
                }
                IntermediateKind::FunctionCall { argument, .. } => {
                    stack.push(Frame {
                        expr: argument,
                        possibility: frame.possibility,
                        in_inner_loop: frame.in_inner_loop,
                        visited: false,
                    });
                }
                IntermediateKind::Loop { body } => {
                    stack.push(Frame {
                        expr: body,
                        possibility: frame.possibility,
                        in_inner_loop: true,
                        visited: false,
                    });
                }
                IntermediateKind::PropertyAccess { object, .. } => {
                    stack.push(Frame {
                        expr: object,
                        possibility: frame.possibility,
                        in_inner_loop: frame.in_inner_loop,
                        visited: false,
                    });
                }
                IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Binary(
                    left,
                    right,
                    _,
                )) => {
                    stack.push(Frame {
                        expr: right,
                        possibility: frame.possibility,
                        in_inner_loop: frame.in_inner_loop,
                        visited: false,
                    });
                    stack.push(Frame {
                        expr: left,
                        possibility: frame.possibility,
                        in_inner_loop: frame.in_inner_loop,
                        visited: false,
                    });
                }
                IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Unary(
                    operand,
                    _,
                )) => {
                    stack.push(Frame {
                        expr: operand,
                        possibility: frame.possibility,
                        in_inner_loop: frame.in_inner_loop,
                        visited: false,
                    });
                }
                IntermediateKind::Struct(fields) => {
                    for (_, child) in fields.iter().rev() {
                        stack.push(Frame {
                            expr: child,
                            possibility: frame.possibility,
                            in_inner_loop: frame.in_inner_loop,
                            visited: false,
                        });
                    }
                }
                _ => {}
            }
            continue;
        }

        let result = match frame.expr {
            IntermediateKind::Diverge {
                divergance_type: DivergeExpressionType::Break,
                ..
            } => !frame.in_inner_loop,
            IntermediateKind::Unreachable => true,
            IntermediateKind::Diverge {
                divergance_type: DivergeExpressionType::Return,
                ..
            } => true,
            IntermediateKind::Block(exprs) => {
                let mut diverges = false;
                for _ in 0..exprs.len() {
                    if results.pop().unwrap_or(false) {
                        diverges = true;
                    }
                }
                diverges
            }
            IntermediateKind::If { .. } => {
                let else_diverges = results.pop().unwrap_or(false);
                let then_diverges = results.pop().unwrap_or(false);
                if frame.possibility {
                    then_diverges || else_diverges
                } else {
                    then_diverges && else_diverges
                }
            }
            IntermediateKind::Binding(..)
            | IntermediateKind::Assignment { .. }
            | IntermediateKind::FunctionCall { .. }
            | IntermediateKind::Loop { .. }
            | IntermediateKind::PropertyAccess { .. }
            | IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Unary(..)) => {
                results.pop().unwrap_or(false)
            }
            IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Binary(..)) => {
                let right_diverges = results.pop().unwrap_or(false);
                let left_diverges = results.pop().unwrap_or(false);
                left_diverges || right_diverges
            }
            IntermediateKind::Struct(fields) => {
                let mut diverges = false;
                for _ in 0..fields.len() {
                    if results.pop().unwrap_or(false) {
                        diverges = true;
                    }
                }
                diverges
            }
            IntermediateKind::Literal(_) | IntermediateKind::Identifier(_) => false,
        };

        results.push(result);
    }

    results.pop().unwrap_or(false)
}

fn collect_break_values(expr: &IntermediateKind) -> Vec<IntermediateKind> {
    let mut values = Vec::new();
    let mut stack = vec![expr];

    while let Some(node) = stack.pop() {
        match node {
            IntermediateKind::Diverge {
                value,
                divergance_type: DivergeExpressionType::Break,
            } => {
                values.push((**value).clone());
            }
            IntermediateKind::Block(exprs) => {
                for expr in exprs.iter().rev() {
                    stack.push(expr);
                }
            }
            IntermediateKind::If {
                then_branch,
                else_branch,
                ..
            } => {
                stack.push(else_branch);
                stack.push(then_branch);
            }
            IntermediateKind::Binding(binding) => {
                stack.push(&binding.expr);
            }
            IntermediateKind::Assignment { expr, .. } => {
                stack.push(expr);
            }
            IntermediateKind::FunctionCall { argument, .. } => {
                stack.push(argument);
            }
            IntermediateKind::Loop { body } => {
                stack.push(body);
            }
            IntermediateKind::PropertyAccess { object, .. } => {
                stack.push(object);
            }
            IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Binary(
                left,
                right,
                _,
            )) => {
                stack.push(right);
                stack.push(left);
            }
            IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Unary(
                operand,
                _,
            )) => {
                stack.push(operand);
            }
            IntermediateKind::Struct(fields) => {
                for (_, expr) in fields.iter().rev() {
                    stack.push(expr);
                }
            }
            _ => {}
        }
    }

    values
}
