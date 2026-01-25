mod encoder;
mod types;

use self::encoder::{
    ArrayType, BlockType, CodeSection, CompositeInnerType, CompositeType, ConstExpr, DataSection,
    EntityType, ExportKind, ExportSection, FieldType, Function, FunctionSection, GlobalSection,
    GlobalType, ImportSection, Instruction, MemorySection, MemoryType, Module, StorageType,
    StructType, SubType, TypeSection, ValType,
};
use self::types::{
    BoxContext, BoxInfo, BoxRegistry, ControlFrame, LoopContext, MatchCounter, TypeContext,
    WasmExports, WasmFunctionDef, WasmFunctionExport, WasmFunctionImport, WasmFunctionParam,
    WasmGlobalExport, WasmType,
};
use self::types::{
    format_wasm_type, intermediate_type_to_wasm, memarg, pages_for_size, record_box_binding,
    resolve_box_expr, sorted_wasm_fields, strip_box_owned, strip_box_type,
    struct_fields_to_wasm_type, wasm_type_size, wasm_types_equivalent,
};

use crate::{
    diagnostics::{Diagnostic, SourceSpan},
    intermediate::{
        IntermediateBinding, IntermediateExportType, IntermediateFunction,
        IntermediateIntrinsicOperation, IntermediateKind, IntermediateLValue, IntermediateResult,
        IntermediateType,
    },
    parsing::{
        BinaryIntrinsicOperator, BindingPattern, DivergeExpressionType, ExpressionLiteral,
        Identifier, TargetLiteral, UnaryIntrinsicOperator,
    },
};

fn lvalue_to_intermediate(target: &IntermediateLValue) -> IntermediateKind {
    match target {
        IntermediateLValue::Identifier(identifier, _) => {
            IntermediateKind::Identifier(identifier.clone())
        }
        IntermediateLValue::TypePropertyAccess {
            object, property, ..
        } => IntermediateKind::TypePropertyAccess {
            object: Box::new(lvalue_to_intermediate(object)),
            property: property.clone(),
        },
        IntermediateLValue::ArrayIndex { array, index, .. } => IntermediateKind::ArrayIndex {
            array: Box::new(lvalue_to_intermediate(array)),
            index: Box::new((**index).clone()),
        },
    }
}

fn ensure_lvalue_local(
    target: &IntermediateLValue,
    locals_types: &std::collections::HashMap<String, WasmType>,
    span: SourceSpan,
) -> Result<(), Diagnostic> {
    let mut current = target;
    loop {
        match current {
            IntermediateLValue::Identifier(identifier, _) => {
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
            IntermediateLValue::TypePropertyAccess { object, .. } => {
                current = object;
            }
            IntermediateLValue::ArrayIndex { array, .. } => {
                current = array;
            }
        }
    }
}

fn resolve_inline_constants(
    expr: &IntermediateKind,
    inline_bindings: &std::collections::HashMap<String, IntermediateKind>,
) -> IntermediateKind {
    match expr {
        IntermediateKind::Identifier(identifier) => inline_bindings
            .get(&identifier.name)
            .cloned()
            .unwrap_or_else(|| IntermediateKind::Identifier(identifier.clone())),
        IntermediateKind::Struct(items) => IntermediateKind::Struct(
            items
                .iter()
                .map(|(id, value)| (id.clone(), resolve_inline_constants(value, inline_bindings)))
                .collect(),
        ),
        IntermediateKind::ArrayLiteral {
            items,
            element_type,
            field_names,
        } => IntermediateKind::ArrayLiteral {
            items: items
                .iter()
                .map(|item| resolve_inline_constants(item, inline_bindings))
                .collect(),
            element_type: element_type.clone(),
            field_names: field_names.clone(),
        },
        IntermediateKind::BoxAlloc {
            value,
            element_type,
        } => IntermediateKind::BoxAlloc {
            value: Box::new(resolve_inline_constants(value, inline_bindings)),
            element_type: element_type.clone(),
        },
        IntermediateKind::If {
            condition,
            then_branch,
            else_branch,
        } => IntermediateKind::If {
            condition: Box::new(resolve_inline_constants(condition, inline_bindings)),
            then_branch: Box::new(resolve_inline_constants(then_branch, inline_bindings)),
            else_branch: Box::new(resolve_inline_constants(else_branch, inline_bindings)),
        },
        IntermediateKind::Assignment { target, expr } => IntermediateKind::Assignment {
            target: target.clone(),
            expr: Box::new(resolve_inline_constants(expr, inline_bindings)),
        },
        IntermediateKind::FunctionCall { function, argument } => IntermediateKind::FunctionCall {
            function: *function,
            argument: Box::new(resolve_inline_constants(argument, inline_bindings)),
        },
        IntermediateKind::ArrayIndex { array, index } => IntermediateKind::ArrayIndex {
            array: Box::new(resolve_inline_constants(array, inline_bindings)),
            index: Box::new(resolve_inline_constants(index, inline_bindings)),
        },
        IntermediateKind::TypePropertyAccess { object, property } => {
            IntermediateKind::TypePropertyAccess {
                object: Box::new(resolve_inline_constants(object, inline_bindings)),
                property: property.clone(),
            }
        }
        IntermediateKind::Binding(binding) => {
            IntermediateKind::Binding(Box::new(IntermediateBinding {
                identifier: binding.identifier.clone(),
                binding_type: binding.binding_type.clone(),
                expr: resolve_inline_constants(&binding.expr, inline_bindings),
            }))
        }
        IntermediateKind::Block(items) => IntermediateKind::Block(
            items
                .iter()
                .map(|item| resolve_inline_constants(item, inline_bindings))
                .collect(),
        ),
        IntermediateKind::Diverge {
            value,
            divergance_type,
        } => IntermediateKind::Diverge {
            value: Box::new(resolve_inline_constants(value, inline_bindings)),
            divergance_type: divergance_type.clone(),
        },
        IntermediateKind::Loop { body } => IntermediateKind::Loop {
            body: Box::new(resolve_inline_constants(body, inline_bindings)),
        },
        IntermediateKind::IntrinsicOperation(op) => {
            IntermediateKind::IntrinsicOperation(match op {
                IntermediateIntrinsicOperation::Binary(left, right, operator) => {
                    IntermediateIntrinsicOperation::Binary(
                        Box::new(resolve_inline_constants(left, inline_bindings)),
                        Box::new(resolve_inline_constants(right, inline_bindings)),
                        operator.clone(),
                    )
                }
                IntermediateIntrinsicOperation::Unary(operand, operator) => {
                    IntermediateIntrinsicOperation::Unary(
                        Box::new(resolve_inline_constants(operand, inline_bindings)),
                        operator.clone(),
                    )
                }
            })
        }
        IntermediateKind::InlineAssembly { target, code } => IntermediateKind::InlineAssembly {
            target: target.clone(),
            code: code.clone(),
        },
        IntermediateKind::Literal(literal) => IntermediateKind::Literal(literal.clone()),
        IntermediateKind::Unreachable => IntermediateKind::Unreachable,
    }
}

pub fn compile_exports(intermediate: &IntermediateResult) -> Result<Vec<u8>, Diagnostic> {
    let exports = collect_wasm_exports(intermediate)?;
    if exports.imports.is_empty() && exports.functions.is_empty() && exports.globals.is_empty() {
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
    let mut import_section = ImportSection::new();
    let mut function_section = FunctionSection::new();
    let mut global_section = GlobalSection::new();
    let mut export_section = ExportSection::new();
    let mut code_section = CodeSection::new();
    let mut box_registry = BoxRegistry::default();
    let mut global_box_bindings: std::collections::HashMap<String, BoxInfo> =
        std::collections::HashMap::new();

    let mut type_ctx = TypeContext::new();

    let function_return_types = intermediate
        .functions
        .iter()
        .map(|function| intermediate_type_to_wasm(&function.return_type))
        .collect::<Vec<_>>();

    for function in &all_functions {
        let mut locals_types = std::collections::HashMap::new();
        for param in &function.params {
            if !matches!(
                param.ty,
                WasmType::I32 | WasmType::U8 | WasmType::Box { .. }
            ) {
                type_ctx.get_or_register_type(param.ty.clone());
            }
            locals_types.insert(param.name.clone(), param.ty.clone());
        }
        for global in &exports.globals {
            if matches!(global.ty, WasmType::Box { .. }) {
                locals_types.insert(global.name.clone(), global.ty.clone());
            }
        }
        if !matches!(
            function.return_type,
            WasmType::I32 | WasmType::U8 | WasmType::Box { .. }
        ) {
            type_ctx.get_or_register_type(function.return_type.clone());
        }
        collect_types(
            &function.body,
            &mut type_ctx,
            &mut locals_types,
            &function_return_types,
        )?;
    }

    for global in &exports.globals {
        if !matches!(
            global.ty,
            WasmType::I32 | WasmType::U8 | WasmType::Box { .. }
        ) {
            type_ctx.get_or_register_type(global.ty.clone());
        }
        if let WasmType::Box { element } = &global.ty {
            if !matches!(
                element.as_ref(),
                WasmType::I32 | WasmType::U8 | WasmType::Box { .. }
            ) {
                type_ctx.get_or_register_type(element.as_ref().clone());
            }
        }
    }

    for import in &exports.imports {
        for param in &import.params {
            if !matches!(
                param.ty,
                WasmType::I32 | WasmType::U8 | WasmType::Box { .. }
            ) {
                type_ctx.get_or_register_type(param.ty.clone());
            }
        }
        if !matches!(
            import.return_type,
            WasmType::I32 | WasmType::U8 | WasmType::Box { .. }
        ) {
            type_ctx.get_or_register_type(import.return_type.clone());
        }
    }

    for ty in &type_ctx.types {
        let composite_type = match ty {
            WasmType::Struct(fields) => {
                let wasm_fields = fields.iter().map(|(_, ty)| {
                    let storage_type = match ty {
                        WasmType::U8 => StorageType::I8,
                        _ => StorageType::Val(ty.to_val_type(&type_ctx)),
                    };
                    FieldType {
                        element_type: storage_type,
                        mutable: true,
                    }
                });
                CompositeInnerType::Struct(StructType {
                    fields: wasm_fields.collect::<Vec<_>>().into_boxed_slice(),
                })
            }
            WasmType::Array { element, .. } => {
                let storage_type = match element.as_ref() {
                    WasmType::U8 => StorageType::I8,
                    _ => StorageType::Val(element.to_val_type(&type_ctx)),
                };
                CompositeInnerType::Array(ArrayType(FieldType {
                    element_type: storage_type,
                    mutable: true,
                }))
            }
            WasmType::I32 | WasmType::U8 | WasmType::Box { .. } => {
                continue;
            }
        };

        type_section.rec(vec![SubType {
            is_final: false,
            supertype_idx: None,
            composite_type: CompositeType {
                inner: composite_type,
                shared: false,
            },
        }]);
    }

    let composite_type_count = type_ctx.types.len() as u32;
    let mut next_type_index = composite_type_count;

    for import in &exports.imports {
        let param_types: Vec<ValType> = import
            .params
            .iter()
            .map(|param| param.ty.to_val_type(&type_ctx))
            .collect();
        let return_types = vec![import.return_type.to_val_type(&type_ctx)];
        type_section.function(param_types, return_types);
        import_section.import(
            "silk",
            &import.import_name,
            EntityType::Function(next_type_index),
        );
        next_type_index += 1;
    }

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
        if matches!(global.ty, WasmType::Box { .. }) {
            let IntermediateType::Box { element } = &global.intermediate_ty else {
                return Err(Diagnostic::new(
                    "Box export missing intermediate element type".to_string(),
                )
                .with_span(SourceSpan::default()));
            };
            let (init_value, element_type) = match &global.init {
                IntermediateKind::BoxAlloc {
                    value,
                    element_type,
                } => (value.as_ref(), element_type),
                other => (other, element.as_ref()),
            };
            let resolved_init = resolve_inline_constants(init_value, &intermediate.inline_bindings);
            let info = box_registry.register_box(
                &resolved_init,
                element_type,
                Some(global.name.clone()),
            )?;
            global_box_bindings.insert(global.name.clone(), info);
            continue;
        }
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

    let function_index_offset = exports.imports.len() as u32;

    for function in &all_functions {
        let mut locals = Vec::new();
        let mut local_indices = std::collections::HashMap::new();
        let mut locals_types = std::collections::HashMap::new();

        for (j, param) in function.params.iter().enumerate() {
            local_indices.insert(param.name.clone(), j as u32);
            locals_types.insert(param.name.clone(), param.ty.clone());
        }
        for global in &exports.globals {
            if matches!(global.ty, WasmType::Box { .. }) {
                locals_types.insert(global.name.clone(), global.ty.clone());
            }
        }

        let mut match_counter = MatchCounter::default();
        let body_locals = collect_locals(
            &function.body,
            &mut locals_types,
            &function_return_types,
            &intermediate.functions,
            &mut match_counter,
        )?;
        for (name, ty) in body_locals {
            if !local_indices.contains_key(&name) {
                local_indices.insert(name.clone(), (function.params.len() + locals.len()) as u32);
                locals.push(ty.clone());
            }
        }
        for ty in &locals {
            if !matches!(ty, WasmType::I32 | WasmType::U8 | WasmType::Box { .. }) {
                type_ctx.get_or_register_type(ty.clone());
            }
        }

        let box_temp_local = ensure_box_temp_local(
            &mut locals,
            &mut locals_types,
            &mut local_indices,
            function.params.len(),
        );
        let box_base_local = ensure_box_temp_local(
            &mut locals,
            &mut locals_types,
            &mut local_indices,
            function.params.len(),
        );

        let mut func = Function::new(locals.iter().map(|ty| (1, ty.to_val_type(&type_ctx))));

        let mut control_stack = Vec::new();
        let mut loop_stack = Vec::new();

        let mut match_counter = MatchCounter::default();
        let mut box_ctx = BoxContext::default();
        for (name, info) in &global_box_bindings {
            box_ctx.bindings.insert(name.clone(), info.clone());
        }
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
            &function.return_type,
            &mut box_ctx,
            &mut box_registry,
            Some(box_temp_local),
            Some(box_base_local),
            function_index_offset,
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

    let (memory_section, data_section) = if box_registry.memories.is_empty() {
        (None, None)
    } else {
        let mut memory_section = MemorySection::new();
        let mut data_section = DataSection::new();
        let mut memories = box_registry.memories.clone();
        memories.sort_by_key(|memory| memory.memory_index);
        for memory in memories.iter() {
            let minimum_pages = std::cmp::max(1u64, pages_for_size(memory.bytes.len()));
            memory_section.memory(MemoryType {
                minimum: minimum_pages,
                maximum: None,
                memory64: false,
                shared: false,
                page_size_log2: None,
            });
            let offset_expr = ConstExpr::i32_const(0);
            data_section.active(memory.memory_index, &offset_expr, memory.bytes.clone());
        }
        (Some(memory_section), Some(data_section))
    };

    module.section(&type_section);
    if !exports.imports.is_empty() {
        module.section(&import_section);
    }
    module.section(&function_section);
    if let Some(memory_section) = &memory_section {
        module.section(memory_section);
    }
    if !exports.globals.is_empty() {
        module.section(&global_section);
    }
    let import_count = exports.imports.len() as u32;
    for (import_index, import) in exports.imports.iter().enumerate() {
        export_section.export(&import.export_name, ExportKind::Func, import_index as u32);
    }
    for export in &exports.functions {
        export_section.export(
            &export.name,
            ExportKind::Func,
            export.index as u32 + import_count,
        );
    }
    let mut global_index = 0;
    for global in &exports.globals {
        if matches!(global.ty, WasmType::Box { .. }) {
            continue;
        }
        if global.exported {
            export_section.export(&global.name, ExportKind::Global, global_index as u32);
        }
        global_index += 1;
    }
    for memory in &box_registry.memories {
        if let Some(name) = &memory.export_name {
            export_section.export(name, ExportKind::Memory, memory.memory_index);
        }
    }
    module.section(&export_section);
    module.section(&code_section);
    if let Some(data_section) = &data_section {
        module.section(data_section);
    }

    Ok(module.finish())
}

fn collect_wasm_exports(intermediate: &IntermediateResult) -> Result<WasmExports, Diagnostic> {
    let mut imports = Vec::new();
    let mut functions = Vec::new();
    let mut globals = Vec::new();
    let mut exported_globals = std::collections::HashSet::new();

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
                let _global = intermediate.globals.get(export.index).ok_or_else(|| {
                    Diagnostic::new(format!(
                        "Export `{}` references unknown global index {}",
                        export.name, export.index
                    ))
                    .with_span(SourceSpan::default())
                })?;
                exported_globals.insert(export.index);
            }
        }
    }

    for wrap in &intermediate.wrappers {
        if wrap.wrap_target != TargetLiteral::WasmTarget {
            continue;
        }
        match wrap.export_type {
            IntermediateExportType::Function => {
                let function = intermediate.functions.get(wrap.index).ok_or_else(|| {
                    Diagnostic::new(format!(
                        "Wrap `{}` references unknown function index {}",
                        wrap.name, wrap.index
                    ))
                    .with_span(SourceSpan::default())
                })?;
                let params = extract_function_params(&function.parameter, &function.input_type)?;
                let return_type = intermediate_type_to_wasm(&function.return_type);
                imports.push(WasmFunctionImport {
                    import_name: format!("__silk_wrap_{}", wrap.name),
                    export_name: wrap.name.clone(),
                    params,
                    return_type,
                });
            }
            IntermediateExportType::Global => {
                return Err(Diagnostic::new(
                    "wrap annotation does not support globals for wasm target",
                )
                .with_span(SourceSpan::default()));
            }
        }
    }

    imports.sort_by(|a, b| a.export_name.cmp(&b.export_name));
    functions.sort_by(|a, b| a.name.cmp(&b.name));
    for (index, global) in intermediate.globals.iter().enumerate() {
        globals.push(WasmGlobalExport {
            name: global.name.clone(),
            ty: intermediate_type_to_wasm(&global.ty),
            init: global.value.clone(),
            intermediate_ty: global.ty.clone(),
            exported: exported_globals.contains(&index),
        });
    }

    globals.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(WasmExports {
        imports,
        functions,
        globals,
    })
}

fn const_expr_for_global(expr: &IntermediateKind, ty: &WasmType) -> Result<ConstExpr, Diagnostic> {
    match (ty, expr) {
        (WasmType::I32, IntermediateKind::Literal(ExpressionLiteral::Number(value))) => {
            Ok(ConstExpr::i32_const(*value))
        }
        (WasmType::U8, IntermediateKind::Literal(ExpressionLiteral::Number(value))) => {
            Ok(ConstExpr::i32_const(value & 0xFF))
        }
        (WasmType::I32, IntermediateKind::Literal(ExpressionLiteral::Char(value))) => {
            Ok(ConstExpr::i32_const(i32::from(*value)))
        }
        (WasmType::U8, IntermediateKind::Literal(ExpressionLiteral::Char(value))) => {
            Ok(ConstExpr::i32_const(i32::from(*value)))
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
                IntermediateKind::ArrayLiteral { items, .. } => {
                    for value in items.iter().rev() {
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
                IntermediateKind::BoxAlloc { value, .. } => {
                    stack.push((value, false));
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
                IntermediateKind::TypePropertyAccess { object, .. } => {
                    stack.push((object, false));
                }
                IntermediateKind::ArrayIndex { array, index } => {
                    stack.push((index, false));
                    stack.push((array, false));
                }
                _ => {}
            }
            continue;
        }

        match node {
            IntermediateKind::Struct(_fields) => {
                let ty = infer_type(node, locals_types, function_return_types)?;
                if !matches!(ty, WasmType::I32 | WasmType::U8) {
                    ctx.get_or_register_type(ty);
                }
            }
            IntermediateKind::ArrayLiteral {
                element_type,
                field_names,
                items,
            } => {
                let element_wasm = intermediate_type_to_wasm(element_type);
                let ty = WasmType::Array {
                    element: Box::new(element_wasm),
                    length: items.len(),
                    field_names: field_names.clone(),
                };
                ctx.get_or_register_type(ty);
            }
            IntermediateKind::Binding(binding) => {
                let binding = binding.as_ref();
                let ty = intermediate_type_to_wasm(&binding.binding_type);
                if !matches!(ty, WasmType::I32 | WasmType::U8) {
                    ctx.get_or_register_type(ty.clone());
                }
                let mut locals = Vec::new();
                collect_locals_for_pattern(binding, ty, locals_types, &mut locals)?;
            }
            IntermediateKind::BoxAlloc { element_type, .. } => {
                let element_wasm = intermediate_type_to_wasm(element_type);
                if !matches!(
                    element_wasm,
                    WasmType::I32 | WasmType::U8 | WasmType::Box { .. }
                ) {
                    ctx.get_or_register_type(element_wasm);
                }
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
        let normalized = strip_box_owned(first_type.clone());
        let mut mismatched = None;
        for ty in &break_types {
            if !wasm_types_equivalent(&normalized, ty) {
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

        Ok(Some(normalized))
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

fn parse_inline_wasm(code: &str) -> Result<Vec<Instruction<'static>>, Diagnostic> {
    let mut instructions = Vec::new();
    for raw in code.split(|c| c == ';' || c == '\n' || c == '\r') {
        let trimmed = raw.trim();
        if trimmed.is_empty() {
            continue;
        }
        let mut parts = trimmed.split_whitespace();
        let op = parts
            .next()
            .ok_or_else(|| Diagnostic::new("asm instruction cannot be empty".to_string()))?;
        match op {
            "local.get" => {
                let index = parts
                    .next()
                    .ok_or_else(|| {
                        Diagnostic::new("asm local.get expects a local index".to_string())
                    })?
                    .parse::<u32>()
                    .map_err(|_| {
                        Diagnostic::new("asm local.get expects a valid u32".to_string())
                    })?;
                if parts.next().is_some() {
                    return Err(Diagnostic::new(
                        "asm local.get expects a single index value".to_string(),
                    ));
                }
                instructions.push(Instruction::LocalGet(index));
            }
            "local.set" => {
                let index = parts
                    .next()
                    .ok_or_else(|| {
                        Diagnostic::new("asm local.set expects a local index".to_string())
                    })?
                    .parse::<u32>()
                    .map_err(|_| {
                        Diagnostic::new("asm local.set expects a valid u32".to_string())
                    })?;
                if parts.next().is_some() {
                    return Err(Diagnostic::new(
                        "asm local.set expects a single index value".to_string(),
                    ));
                }
                instructions.push(Instruction::LocalSet(index));
            }
            "i32.const" => {
                let value = parts
                    .next()
                    .ok_or_else(|| {
                        Diagnostic::new("asm i32.const expects an immediate value".to_string())
                    })?
                    .parse::<i32>()
                    .map_err(|_| {
                        Diagnostic::new("asm i32.const expects a valid i32".to_string())
                    })?;
                if parts.next().is_some() {
                    return Err(Diagnostic::new(
                        "asm i32.const expects a single immediate value".to_string(),
                    ));
                }
                instructions.push(Instruction::I32Const(value));
            }
            "i32.add" => {
                if parts.next().is_some() {
                    return Err(Diagnostic::new("asm i32.add takes no operands".to_string()));
                }
                instructions.push(Instruction::I32Add);
            }
            "i32.sub" => {
                if parts.next().is_some() {
                    return Err(Diagnostic::new("asm i32.sub takes no operands".to_string()));
                }
                instructions.push(Instruction::I32Sub);
            }
            "i32.mul" => {
                if parts.next().is_some() {
                    return Err(Diagnostic::new("asm i32.mul takes no operands".to_string()));
                }
                instructions.push(Instruction::I32Mul);
            }
            "i32.div_s" => {
                if parts.next().is_some() {
                    return Err(Diagnostic::new(
                        "asm i32.div_s takes no operands".to_string(),
                    ));
                }
                instructions.push(Instruction::I32DivS);
            }
            "i32.eq" => {
                if parts.next().is_some() {
                    return Err(Diagnostic::new("asm i32.eq takes no operands".to_string()));
                }
                instructions.push(Instruction::I32Eq);
            }
            "i32.ne" => {
                if parts.next().is_some() {
                    return Err(Diagnostic::new("asm i32.ne takes no operands".to_string()));
                }
                instructions.push(Instruction::I32Ne);
            }
            "i32.lt_s" => {
                if parts.next().is_some() {
                    return Err(Diagnostic::new(
                        "asm i32.lt_s takes no operands".to_string(),
                    ));
                }
                instructions.push(Instruction::I32LtS);
            }
            "i32.gt_s" => {
                if parts.next().is_some() {
                    return Err(Diagnostic::new(
                        "asm i32.gt_s takes no operands".to_string(),
                    ));
                }
                instructions.push(Instruction::I32GtS);
            }
            "i32.le_s" => {
                if parts.next().is_some() {
                    return Err(Diagnostic::new(
                        "asm i32.le_s takes no operands".to_string(),
                    ));
                }
                instructions.push(Instruction::I32LeS);
            }
            "i32.ge_s" => {
                if parts.next().is_some() {
                    return Err(Diagnostic::new(
                        "asm i32.ge_s takes no operands".to_string(),
                    ));
                }
                instructions.push(Instruction::I32GeS);
            }
            "i32.eqz" => {
                if parts.next().is_some() {
                    return Err(Diagnostic::new("asm i32.eqz takes no operands".to_string()));
                }
                instructions.push(Instruction::I32Eqz);
            }
            other => {
                return Err(Diagnostic::new(format!(
                    "Unsupported asm instruction `{other}`"
                )));
            }
        }
    }

    if instructions.is_empty() {
        return Err(Diagnostic::new(
            "asm string must contain at least one instruction".to_string(),
        ));
    }

    Ok(instructions)
}

fn infer_type(
    expr: &IntermediateKind,
    locals_types: &std::collections::HashMap<String, WasmType>,
    function_return_types: &[WasmType],
) -> Result<WasmType, Diagnostic> {
    infer_type_impl(expr, locals_types, function_return_types)
}

fn intrinsic_binary_result_type(
    op: &BinaryIntrinsicOperator,
    left: &WasmType,
    right: &WasmType,
) -> WasmType {
    let left = strip_box_type(left);
    let right = strip_box_type(right);
    let is_u8 = matches!(left, WasmType::U8) && matches!(right, WasmType::U8);
    match op {
        BinaryIntrinsicOperator::I32Add
        | BinaryIntrinsicOperator::I32Subtract
        | BinaryIntrinsicOperator::I32Multiply
        | BinaryIntrinsicOperator::I32Divide => {
            if is_u8 {
                WasmType::U8
            } else {
                WasmType::I32
            }
        }
        BinaryIntrinsicOperator::I32Equal
        | BinaryIntrinsicOperator::I32NotEqual
        | BinaryIntrinsicOperator::I32LessThan
        | BinaryIntrinsicOperator::I32GreaterThan
        | BinaryIntrinsicOperator::I32LessThanOrEqual
        | BinaryIntrinsicOperator::I32GreaterThanOrEqual
        | BinaryIntrinsicOperator::BooleanAnd
        | BinaryIntrinsicOperator::BooleanOr
        | BinaryIntrinsicOperator::BooleanXor => WasmType::I32,
    }
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
        FinishTypePropertyAccess { property: String },
        FinishArrayIndex,
        FinishIntrinsicBinary { op: BinaryIntrinsicOperator },
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
                IntermediateKind::Literal(ExpressionLiteral::Char(_)) => {
                    results.push(WasmType::U8);
                }
                IntermediateKind::Struct(fields) => {
                    let field_names = fields.iter().map(|(name, _)| name.name.clone()).collect();
                    stack.push(InferTask::FinishStruct { field_names });
                    for (_, value) in fields.into_iter().rev() {
                        stack.push(InferTask::Eval(value));
                    }
                }
                IntermediateKind::ArrayLiteral {
                    items,
                    element_type,
                    field_names,
                } => {
                    let wasm_element = intermediate_type_to_wasm(&element_type);
                    results.push(WasmType::Array {
                        element: Box::new(wasm_element),
                        length: items.len(),
                        field_names,
                    });
                }
                IntermediateKind::BoxAlloc { element_type, .. } => {
                    let element = intermediate_type_to_wasm(&element_type);
                    results.push(WasmType::Box {
                        element: Box::new(element),
                    });
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
                IntermediateKind::TypePropertyAccess { object, property } => {
                    stack.push(InferTask::FinishTypePropertyAccess { property });
                    stack.push(InferTask::Eval((*object).clone()));
                }
                IntermediateKind::ArrayIndex { array, index } => {
                    stack.push(InferTask::FinishArrayIndex);
                    stack.push(InferTask::Eval((*index).clone()));
                    stack.push(InferTask::Eval((*array).clone()));
                }
                IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Binary(
                    left,
                    right,
                    op,
                )) => {
                    stack.push(InferTask::FinishIntrinsicBinary { op });
                    stack.push(InferTask::Eval((*right).clone()));
                    stack.push(InferTask::Eval((*left).clone()));
                }
                IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Unary(..))
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
                let mut ordered_names = Vec::with_capacity(field_names.len());
                for name in field_names.into_iter().rev() {
                    let ty = results
                        .pop()
                        .expect("infer_type_basic should have field types");
                    ordered_names.push(name);
                    field_types.push(ty);
                }
                field_types.reverse();
                ordered_names.reverse();
                results.push(struct_fields_to_wasm_type(ordered_names, field_types));
            }
            InferTask::FinishAssignment => {
                let existing_type = results
                    .pop()
                    .expect("infer_type_basic should have assignment target type");
                let value_type = results
                    .pop()
                    .expect("infer_type_basic should have assignment value type");

                if !wasm_types_equivalent(&value_type, &existing_type) {
                    return Err(Diagnostic::new(
                        "Cannot assign value of different type to target".to_string(),
                    )
                    .with_span(SourceSpan::default()));
                }
                results.push(value_type);
            }
            InferTask::FinishTypePropertyAccess { property } => {
                let object_type = results
                    .pop()
                    .expect("infer_type_basic should have object type");
                match strip_box_owned(object_type) {
                    WasmType::Struct(fields) => {
                        if let Some((_, field_type)) = fields.iter().find(|(n, _)| n == &property) {
                            results.push(field_type.clone());
                        } else {
                            return Err(Diagnostic::new(format!(
                                "Field `{}` not found in struct",
                                property
                            ))
                            .with_span(SourceSpan::default()));
                        }
                    }
                    WasmType::Array {
                        element,
                        field_names,
                        ..
                    } => {
                        if field_names.iter().any(|name| name == &property) {
                            results.push(*element);
                        } else {
                            return Err(Diagnostic::new(format!(
                                "Field `{}` not found in array",
                                property
                            ))
                            .with_span(SourceSpan::default()));
                        }
                    }
                    WasmType::I32 | WasmType::U8 => {
                        return Err(Diagnostic::new("Type property access on non-struct type")
                            .with_span(SourceSpan::default()));
                    }
                    WasmType::Box { .. } => {
                        return Err(Diagnostic::new("Nested box types are not supported")
                            .with_span(SourceSpan::default()));
                    }
                }
            }
            InferTask::FinishArrayIndex => {
                let _index_type = results
                    .pop()
                    .expect("infer_type_basic should have index type");
                let array_type = results
                    .pop()
                    .expect("infer_type_basic should have array type");
                if let WasmType::Array { element, .. } = strip_box_owned(array_type) {
                    results.push(*element);
                } else {
                    return Err(Diagnostic::new("Indexing on non-array type")
                        .with_span(SourceSpan::default()));
                }
            }
            InferTask::FinishIntrinsicBinary { op } => {
                let right_type = results
                    .pop()
                    .expect("infer_type_basic should have intrinsic right type");
                let left_type = results
                    .pop()
                    .expect("infer_type_basic should have intrinsic left type");
                results.push(intrinsic_binary_result_type(&op, &left_type, &right_type));
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
        FinishTypePropertyAccess { property: String },
        FinishArrayIndex,
        FinishLoop { body: IntermediateKind },
        FinishIntrinsicBinary { op: BinaryIntrinsicOperator },
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
                IntermediateKind::Literal(ExpressionLiteral::Char(_)) => {
                    results.push(WasmType::U8);
                }
                IntermediateKind::Struct(fields) => {
                    let field_names = fields.iter().map(|(name, _)| name.name.clone()).collect();
                    stack.push(InferTask::FinishStruct { field_names });
                    for (_, value) in fields.into_iter().rev() {
                        stack.push(InferTask::Eval(value));
                    }
                }
                IntermediateKind::ArrayLiteral {
                    items,
                    element_type,
                    field_names,
                } => {
                    let wasm_element = intermediate_type_to_wasm(&element_type);
                    results.push(WasmType::Array {
                        element: Box::new(wasm_element),
                        length: items.len(),
                        field_names,
                    });
                }
                IntermediateKind::BoxAlloc { element_type, .. } => {
                    let element = intermediate_type_to_wasm(&element_type);
                    results.push(WasmType::Box {
                        element: Box::new(element),
                    });
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
                IntermediateKind::TypePropertyAccess { object, property } => {
                    stack.push(InferTask::FinishTypePropertyAccess { property });
                    stack.push(InferTask::Eval((*object).clone()));
                }
                IntermediateKind::ArrayIndex { array, index } => {
                    stack.push(InferTask::FinishArrayIndex);
                    stack.push(InferTask::Eval((*index).clone()));
                    stack.push(InferTask::Eval((*array).clone()));
                }
                IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Binary(
                    left,
                    right,
                    op,
                )) => {
                    stack.push(InferTask::FinishIntrinsicBinary { op });
                    stack.push(InferTask::Eval((*right).clone()));
                    stack.push(InferTask::Eval((*left).clone()));
                }
                IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Unary(..))
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
                let mut ordered_names = Vec::with_capacity(field_names.len());
                for name in field_names.into_iter().rev() {
                    let ty = results
                        .pop()
                        .expect("infer_type_impl should have field types");
                    ordered_names.push(name);
                    field_types.push(ty);
                }
                field_types.reverse();
                ordered_names.reverse();
                results.push(struct_fields_to_wasm_type(ordered_names, field_types));
            }
            InferTask::FinishAssignment => {
                let existing_type = results
                    .pop()
                    .expect("infer_type_impl should have assignment target type");
                let value_type = results
                    .pop()
                    .expect("infer_type_impl should have assignment value type");

                if !wasm_types_equivalent(&value_type, &existing_type) {
                    return Err(Diagnostic::new(
                        "Cannot assign value of different type to target".to_string(),
                    )
                    .with_span(SourceSpan::default()));
                }
                results.push(value_type);
            }
            InferTask::FinishTypePropertyAccess { property } => {
                let object_type = results
                    .pop()
                    .expect("infer_type_impl should have object type");
                match strip_box_owned(object_type) {
                    WasmType::Struct(fields) => {
                        if let Some((_, field_type)) = fields.iter().find(|(n, _)| n == &property) {
                            results.push(field_type.clone());
                        } else {
                            return Err(Diagnostic::new(format!(
                                "Field `{}` not found in struct",
                                property
                            ))
                            .with_span(SourceSpan::default()));
                        }
                    }
                    WasmType::Array {
                        element,
                        field_names,
                        ..
                    } => {
                        if field_names.iter().any(|name| name == &property) {
                            results.push(*element);
                        } else {
                            return Err(Diagnostic::new(format!(
                                "Field `{}` not found in array",
                                property
                            ))
                            .with_span(SourceSpan::default()));
                        }
                    }
                    WasmType::I32 | WasmType::U8 => {
                        return Err(Diagnostic::new("Type property access on non-struct type")
                            .with_span(SourceSpan::default()));
                    }
                    WasmType::Box { .. } => {
                        return Err(Diagnostic::new("Nested box types are not supported")
                            .with_span(SourceSpan::default()));
                    }
                }
            }
            InferTask::FinishArrayIndex => {
                let _index_type = results
                    .pop()
                    .expect("infer_type_impl should have index type");
                let array_type = results
                    .pop()
                    .expect("infer_type_impl should have array type");
                if let WasmType::Array { element, .. } = strip_box_owned(array_type) {
                    results.push(*element);
                } else {
                    return Err(Diagnostic::new("Indexing on non-array type")
                        .with_span(SourceSpan::default()));
                }
            }
            InferTask::FinishIntrinsicBinary { op } => {
                let right_type = results
                    .pop()
                    .expect("infer_type_impl should have intrinsic right type");
                let left_type = results
                    .pop()
                    .expect("infer_type_impl should have intrinsic left type");
                results.push(intrinsic_binary_result_type(&op, &left_type, &right_type));
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
    fn struct_field_types(
        current_type: &IntermediateType,
        span: SourceSpan,
    ) -> Result<Vec<(String, IntermediateType)>, Diagnostic> {
        match current_type {
            IntermediateType::Struct(field_types) => Ok(field_types.clone()),
            IntermediateType::Array {
                element,
                field_names,
                ..
            } => Ok(field_names
                .iter()
                .cloned()
                .map(|name| (name, (*element.clone())))
                .collect()),
            _ => Err(
                Diagnostic::new("Struct pattern requires struct parameter type").with_span(span),
            ),
        }
    }

    let mut params = Vec::new();
    let mut stack = vec![(pattern, ty.clone())];

    while let Some((current_pattern, current_type)) = stack.pop() {
        match stripped_binding_pattern(current_pattern) {
            BindingPattern::Struct(fields, span) => {
                let field_types = struct_field_types(&current_type, *span)?;

                for (field_id, sub_pattern) in fields.iter().rev() {
                    let field_ty = field_types
                        .iter()
                        .find(|(name, _)| name == &field_id.name)
                        .map(|(_, ty)| ty.clone())
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
                ty: intermediate_type_to_wasm(&current_type),
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
    functions: &[IntermediateFunction],
    match_counter: &mut MatchCounter,
) -> Result<Vec<(String, WasmType)>, Diagnostic> {
    fn collect_lvalue_exprs<'a>(
        target: &'a IntermediateLValue,
        stack: &mut Vec<&'a IntermediateKind>,
    ) {
        match target {
            IntermediateLValue::Identifier(_, _) => {}
            IntermediateLValue::TypePropertyAccess { object, .. } => {
                collect_lvalue_exprs(object, stack);
            }
            IntermediateLValue::ArrayIndex { array, index, .. } => {
                stack.push(index);
                collect_lvalue_exprs(array, stack);
            }
        }
    }

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
                let mut allow_non_local = false;
                if let IntermediateLValue::ArrayIndex { array, .. } = target {
                    let array_expr = lvalue_to_intermediate(array.as_ref());
                    let array_type = infer_type(&array_expr, locals_types, function_return_types)?;
                    if matches!(array_type, WasmType::Box { .. }) {
                        allow_non_local = true;
                    }
                }
                if !allow_non_local {
                    ensure_lvalue_local(target, locals_types, SourceSpan::default())?;
                }
                collect_lvalue_exprs(target, &mut stack);
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
            IntermediateKind::Struct(items) => {
                for (_, field_expr) in items.iter().rev() {
                    stack.push(field_expr);
                }
            }
            IntermediateKind::ArrayLiteral { items, .. } => {
                for item in items.iter().rev() {
                    stack.push(item);
                }
            }
            IntermediateKind::BoxAlloc { value, .. } => {
                stack.push(value);
            }
            IntermediateKind::FunctionCall { function, argument } => {
                let callee = functions.get(*function).ok_or_else(|| {
                    Diagnostic::new("Unknown function call target".to_string())
                        .with_span(SourceSpan::default())
                })?;
                let arg_type = intermediate_type_to_wasm(&callee.input_type);
                let temp_local_name = match_counter.temp_for_function(*function);
                locals.push((temp_local_name.clone(), arg_type.clone()));
                locals_types.insert(temp_local_name, arg_type);
                stack.push(argument);
            }
            IntermediateKind::ArrayIndex { array, index } => {
                stack.push(index);
                stack.push(array);
            }
            IntermediateKind::TypePropertyAccess { object, .. } => {
                stack.push(object);
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
            IntermediateKind::Diverge { value, .. } => {
                stack.push(value);
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

fn ensure_box_temp_local(
    locals: &mut Vec<WasmType>,
    locals_types: &mut std::collections::HashMap<String, WasmType>,
    local_indices: &mut std::collections::HashMap<String, u32>,
    param_count: usize,
) -> u32 {
    let base = "__box_temp";
    let mut name = base.to_string();
    let mut suffix = 0;
    while locals_types.contains_key(&name) || local_indices.contains_key(&name) {
        name = format!("{base}_{suffix}");
        suffix += 1;
    }
    let index = (param_count + locals.len()) as u32;
    locals.push(WasmType::I32);
    locals_types.insert(name.clone(), WasmType::I32);
    local_indices.insert(name, index);
    index
}

fn flatten_call_arguments(
    pattern: &BindingPattern,
    input_type: &IntermediateType,
    argument_expr: IntermediateKind,
) -> Result<Vec<IntermediateKind>, Diagnostic> {
    fn struct_field_types(
        current_type: &IntermediateType,
        span: SourceSpan,
    ) -> Result<Vec<(String, IntermediateType)>, Diagnostic> {
        match current_type {
            IntermediateType::Struct(field_types) => Ok(field_types.clone()),
            IntermediateType::Array {
                element,
                field_names,
                ..
            } => Ok(field_names
                .iter()
                .cloned()
                .map(|name| (name, (*element.clone())))
                .collect()),
            _ => Err(
                Diagnostic::new("Struct pattern requires struct parameter type").with_span(span),
            ),
        }
    }

    let mut results = Vec::new();
    let mut stack = vec![(pattern, input_type.clone(), argument_expr)];

    while let Some((current_pattern, current_type, current_expr)) = stack.pop() {
        match stripped_binding_pattern(current_pattern) {
            BindingPattern::Identifier(_, _) => results.push(current_expr),
            BindingPattern::Struct(fields, span) => {
                let field_types = struct_field_types(&current_type, *span)?;

                for (field_id, sub_pattern) in fields.iter().rev() {
                    let field_ty = field_types
                        .iter()
                        .find(|(name, _)| name == &field_id.name)
                        .map(|(_, ty)| ty.clone())
                        .ok_or_else(|| {
                            Diagnostic::new(format!(
                                "Missing field {} in struct parameter type",
                                field_id.name
                            ))
                            .with_span(sub_pattern.span())
                        })?;
                    let field_expr = IntermediateKind::TypePropertyAccess {
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

fn emit_load_from_memory(
    func: &mut Function,
    ty: &WasmType,
    type_ctx: &TypeContext,
    temp_local: u32,
    base_offset: u32,
    memory_index: u32,
) -> Result<(), Diagnostic> {
    match ty {
        WasmType::I32 | WasmType::Box { .. } => {
            func.instruction(&Instruction::LocalGet(temp_local));
            func.instruction(&Instruction::I32Load(memarg(base_offset, 2, memory_index)));
        }
        WasmType::U8 => {
            func.instruction(&Instruction::LocalGet(temp_local));
            func.instruction(&Instruction::I32Load8U(memarg(
                base_offset,
                0,
                memory_index,
            )));
        }
        WasmType::Struct(fields) => {
            let sorted_fields = sorted_wasm_fields(fields);
            let mut offset = base_offset;
            for (_, field_ty) in &sorted_fields {
                emit_load_from_memory(func, field_ty, type_ctx, temp_local, offset, memory_index)?;
                offset = offset.saturating_add(wasm_type_size(field_ty) as u32);
            }
            let type_index = type_ctx
                .get_type_index(&WasmType::Struct(sorted_fields))
                .ok_or_else(|| {
                    Diagnostic::new("Struct type not found in context")
                        .with_span(SourceSpan::default())
                })?;
            func.instruction(&Instruction::StructNew(type_index));
        }
        WasmType::Array {
            element,
            length,
            field_names,
        } => {
            let array_type = WasmType::Array {
                element: element.clone(),
                length: *length,
                field_names: field_names.clone(),
            };
            let type_index = type_ctx.get_type_index(&array_type).ok_or_else(|| {
                Diagnostic::new("Array type not found in context").with_span(SourceSpan::default())
            })?;
            let element_size = wasm_type_size(element);
            for idx in 0..*length {
                let offset = base_offset.saturating_add((idx * element_size) as u32);
                emit_load_from_memory(func, element, type_ctx, temp_local, offset, memory_index)?;
            }
            func.instruction(&Instruction::ArrayNewFixed {
                array_type_index: type_index,
                array_size: *length as u32,
            });
        }
    }
    Ok(())
}

fn collect_boxed_store_leaves(
    value: &IntermediateKind,
    ty: &WasmType,
    base_offset: i32,
) -> Result<Vec<(IntermediateKind, WasmType, i32)>, Diagnostic> {
    fn struct_field_expr(
        value: &IntermediateKind,
        name: &str,
    ) -> Result<IntermediateKind, Diagnostic> {
        match value {
            IntermediateKind::Struct(items) => items
                .iter()
                .find(|(id, _)| id.name == name)
                .map(|(_, expr)| expr.clone())
                .ok_or_else(|| {
                    Diagnostic::new(format!("Missing field `{}` in struct literal", name))
                        .with_span(SourceSpan::default())
                }),
            _ => Ok(IntermediateKind::TypePropertyAccess {
                object: Box::new(value.clone()),
                property: name.to_string(),
            }),
        }
    }

    fn array_element_expr(
        value: &IntermediateKind,
        idx: usize,
        field_names: &[String],
    ) -> Result<IntermediateKind, Diagnostic> {
        match value {
            IntermediateKind::ArrayLiteral {
                items,
                field_names: literal_names,
                ..
            } => {
                let element = if literal_names.is_empty() {
                    items.get(idx).cloned()
                } else {
                    literal_names
                        .iter()
                        .position(|name| name == &field_names[idx])
                        .and_then(|pos| items.get(pos).cloned())
                }
                .ok_or_else(|| {
                    Diagnostic::new(format!(
                        "Missing field `{}` in struct literal",
                        field_names[idx]
                    ))
                    .with_span(SourceSpan::default())
                })?;
                Ok(element)
            }
            _ => Ok(IntermediateKind::ArrayIndex {
                array: Box::new(value.clone()),
                index: Box::new(IntermediateKind::Literal(ExpressionLiteral::Number(
                    idx as i32,
                ))),
            }),
        }
    }

    let mut results = Vec::new();
    match ty {
        WasmType::I32 | WasmType::U8 | WasmType::Box { .. } => {
            results.push((value.clone(), ty.clone(), base_offset));
        }
        WasmType::Struct(fields) => {
            let sorted_fields = sorted_wasm_fields(fields);
            let mut offset = base_offset;
            for (name, field_ty) in sorted_fields {
                let field_expr = struct_field_expr(value, &name)?;
                let mut nested = collect_boxed_store_leaves(&field_expr, &field_ty, offset)?;
                results.append(&mut nested);
                offset = offset.saturating_add(wasm_type_size(&field_ty) as i32);
            }
        }
        WasmType::Array {
            element,
            length,
            field_names,
        } => {
            let element_size = wasm_type_size(element) as i32;
            for idx in 0..*length {
                let element_expr = array_element_expr(value, idx, field_names)?;
                let offset = base_offset.saturating_add(idx as i32 * element_size);
                let mut nested = collect_boxed_store_leaves(&element_expr, element, offset)?;
                results.append(&mut nested);
            }
        }
    }
    Ok(results)
}

#[allow(clippy::too_many_arguments)]
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
    expected_return_type: &WasmType,
    box_ctx: &mut BoxContext,
    box_registry: &mut BoxRegistry,
    box_temp_local: Option<u32>,
    box_base_local: Option<u32>,
    function_index_offset: u32,
) -> Result<(), Diagnostic> {
    enum EmitTask<'a> {
        Eval(IntermediateKind),
        EvalValue(IntermediateKind),
        EvalWithType {
            expr: IntermediateKind,
            expected: WasmType,
        },
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
        FinishBoxArrayIndex {
            element: WasmType,
            memory_index: u32,
        },
    }

    let mut tasks = vec![EmitTask::EvalWithType {
        expr: expr.clone(),
        expected: expected_return_type.clone(),
    }];

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
                if !wasm_types_equivalent(&arg_type, &expected_type)
                    || (matches!(expected_type, WasmType::Box { .. })
                        && !matches!(arg_type, WasmType::Box { .. }))
                {
                    return Err(Diagnostic::new(
                        "Function call argument does not match function input type".to_string(),
                    )
                    .with_span(SourceSpan::default()));
                }
            }
            EmitTask::EvalValue(node) => {
                let expr_type = infer_type(&node, locals_types, function_return_types)?;
                match expr_type {
                    WasmType::Box { element } => {
                        let box_info = resolve_box_expr(&node, box_ctx, box_registry)?;
                        let temp_local = box_temp_local.ok_or_else(|| {
                            Diagnostic::new("Box load requires a temporary local".to_string())
                                .with_span(SourceSpan::default())
                        })?;
                        func.instruction(&Instruction::I32Const(0));
                        func.instruction(&Instruction::LocalSet(temp_local));
                        emit_load_from_memory(
                            func,
                            &element,
                            type_ctx,
                            temp_local,
                            0,
                            box_info.memory_index,
                        )?;
                    }
                    _ => tasks.push(EmitTask::Eval(node)),
                }
            }
            EmitTask::EvalWithType { expr, expected } => {
                let expr_type = infer_type(&expr, locals_types, function_return_types)?;
                match expected {
                    boxed @ WasmType::Box { .. } => {
                        if expr_type != boxed {
                            return Err(Diagnostic::new("Expected boxed value".to_string())
                                .with_span(SourceSpan::default()));
                        }
                        tasks.push(EmitTask::Eval(expr));
                    }
                    other => {
                        if !wasm_types_equivalent(&expr_type, &other) {
                            return Err(Diagnostic::new(format!(
                                "Expression type does not match expected type: expected `{}`, got `{}`",
                                format_wasm_type(&other),
                                format_wasm_type(&expr_type),
                            ))
                            .with_span(SourceSpan::default()));
                        }
                        if let WasmType::Box { element } = &expr_type {
                            let box_info = resolve_box_expr(&expr, box_ctx, box_registry)?;
                            let temp_local = box_temp_local.ok_or_else(|| {
                                Diagnostic::new("Box load requires a temporary local".to_string())
                                    .with_span(SourceSpan::default())
                            })?;
                            func.instruction(&Instruction::I32Const(0));
                            func.instruction(&Instruction::LocalSet(temp_local));
                            emit_load_from_memory(
                                func,
                                element.as_ref(),
                                type_ctx,
                                temp_local,
                                0,
                                box_info.memory_index,
                            )?;
                        } else if matches!(other, WasmType::U8)
                            && matches!(expr_type, WasmType::I32)
                        {
                            tasks.push(EmitTask::Instr(Instruction::I32And));
                            tasks.push(EmitTask::Instr(Instruction::I32Const(0xFF)));
                            tasks.push(EmitTask::Eval(expr));
                        } else {
                            tasks.push(EmitTask::Eval(expr));
                        }
                    }
                }
            }
            EmitTask::FinishBoxArrayIndex {
                element,
                memory_index,
            } => {
                let temp_local = box_temp_local.ok_or_else(|| {
                    Diagnostic::new("Box array access requires a temporary local".to_string())
                        .with_span(SourceSpan::default())
                })?;
                let element_size = wasm_type_size(&element) as i32;
                func.instruction(&Instruction::I32Const(element_size));
                func.instruction(&Instruction::I32Mul);
                func.instruction(&Instruction::LocalSet(temp_local));
                emit_load_from_memory(func, &element, type_ctx, temp_local, 0, memory_index)?;
            }
            EmitTask::Eval(node) => match node {
                IntermediateKind::Literal(ExpressionLiteral::Number(value)) => {
                    tasks.push(EmitTask::Instr(Instruction::I32Const(value)));
                }
                IntermediateKind::Literal(ExpressionLiteral::Char(value)) => {
                    tasks.push(EmitTask::Instr(Instruction::I32Const(i32::from(value))));
                }
                IntermediateKind::Literal(ExpressionLiteral::Boolean(value)) => {
                    tasks.push(EmitTask::Instr(Instruction::I32Const(if value {
                        1
                    } else {
                        0
                    })));
                }
                IntermediateKind::BoxAlloc { .. } => {
                    tasks.push(EmitTask::Instr(Instruction::I32Const(0)));
                }
                IntermediateKind::InlineAssembly { target, code } => {
                    if target != TargetLiteral::WasmTarget {
                        tasks.push(EmitTask::Instr(Instruction::Unreachable));
                    } else {
                        let source = String::from_utf8(code.clone()).map_err(|_| {
                            Diagnostic::new("asm string must be valid UTF-8".to_string())
                        })?;
                        let instructions = parse_inline_wasm(&source)?;
                        for instr in instructions.into_iter().rev() {
                            tasks.push(EmitTask::Instr(instr));
                        }
                    }
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
                } => match &target {
                    IntermediateLValue::Identifier(identifier, _) => {
                        let local_index =
                            locals.get(&identifier.name).copied().ok_or_else(|| {
                                Diagnostic::new(format!(
                                    "Identifier `{}` is not a local variable or parameter",
                                    identifier.name
                                ))
                                .with_span(SourceSpan::default())
                            })?;
                        let target_type =
                            locals_types.get(&identifier.name).cloned().ok_or_else(|| {
                                Diagnostic::new(format!(
                                    "Identifier `{}` is not a local variable or parameter",
                                    identifier.name
                                ))
                                .with_span(SourceSpan::default())
                            })?;
                        tasks.push(EmitTask::Instr(Instruction::LocalTee(local_index)));
                        tasks.push(EmitTask::EvalWithType {
                            expr: (*value).clone(),
                            expected: target_type,
                        });
                    }
                    IntermediateLValue::TypePropertyAccess {
                        object, property, ..
                    } => {
                        if let IntermediateLValue::ArrayIndex { array, index, .. } = object.as_ref()
                        {
                            let array_expr = lvalue_to_intermediate(array.as_ref());
                            let array_type =
                                infer_type(&array_expr, locals_types, function_return_types)?;
                            if let WasmType::Box { element } = array_type {
                                if let WasmType::Array { element, .. } = element.as_ref() {
                                    let index_type = infer_type(
                                        index.as_ref(),
                                        locals_types,
                                        function_return_types,
                                    )?;
                                    if strip_box_type(&index_type) != &WasmType::I32 {
                                        return Err(Diagnostic::new(
                                            "Array index must be i32".to_string(),
                                        )
                                        .with_span(SourceSpan::default()));
                                    }

                                    let temp_local = box_temp_local.ok_or_else(|| {
                                        Diagnostic::new(
                                            "Boxed array assignment requires a temporary local"
                                                .to_string(),
                                        )
                                        .with_span(SourceSpan::default())
                                    })?;
                                    let element_size = wasm_type_size(element) as i32;
                                    let memory_index =
                                        resolve_box_expr(&array_expr, box_ctx, box_registry)?
                                            .memory_index;

                                    let (field_offset, field_type) = match element.as_ref() {
                                        WasmType::Struct(fields) => {
                                            let sorted_fields = sorted_wasm_fields(fields);
                                            let mut offset = 0i32;
                                            let mut field_offset: Option<i32> = None;
                                            let mut field_type: Option<WasmType> = None;
                                            for (name, ty) in &sorted_fields {
                                                if name == property {
                                                    field_offset = Some(offset);
                                                    field_type = Some(ty.clone());
                                                    break;
                                                }
                                                offset = offset
                                                    .saturating_add(wasm_type_size(ty) as i32);
                                            }
                                            (
                                                field_offset.ok_or_else(|| {
                                                    Diagnostic::new(format!(
                                                        "Field `{}` not found in struct",
                                                        property
                                                    ))
                                                    .with_span(SourceSpan::default())
                                                })?,
                                                field_type.expect("Field type missing"),
                                            )
                                        }
                                        WasmType::Array {
                                            element: field_element,
                                            field_names,
                                            ..
                                        } => {
                                            let field_index = field_names
                                                .iter()
                                                .position(|name| name == property)
                                                .ok_or_else(|| {
                                                    Diagnostic::new(format!(
                                                        "Field `{}` not found in array",
                                                        property
                                                    ))
                                                    .with_span(SourceSpan::default())
                                                })?;
                                            let field_offset = field_index as i32
                                                * wasm_type_size(field_element) as i32;
                                            (field_offset, (**field_element).clone())
                                        }
                                        _ => {
                                            return Err(Diagnostic::new(
                                                "Property assignment on non-struct type"
                                                    .to_string(),
                                            )
                                            .with_span(SourceSpan::default()));
                                        }
                                    };

                                    let store_instr = match field_type {
                                        WasmType::U8 => {
                                            Instruction::I32Store8(memarg(0, 0, memory_index))
                                        }
                                        WasmType::I32 | WasmType::Box { .. } => {
                                            Instruction::I32Store(memarg(0, 2, memory_index))
                                        }
                                        _ => {
                                            return Err(Diagnostic::new(
                                                "Boxed struct field assignment only supports i32 or u8 fields"
                                                    .to_string(),
                                            )
                                            .with_span(SourceSpan::default()));
                                        }
                                    };
                                    let full_target_expr = lvalue_to_intermediate(&target);
                                    tasks.push(EmitTask::Eval(full_target_expr));
                                    tasks.push(EmitTask::Instr(store_instr));
                                    tasks.push(EmitTask::EvalWithType {
                                        expr: (*value).clone(),
                                        expected: field_type,
                                    });
                                    tasks.push(EmitTask::Instr(Instruction::I32Add));
                                    tasks
                                        .push(EmitTask::Instr(Instruction::I32Const(field_offset)));
                                    tasks.push(EmitTask::Instr(Instruction::LocalGet(temp_local)));
                                    tasks.push(EmitTask::Instr(Instruction::LocalSet(temp_local)));
                                    tasks.push(EmitTask::Instr(Instruction::I32Mul));
                                    tasks
                                        .push(EmitTask::Instr(Instruction::I32Const(element_size)));
                                    tasks.push(EmitTask::EvalValue((**index).clone()));
                                    continue;
                                }
                            }
                        }
                        let object_expr = lvalue_to_intermediate(object);
                        let object_type =
                            infer_type(&object_expr, locals_types, function_return_types)?;
                        match &object_type {
                            WasmType::Box { element } => match element.as_ref() {
                                WasmType::Struct(fields) => {
                                    let mut offset = 0i32;
                                    let mut field_type: Option<WasmType> = None;
                                    for (name, ty) in sorted_wasm_fields(fields) {
                                        if name == *property {
                                            field_type = Some(ty.clone());
                                            break;
                                        }
                                        offset = offset
                                            .saturating_add(wasm_type_size(&ty) as i32);
                                    }
                                    let field_type = field_type.ok_or_else(|| {
                                        Diagnostic::new(format!(
                                            "Field `{}` not found in struct",
                                            property
                                        ))
                                        .with_span(SourceSpan::default())
                                    })?;
                                    let memory_index =
                                        resolve_box_expr(&object_expr, box_ctx, box_registry)?
                                            .memory_index;
                                    let full_target_expr = lvalue_to_intermediate(&target);

                                    if matches!(
                                        &field_type,
                                        WasmType::U8 | WasmType::I32 | WasmType::Box { .. }
                                    ) {
                                        let temp_local = box_temp_local.ok_or_else(|| {
                                            Diagnostic::new(
                                                "Boxed struct assignment requires a temporary local"
                                                    .to_string(),
                                            )
                                            .with_span(SourceSpan::default())
                                        })?;
                                        let store_instr = match &field_type {
                                            WasmType::U8 => Instruction::I32Store8(memarg(
                                                0,
                                                0,
                                                memory_index,
                                            )),
                                            WasmType::I32 | WasmType::Box { .. } => {
                                                Instruction::I32Store(memarg(0, 2, memory_index))
                                            }
                                            _ => unreachable!(
                                                "non-leaf boxed struct assignment checked above"
                                            ),
                                        };
                                        tasks.push(EmitTask::Eval(full_target_expr));
                                        tasks.push(EmitTask::Instr(store_instr));
                                        tasks.push(EmitTask::EvalWithType {
                                            expr: (*value).clone(),
                                            expected: field_type.clone(),
                                        });
                                        tasks.push(EmitTask::Instr(Instruction::I32Add));
                                        tasks.push(EmitTask::Instr(Instruction::I32Const(offset)));
                                        tasks.push(EmitTask::Instr(Instruction::LocalGet(
                                            temp_local,
                                        )));
                                        tasks.push(EmitTask::Instr(Instruction::LocalSet(
                                            temp_local,
                                        )));
                                        tasks.push(EmitTask::Instr(Instruction::I32Const(0)));
                                    } else {
                                        let base_local = box_base_local.ok_or_else(|| {
                                            Diagnostic::new(
                                                "Boxed struct assignment requires a temporary local"
                                                    .to_string(),
                                            )
                                            .with_span(SourceSpan::default())
                                        })?;
                                        let store_fields =
                                            collect_boxed_store_leaves(value.as_ref(), &field_type, offset)?;
                                        tasks.push(EmitTask::Eval(full_target_expr));
                                        for (field_expr, field_type, field_offset) in
                                            store_fields.into_iter().rev()
                                        {
                                            let field_store = match field_type {
                                                WasmType::U8 => Instruction::I32Store8(memarg(
                                                    0,
                                                    0,
                                                    memory_index,
                                                )),
                                                WasmType::I32 | WasmType::Box { .. } => {
                                                    Instruction::I32Store(memarg(
                                                        0,
                                                        2,
                                                        memory_index,
                                                    ))
                                                }
                                                _ => {
                                                    return Err(Diagnostic::new(
                                                        "Boxed struct assignment only supports i32 or u8 fields"
                                                            .to_string(),
                                                    )
                                                    .with_span(SourceSpan::default()));
                                                }
                                            };
                                            tasks.push(EmitTask::Instr(field_store));
                                            tasks.push(EmitTask::EvalWithType {
                                                expr: field_expr,
                                                expected: field_type,
                                            });
                                            tasks.push(EmitTask::Instr(Instruction::I32Add));
                                            tasks.push(EmitTask::Instr(Instruction::I32Const(
                                                field_offset,
                                            )));
                                            tasks.push(EmitTask::Instr(Instruction::LocalGet(
                                                base_local,
                                            )));
                                        }
                                        tasks.push(EmitTask::Instr(Instruction::LocalSet(
                                            base_local,
                                        )));
                                        tasks.push(EmitTask::Instr(Instruction::I32Const(0)));
                                    }
                                }
                                WasmType::Array {
                                    element,
                                    field_names,
                                    ..
                                } => {
                                    let field_index = field_names
                                        .iter()
                                        .position(|name| name == property)
                                        .ok_or_else(|| {
                                            Diagnostic::new(format!(
                                                "Field `{}` not found in array",
                                                property
                                            ))
                                            .with_span(SourceSpan::default())
                                        })?;
                                    let field_offset =
                                        (field_index * wasm_type_size(element)) as i32;
                                    let field_type = (**element).clone();
                                    let memory_index =
                                        resolve_box_expr(&object_expr, box_ctx, box_registry)?
                                            .memory_index;
                                    let full_target_expr = lvalue_to_intermediate(&target);

                                    if matches!(
                                        &field_type,
                                        WasmType::U8 | WasmType::I32 | WasmType::Box { .. }
                                    ) {
                                        let temp_local = box_temp_local.ok_or_else(|| {
                                            Diagnostic::new(
                                                "Boxed array assignment requires a temporary local"
                                                    .to_string(),
                                            )
                                            .with_span(SourceSpan::default())
                                        })?;
                                        let store_instr = match &field_type {
                                            WasmType::U8 => Instruction::I32Store8(memarg(
                                                0,
                                                0,
                                                memory_index,
                                            )),
                                            WasmType::I32 | WasmType::Box { .. } => {
                                                Instruction::I32Store(memarg(0, 2, memory_index))
                                            }
                                            _ => unreachable!(
                                                "non-leaf boxed array assignment checked above"
                                            ),
                                        };
                                        tasks.push(EmitTask::Eval(full_target_expr));
                                        tasks.push(EmitTask::Instr(store_instr));
                                        tasks.push(EmitTask::EvalWithType {
                                            expr: (*value).clone(),
                                            expected: field_type.clone(),
                                        });
                                        tasks.push(EmitTask::Instr(Instruction::I32Add));
                                        tasks.push(EmitTask::Instr(Instruction::I32Const(
                                            field_offset,
                                        )));
                                        tasks.push(EmitTask::Instr(Instruction::LocalGet(
                                            temp_local,
                                        )));
                                        tasks.push(EmitTask::Instr(Instruction::LocalSet(
                                            temp_local,
                                        )));
                                        tasks.push(EmitTask::Instr(Instruction::I32Const(0)));
                                    } else {
                                        let base_local = box_base_local.ok_or_else(|| {
                                            Diagnostic::new(
                                                "Boxed array assignment requires a temporary local"
                                                    .to_string(),
                                            )
                                            .with_span(SourceSpan::default())
                                        })?;
                                        let store_fields = collect_boxed_store_leaves(
                                            value.as_ref(),
                                            &field_type,
                                            field_offset,
                                        )?;
                                        tasks.push(EmitTask::Eval(full_target_expr));
                                        for (field_expr, field_type, field_offset) in
                                            store_fields.into_iter().rev()
                                        {
                                            let field_store = match field_type {
                                                WasmType::U8 => Instruction::I32Store8(memarg(
                                                    0,
                                                    0,
                                                    memory_index,
                                                )),
                                                WasmType::I32 | WasmType::Box { .. } => {
                                                    Instruction::I32Store(memarg(
                                                        0,
                                                        2,
                                                        memory_index,
                                                    ))
                                                }
                                                _ => {
                                                    return Err(Diagnostic::new(
                                                        "Boxed array assignment only supports i32 or u8 fields"
                                                            .to_string(),
                                                    )
                                                    .with_span(SourceSpan::default()));
                                                }
                                            };
                                            tasks.push(EmitTask::Instr(field_store));
                                            tasks.push(EmitTask::EvalWithType {
                                                expr: field_expr,
                                                expected: field_type,
                                            });
                                            tasks.push(EmitTask::Instr(Instruction::I32Add));
                                            tasks.push(EmitTask::Instr(Instruction::I32Const(
                                                field_offset,
                                            )));
                                            tasks.push(EmitTask::Instr(Instruction::LocalGet(
                                                base_local,
                                            )));
                                        }
                                        tasks.push(EmitTask::Instr(Instruction::LocalSet(
                                            base_local,
                                        )));
                                        tasks.push(EmitTask::Instr(Instruction::I32Const(0)));
                                    }
                                }
                                _ => {
                                    return Err(Diagnostic::new(
                                        "Property assignment on non-struct type".to_string(),
                                    )
                                    .with_span(SourceSpan::default()));
                                }
                            },
                            WasmType::Struct(fields) => {
                                let (field_index, field_type) = fields
                                    .iter()
                                    .enumerate()
                                    .find(|(_, (name, _))| name == property)
                                    .map(|(index, (_, ty))| (index as u32, ty.clone()))
                                    .ok_or_else(|| {
                                        Diagnostic::new(format!(
                                            "Field `{}` not found in struct",
                                            property
                                        ))
                                        .with_span(SourceSpan::default())
                                    })?;

                                let type_index = type_ctx
                                    .get_type_index(&WasmType::Struct(fields.clone()))
                                    .expect("Type should be registered");

                                let full_target_expr = lvalue_to_intermediate(&target);

                                tasks.push(EmitTask::Eval(full_target_expr));
                                tasks.push(EmitTask::Instr(Instruction::StructSet {
                                    struct_type_index: type_index,
                                    field_index,
                                }));
                                tasks.push(EmitTask::EvalWithType {
                                    expr: (*value).clone(),
                                    expected: field_type.clone(),
                                });
                                tasks.push(EmitTask::Eval(object_expr));
                            }
                            WasmType::Array {
                                field_names,
                                element,
                                ..
                            } => {
                                let field_index = field_names
                                    .iter()
                                    .position(|name| name == property)
                                    .ok_or_else(|| {
                                        Diagnostic::new(format!(
                                            "Field `{}` not found in array",
                                            property
                                        ))
                                        .with_span(SourceSpan::default())
                                    })? as u32;
                                let type_index = type_ctx
                                    .get_type_index(&object_type)
                                    .expect("Type should be registered");
                                let full_target_expr = lvalue_to_intermediate(&target);
                                tasks.push(EmitTask::Eval(full_target_expr));
                                tasks.push(EmitTask::Instr(Instruction::ArraySet(type_index)));
                                tasks.push(EmitTask::EvalWithType {
                                    expr: (*value).clone(),
                                    expected: (**element).clone(),
                                });
                                tasks.push(EmitTask::Instr(Instruction::I32Const(
                                    field_index as i32,
                                )));
                                tasks.push(EmitTask::Eval(object_expr));
                            }
                            WasmType::I32 | WasmType::U8 => {
                                return Err(Diagnostic::new(
                                    "Property assignment on non-struct type".to_string(),
                                )
                                .with_span(SourceSpan::default()));
                            }
                        }
                    }
                    IntermediateLValue::ArrayIndex { array, index, .. } => {
                        let array_expr = lvalue_to_intermediate(array.as_ref());
                        let array_type =
                            infer_type(&array_expr, locals_types, function_return_types)?;
                        let resolved_box =
                            resolve_box_expr(&array_expr, box_ctx, box_registry).ok();
                        let (element, boxed_memory) = match &array_type {
                            WasmType::Array { element, .. } => {
                                (element.clone(), resolved_box.map(|info| info.memory_index))
                            }
                            WasmType::Box { element } => match element.as_ref() {
                                WasmType::Array { element, .. } => {
                                    let info = if let Some(info) = resolved_box {
                                        info
                                    } else {
                                        resolve_box_expr(&array_expr, box_ctx, box_registry)?
                                    };
                                    (element.clone(), Some(info.memory_index))
                                }
                                _ => {
                                    return Err(Diagnostic::new(
                                        "Index assignment on non-array type",
                                    )
                                    .with_span(SourceSpan::default()));
                                }
                            },
                            _ => {
                                return Err(Diagnostic::new("Index assignment on non-array type")
                                    .with_span(SourceSpan::default()));
                            }
                        };

                        let index_type =
                            infer_type(index.as_ref(), locals_types, function_return_types)?;
                        if strip_box_type(&index_type) != &WasmType::I32 {
                            return Err(Diagnostic::new("Array index must be i32".to_string())
                                .with_span(SourceSpan::default()));
                        }

                        let full_target_expr = lvalue_to_intermediate(&target);
                        if let Some(memory_index) = boxed_memory {
                            let element_size = wasm_type_size(&element) as i32;
                            let temp_local = box_temp_local.ok_or_else(|| {
                                Diagnostic::new(
                                    "Boxed array assignment requires a temporary local".to_string(),
                                )
                                .with_span(SourceSpan::default())
                            })?;
                            let element_type = (*element).clone();
                            if matches!(
                                element_type,
                                WasmType::U8 | WasmType::I32 | WasmType::Box { .. }
                            ) {
                                let store_instr = match element_type {
                                    WasmType::U8 => {
                                        Instruction::I32Store8(memarg(0, 0, memory_index))
                                    }
                                    WasmType::I32 | WasmType::Box { .. } => {
                                        Instruction::I32Store(memarg(0, 2, memory_index))
                                    }
                                    _ => unreachable!("non-leaf element type checked above"),
                                };
                                tasks.push(EmitTask::Eval(full_target_expr));
                                tasks.push(EmitTask::Instr(store_instr));
                                tasks.push(EmitTask::EvalWithType {
                                    expr: (*value).clone(),
                                    expected: element_type,
                                });
                                tasks.push(EmitTask::Instr(Instruction::LocalGet(temp_local)));
                                tasks.push(EmitTask::Instr(Instruction::LocalSet(temp_local)));
                                tasks.push(EmitTask::Instr(Instruction::I32Mul));
                                tasks.push(EmitTask::Instr(Instruction::I32Const(element_size)));
                                tasks.push(EmitTask::EvalValue((**index).clone()));
                            } else {
                                let base_local = box_base_local.ok_or_else(|| {
                                    Diagnostic::new(
                                        "Boxed array assignment requires a temporary local"
                                            .to_string(),
                                    )
                                    .with_span(SourceSpan::default())
                                })?;
                                let store_fields =
                                    collect_boxed_store_leaves(value.as_ref(), &element_type, 0)?;
                                tasks.push(EmitTask::Eval(full_target_expr));
                                for (field_expr, field_type, field_offset) in
                                    store_fields.into_iter().rev()
                                {
                                    let field_store = match field_type {
                                        WasmType::U8 => {
                                            Instruction::I32Store8(memarg(0, 0, memory_index))
                                        }
                                        WasmType::I32 | WasmType::Box { .. } => {
                                            Instruction::I32Store(memarg(0, 2, memory_index))
                                        }
                                        _ => {
                                            return Err(Diagnostic::new(
                                                "Boxed struct array assignment only supports i32 or u8 fields"
                                                    .to_string(),
                                            )
                                            .with_span(SourceSpan::default()));
                                        }
                                    };
                                    tasks.push(EmitTask::Instr(field_store));
                                    tasks.push(EmitTask::EvalWithType {
                                        expr: field_expr,
                                        expected: field_type,
                                    });
                                    tasks.push(EmitTask::Instr(Instruction::I32Add));
                                    tasks
                                        .push(EmitTask::Instr(Instruction::I32Const(field_offset)));
                                    tasks.push(EmitTask::Instr(Instruction::LocalGet(base_local)));
                                }
                                tasks.push(EmitTask::Instr(Instruction::LocalSet(base_local)));
                                tasks.push(EmitTask::Instr(Instruction::I32Mul));
                                tasks.push(EmitTask::Instr(Instruction::I32Const(element_size)));
                                tasks.push(EmitTask::EvalValue((**index).clone()));
                                continue;
                            }
                        } else {
                            let type_index = type_ctx
                                .get_type_index(&array_type)
                                .expect("Type should be registered");
                            tasks.push(EmitTask::Eval(full_target_expr));
                            tasks.push(EmitTask::Instr(Instruction::ArraySet(type_index)));
                            tasks.push(EmitTask::EvalWithType {
                                expr: (*value).clone(),
                                expected: (*element).clone(),
                            });
                            tasks.push(EmitTask::EvalValue((**index).clone()));
                            tasks.push(EmitTask::Eval(array_expr));
                        }
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
                        tasks.push(EmitTask::EvalValue((*right).clone()));
                        tasks.push(EmitTask::Instr(Instruction::If(BlockType::Result(
                            ValType::I32,
                        ))));
                        tasks.push(EmitTask::PushControl(ControlFrame::If));
                        tasks.push(EmitTask::EvalValue((*left).clone()));
                    }
                    BinaryIntrinsicOperator::BooleanOr => {
                        tasks.push(EmitTask::PopControl);
                        tasks.push(EmitTask::Instr(Instruction::End));
                        tasks.push(EmitTask::EvalValue((*right).clone()));
                        tasks.push(EmitTask::Instr(Instruction::Else));
                        tasks.push(EmitTask::Instr(Instruction::I32Const(1)));
                        tasks.push(EmitTask::Instr(Instruction::If(BlockType::Result(
                            ValType::I32,
                        ))));
                        tasks.push(EmitTask::PushControl(ControlFrame::If));
                        tasks.push(EmitTask::EvalValue((*left).clone()));
                    }
                    _ => {
                        let left_type =
                            infer_type(left.as_ref(), locals_types, function_return_types)?;
                        let right_type =
                            infer_type(right.as_ref(), locals_types, function_return_types)?;
                        let left_unboxed = strip_box_type(&left_type);
                        let right_unboxed = strip_box_type(&right_type);
                        let is_u8 = matches!(left_unboxed, WasmType::U8)
                            && matches!(right_unboxed, WasmType::U8);
                        let op_instr = match op {
                            BinaryIntrinsicOperator::I32Add => Instruction::I32Add,
                            BinaryIntrinsicOperator::I32Subtract => Instruction::I32Sub,
                            BinaryIntrinsicOperator::I32Multiply => Instruction::I32Mul,
                            BinaryIntrinsicOperator::I32Divide => {
                                if is_u8 {
                                    Instruction::I32DivU
                                } else {
                                    Instruction::I32DivS
                                }
                            }
                            BinaryIntrinsicOperator::I32Equal => Instruction::I32Eq,
                            BinaryIntrinsicOperator::I32NotEqual => Instruction::I32Ne,
                            BinaryIntrinsicOperator::I32LessThan => {
                                if is_u8 {
                                    Instruction::I32LtU
                                } else {
                                    Instruction::I32LtS
                                }
                            }
                            BinaryIntrinsicOperator::I32GreaterThan => {
                                if is_u8 {
                                    Instruction::I32GtU
                                } else {
                                    Instruction::I32GtS
                                }
                            }
                            BinaryIntrinsicOperator::I32LessThanOrEqual => {
                                if is_u8 {
                                    Instruction::I32LeU
                                } else {
                                    Instruction::I32LeS
                                }
                            }
                            BinaryIntrinsicOperator::I32GreaterThanOrEqual => {
                                if is_u8 {
                                    Instruction::I32GeU
                                } else {
                                    Instruction::I32GeS
                                }
                            }
                            BinaryIntrinsicOperator::BooleanXor => Instruction::I32Xor,
                            BinaryIntrinsicOperator::BooleanAnd
                            | BinaryIntrinsicOperator::BooleanOr => {
                                unreachable!("boolean ops handled before op_instr")
                            }
                        };
                        if is_u8
                            && matches!(
                                op,
                                BinaryIntrinsicOperator::I32Add
                                    | BinaryIntrinsicOperator::I32Subtract
                                    | BinaryIntrinsicOperator::I32Multiply
                                    | BinaryIntrinsicOperator::I32Divide
                            )
                        {
                            tasks.push(EmitTask::Instr(Instruction::I32And));
                            tasks.push(EmitTask::Instr(Instruction::I32Const(0xFF)));
                        }
                        tasks.push(EmitTask::Instr(op_instr));
                        tasks.push(EmitTask::EvalValue((*right).clone()));
                        tasks.push(EmitTask::EvalValue((*left).clone()));
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
                    tasks.push(EmitTask::EvalValue((*operand).clone()));
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
                    if !wasm_types_equivalent(&value_type, expected_type) {
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
                    tasks.push(EmitTask::EvalWithType {
                        expr: (*value).clone(),
                        expected: expected_type.clone(),
                    });
                }
                IntermediateKind::Loop { body } => {
                    let loop_result_type =
                        determine_loop_result_type(&body, locals_types, function_return_types)?;
                    if let Some(result_type) = loop_result_type {
                        let block_type = BlockType::Result(result_type.to_val_type(type_ctx));
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
                        tasks.push(EmitTask::Instr(Instruction::Loop(BlockType::Empty)));
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
                        tasks.push(EmitTask::Instr(Instruction::Loop(BlockType::Empty)));
                        tasks.push(EmitTask::PushControl(ControlFrame::Loop));
                    }
                }
                IntermediateKind::FunctionCall { function, argument } => {
                    let callee = functions.get(function).ok_or_else(|| {
                        Diagnostic::new("Unknown function call target".to_string())
                            .with_span(SourceSpan::default())
                    })?;

                    let arg_type = infer_type(&argument, locals_types, function_return_types)?;
                    let temp_local_name = match_counter.temp_for_function(function);
                    let temp_local_index = locals
                        .get(&temp_local_name)
                        .copied()
                        .unwrap_or_else(|| panic!("Temp local should exist: {}", temp_local_name));

                    let temp_identifier =
                        IntermediateKind::Identifier(Identifier::new(temp_local_name));
                    let argument_exprs = flatten_call_arguments(
                        &callee.parameter,
                        &callee.input_type,
                        temp_identifier,
                    )?;

                    let expected_type = intermediate_type_to_wasm(&callee.input_type);

                    tasks.push(EmitTask::Instr(Instruction::Call(
                        (function as u32) + function_index_offset,
                    )));
                    tasks.push(EmitTask::CheckCallArgType {
                        arg_type,
                        expected_type: expected_type.clone(),
                    });

                    for expr in argument_exprs.into_iter().rev() {
                        tasks.push(EmitTask::Eval(expr));
                    }

                    tasks.push(EmitTask::Instr(Instruction::LocalSet(temp_local_index)));
                    tasks.push(EmitTask::EvalWithType {
                        expr: (*argument).clone(),
                        expected: expected_type,
                    });
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

                    let mut result_type = None;
                    let block_type = if (then_produces_value && else_produces_value)
                        || ((then_diverges || else_diverges)
                            && (then_produces_value || else_produces_value))
                    {
                        let inferred_type = if then_produces_value {
                            infer_type(&then_branch, locals_types, function_return_types)?
                        } else {
                            infer_type(&else_branch, locals_types, function_return_types)?
                        };
                        let wasm_result_type = inferred_type.to_val_type(type_ctx);
                        result_type = Some(inferred_type);
                        BlockType::Result(wasm_result_type)
                    } else {
                        BlockType::Empty
                    };

                    tasks.push(EmitTask::PopControl);
                    tasks.push(EmitTask::Instr(Instruction::End));
                    if else_produces_value && matches!(block_type, BlockType::Empty) {
                        tasks.push(EmitTask::Instr(Instruction::Drop));
                    }
                    if let Some(expected_type) = result_type.clone() {
                        if else_diverges {
                            tasks.push(EmitTask::Eval((*else_branch).clone()));
                        } else {
                            tasks.push(EmitTask::EvalWithType {
                                expr: (*else_branch).clone(),
                                expected: expected_type,
                            });
                        }
                    } else {
                        tasks.push(EmitTask::Eval((*else_branch).clone()));
                    }
                    tasks.push(EmitTask::Instr(Instruction::Else));
                    if then_produces_value && matches!(block_type, BlockType::Empty) {
                        tasks.push(EmitTask::Instr(Instruction::Drop));
                    }
                    if let Some(expected_type) = result_type {
                        if then_diverges {
                            tasks.push(EmitTask::Eval((*then_branch).clone()));
                        } else {
                            tasks.push(EmitTask::EvalWithType {
                                expr: (*then_branch).clone(),
                                expected: expected_type,
                            });
                        }
                    } else {
                        tasks.push(EmitTask::Eval((*then_branch).clone()));
                    }
                    tasks.push(EmitTask::Instr(Instruction::If(block_type)));
                    tasks.push(EmitTask::PushControl(ControlFrame::If));
                    tasks.push(EmitTask::EvalValue((*condition).clone()));
                }
                IntermediateKind::Binding(binding) => {
                    let binding = binding.as_ref();
                    record_box_binding(binding, box_ctx, box_registry)?;
                    let name = binding.identifier.name.clone();
                    let local_index = locals
                        .get(&name)
                        .copied()
                        .unwrap_or_else(|| panic!("Local '{}' should have been collected", name));
                    let binding_type = locals_types
                        .get(&name)
                        .cloned()
                        .unwrap_or_else(|| intermediate_type_to_wasm(&binding.binding_type));
                    tasks.push(EmitTask::Instr(Instruction::I32Const(1)));
                    tasks.push(EmitTask::Instr(Instruction::LocalSet(local_index)));
                    tasks.push(EmitTask::EvalWithType {
                        expr: binding.expr.clone(),
                        expected: binding_type,
                    });
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
                    tasks.push(EmitTask::EvalWithType {
                        expr: (*value).clone(),
                        expected: expected_return_type.clone(),
                    });
                }
                IntermediateKind::Struct(items) => {
                    let mut sorted_items = items.clone();
                    sorted_items.sort_by(|(a_name, _), (b_name, _)| a_name.name.cmp(&b_name.name));

                    let mut field_types = Vec::new();
                    for (name, value) in &sorted_items {
                        let ty = infer_type(value, locals_types, function_return_types)?;
                        field_types.push((name.name.clone(), ty));
                    }

                    let type_index = type_ctx
                        .get_type_index(&WasmType::Struct(field_types))
                        .ok_or_else(|| {
                            Diagnostic::new("Struct type not found in context")
                                .with_span(SourceSpan::default())
                        })?;

                    tasks.push(EmitTask::Instr(Instruction::StructNew(type_index)));

                    for (_, value) in sorted_items.into_iter().rev() {
                        tasks.push(EmitTask::Eval(value));
                    }
                }
                IntermediateKind::ArrayLiteral {
                    items,
                    element_type,
                    field_names,
                } => {
                    let element_wasm = intermediate_type_to_wasm(&element_type);
                    let array_type = WasmType::Array {
                        element: Box::new(element_wasm),
                        length: items.len(),
                        field_names,
                    };
                    let type_index = type_ctx.get_type_index(&array_type).ok_or_else(|| {
                        Diagnostic::new("Array type not found in context")
                            .with_span(SourceSpan::default())
                    })?;
                    tasks.push(EmitTask::Instr(Instruction::ArrayNewFixed {
                        array_type_index: type_index,
                        array_size: items.len() as u32,
                    }));
                    for value in items.into_iter().rev() {
                        tasks.push(EmitTask::Eval(value));
                    }
                }
                IntermediateKind::TypePropertyAccess { object, property } => {
                    let object_type = infer_type(&object, locals_types, function_return_types)?;
                    match object_type {
                        WasmType::Box { element } => match *element {
                            WasmType::Struct(fields) => {
                                let (field_index, field_type) = fields
                                    .iter()
                                    .position(|(n, _)| n == &property)
                                    .and_then(|index| fields.get(index).map(|(_, ty)| (index, ty)))
                                    .ok_or_else(|| {
                                        Diagnostic::new(format!(
                                            "Field `{}` not found in struct",
                                            property
                                        ))
                                        .with_span(SourceSpan::default())
                                    })?;
                                let field_offset = sorted_wasm_fields(&fields)
                                    .iter()
                                    .take(field_index)
                                    .map(|(_, ty)| wasm_type_size(ty) as u32)
                                    .sum();
                                let box_info = resolve_box_expr(&object, box_ctx, box_registry)?;
                                let temp_local = box_temp_local.ok_or_else(|| {
                                    Diagnostic::new(
                                        "Box load requires a temporary local".to_string(),
                                    )
                                    .with_span(SourceSpan::default())
                                })?;
                                func.instruction(&Instruction::I32Const(0));
                                func.instruction(&Instruction::LocalSet(temp_local));
                                emit_load_from_memory(
                                    func,
                                    field_type,
                                    type_ctx,
                                    temp_local,
                                    field_offset,
                                    box_info.memory_index,
                                )?;
                            }
                            WasmType::Array {
                                element,
                                field_names,
                                ..
                            } => {
                                let field_index = field_names
                                    .iter()
                                    .position(|name| name == &property)
                                    .ok_or_else(|| {
                                        Diagnostic::new(format!(
                                            "Field `{}` not found in array",
                                            property
                                        ))
                                        .with_span(SourceSpan::default())
                                    })?;
                                let offset = (field_index * wasm_type_size(&element)) as u32;
                                let box_info = resolve_box_expr(&object, box_ctx, box_registry)?;
                                let temp_local = box_temp_local.ok_or_else(|| {
                                    Diagnostic::new(
                                        "Box load requires a temporary local".to_string(),
                                    )
                                    .with_span(SourceSpan::default())
                                })?;
                                func.instruction(&Instruction::I32Const(0));
                                func.instruction(&Instruction::LocalSet(temp_local));
                                emit_load_from_memory(
                                    func,
                                    &element,
                                    type_ctx,
                                    temp_local,
                                    offset,
                                    box_info.memory_index,
                                )?;
                            }
                            _ => {
                                return Err(Diagnostic::new(
                                    "Type property access on non-struct type",
                                )
                                .with_span(SourceSpan::default()));
                            }
                        },
                        WasmType::Struct(fields) => {
                            let (field_index, field_type) = fields
                                .iter()
                                .position(|(n, _)| n == &property)
                                .and_then(|index| fields.get(index).map(|(_, ty)| (index, ty)))
                                .ok_or_else(|| {
                                    Diagnostic::new(format!(
                                        "Field `{}` not found in struct",
                                        property
                                    ))
                                    .with_span(SourceSpan::default())
                                })?;

                            let type_index = type_ctx
                                .get_type_index(&WasmType::Struct(fields.clone()))
                                .expect("Type should be registered");

                            let instr = match field_type {
                                WasmType::U8 => Instruction::StructGetU {
                                    struct_type_index: type_index,
                                    field_index: field_index as u32,
                                },
                                _ => Instruction::StructGet {
                                    struct_type_index: type_index,
                                    field_index: field_index as u32,
                                },
                            };
                            tasks.push(EmitTask::Instr(instr));
                            tasks.push(EmitTask::Eval((*object).clone()));
                        }
                        WasmType::Array {
                            element,
                            length,
                            field_names,
                        } => {
                            let field_index = field_names
                                .iter()
                                .position(|name| name == &property)
                                .ok_or_else(|| {
                                    Diagnostic::new(format!(
                                        "Field `{}` not found in array",
                                        property
                                    ))
                                    .with_span(SourceSpan::default())
                                })?;

                            let array_type = WasmType::Array {
                                element: element.clone(),
                                length,
                                field_names: field_names.clone(),
                            };
                            let type_index = type_ctx
                                .get_type_index(&array_type)
                                .expect("Type should be registered");

                            let instr = match element.as_ref() {
                                WasmType::U8 => Instruction::ArrayGetU(type_index),
                                _ => Instruction::ArrayGet(type_index),
                            };
                            tasks.push(EmitTask::Instr(instr));
                            tasks.push(EmitTask::Instr(Instruction::I32Const(field_index as i32)));
                            tasks.push(EmitTask::Eval((*object).clone()));
                        }
                        WasmType::I32 | WasmType::U8 => {
                            return Err(Diagnostic::new("Type property access on non-struct type")
                                .with_span(SourceSpan::default()));
                        }
                    }
                }
                IntermediateKind::ArrayIndex { array, index } => {
                    let array_type = infer_type(&array, locals_types, function_return_types)?;
                    let (array_element, is_boxed) = match &array_type {
                        WasmType::Array { element, .. } => (element.clone(), false),
                        WasmType::Box { element } => match element.as_ref() {
                            WasmType::Array { element, .. } => (element.clone(), true),
                            _ => {
                                return Err(Diagnostic::new("Indexing on non-array type")
                                    .with_span(SourceSpan::default()));
                            }
                        },
                        _ => {
                            return Err(Diagnostic::new("Indexing on non-array type")
                                .with_span(SourceSpan::default()));
                        }
                    };
                    let index_type = infer_type(&index, locals_types, function_return_types)?;
                    if strip_box_type(&index_type) != &WasmType::I32 {
                        return Err(Diagnostic::new("Array index must be i32".to_string())
                            .with_span(SourceSpan::default()));
                    }
                    if is_boxed {
                        let box_info = resolve_box_expr(&array, box_ctx, box_registry)?;
                        tasks.push(EmitTask::FinishBoxArrayIndex {
                            element: (*array_element).clone(),
                            memory_index: box_info.memory_index,
                        });
                        tasks.push(EmitTask::EvalValue((*index).clone()));
                    } else {
                        let type_index = type_ctx
                            .get_type_index(&array_type)
                            .expect("Type should be registered");
                        let instr = match array_element.as_ref() {
                            WasmType::U8 => Instruction::ArrayGetU(type_index),
                            _ => Instruction::ArrayGet(type_index),
                        };
                        tasks.push(EmitTask::Instr(instr));
                        tasks.push(EmitTask::EvalValue((*index).clone()));
                        tasks.push(EmitTask::Eval((*array).clone()));
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
                IntermediateKind::TypePropertyAccess { object, .. } => {
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
                IntermediateKind::ArrayLiteral { items, .. } => {
                    for child in items.iter().rev() {
                        stack.push(Frame {
                            expr: child,
                            possibility: frame.possibility,
                            in_inner_loop: frame.in_inner_loop,
                            visited: false,
                        });
                    }
                }
                IntermediateKind::ArrayIndex { array, index } => {
                    stack.push(Frame {
                        expr: index,
                        possibility: frame.possibility,
                        in_inner_loop: frame.in_inner_loop,
                        visited: false,
                    });
                    stack.push(Frame {
                        expr: array,
                        possibility: frame.possibility,
                        in_inner_loop: frame.in_inner_loop,
                        visited: false,
                    });
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
            | IntermediateKind::TypePropertyAccess { .. }
            | IntermediateKind::ArrayIndex { .. }
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
            IntermediateKind::ArrayLiteral { items, .. } => {
                let mut diverges = false;
                for _ in 0..items.len() {
                    if results.pop().unwrap_or(false) {
                        diverges = true;
                    }
                }
                diverges
            }
            IntermediateKind::Literal(_)
            | IntermediateKind::Identifier(_)
            | IntermediateKind::InlineAssembly { .. }
            | IntermediateKind::BoxAlloc { .. } => false,
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
            IntermediateKind::TypePropertyAccess { object, .. } => {
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
