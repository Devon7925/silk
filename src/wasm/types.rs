use crate::{
    diagnostics::{Diagnostic, SourceSpan},
    intermediate::{IntermediateBinding, IntermediateKind, IntermediateType},
    parsing::{ExpressionLiteral, Identifier},
};

use super::encoder::{HeapType, MemArg, RefType, ValType};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) enum WasmType {
    I32,
    U8,
    Box {
        element: Box<WasmType>,
    },
    Struct(Vec<(String, WasmType)>),
    Array {
        element: Box<WasmType>,
        length: usize,
        field_names: Vec<String>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum ControlFrame {
    Block,
    Loop,
    If,
}

#[derive(Clone, Debug)]
pub(super) struct LoopContext {
    pub(super) break_target_index: usize,
    pub(super) result_type: Option<WasmType>,
}

#[derive(Default)]
pub(super) struct MatchCounter {
    next_id: usize,
    call_temps: std::collections::HashMap<usize, String>,
}

impl MatchCounter {
    pub(super) fn temp_for_function(&mut self, function_index: usize) -> String {
        if let Some(name) = self.call_temps.get(&function_index) {
            return name.clone();
        }
        let name = format!("__match_temp_{}", self.next_id);
        self.next_id += 1;
        self.call_temps.insert(function_index, name.clone());
        name
    }
}

#[derive(Default)]
pub(super) struct BoxRegistry {
    next_index: u32,
    pub(super) memories: Vec<BoxMemory>,
}

#[derive(Clone, Debug)]
pub(super) struct BoxInfo {
    pub(super) memory_index: u32,
}

#[derive(Clone, Debug)]
pub(super) struct BoxMemory {
    pub(super) memory_index: u32,
    pub(super) bytes: Vec<u8>,
    pub(super) export_name: Option<String>,
}

impl BoxRegistry {
    pub(super) fn register_box(
        &mut self,
        value: &IntermediateKind,
        element_type: &IntermediateType,
        export_name: Option<String>,
    ) -> Result<BoxInfo, Diagnostic> {
        let bytes = encode_box_value(value, element_type).map_err(|err| {
            let name = export_name.clone().unwrap_or_else(|| "<anonymous>".to_string());
            Diagnostic {
                message: format!("Box allocation for `{}` failed: {}", name, err.message),
                span: err.span,
            }
        })?;
        let memory_index = self.next_index;
        self.next_index = self.next_index.saturating_add(1);
        self.memories.push(BoxMemory {
            memory_index,
            bytes,
            export_name,
        });
        Ok(BoxInfo { memory_index })
    }
}

#[derive(Default)]
pub(super) struct BoxContext {
    pub(super) bindings: std::collections::HashMap<String, BoxInfo>,
    pub(super) struct_fields:
        std::collections::HashMap<String, std::collections::HashMap<String, BoxInfo>>,
    pub(super) array_elements: std::collections::HashMap<String, Vec<BoxInfo>>,
}

pub(super) fn resolve_box_expr(
    expr: &IntermediateKind,
    box_ctx: &mut BoxContext,
    box_registry: &mut BoxRegistry,
) -> Result<BoxInfo, Diagnostic> {
    match expr {
        IntermediateKind::BoxAlloc {
            value,
            element_type,
        } => box_registry.register_box(value, element_type, None),
        IntermediateKind::Identifier(identifier) => box_ctx
            .bindings
            .get(&identifier.name)
            .cloned()
            .ok_or_else(|| {
                Diagnostic::new(format!(
                    "Unknown boxed identifier `{}` in wasm lowering",
                    identifier.name
                ))
                .with_span(SourceSpan::default())
            }),
        IntermediateKind::TypePropertyAccess { object, property } => {
            if let IntermediateKind::Identifier(identifier) = object.as_ref()
                && let Some(fields) = box_ctx.struct_fields.get(&identifier.name)
                && let Some(info) = fields.get(property)
            {
                return Ok(info.clone());
            }
            if let IntermediateKind::Struct(items) = object.as_ref()
                && let Some((_, field_expr)) = items.iter().find(|(id, _)| id.name == *property)
            {
                return resolve_box_expr(field_expr, box_ctx, box_registry);
            }
            Err(
                Diagnostic::new("Unable to resolve boxed struct field at compile time".to_string())
                    .with_span(SourceSpan::default()),
            )
        }
        IntermediateKind::ArrayIndex { array, index } => {
            let idx = match index.as_ref() {
                IntermediateKind::Literal(ExpressionLiteral::Number(value)) if *value >= 0 => {
                    Some(*value as usize)
                }
                _ => None,
            };
            if let (Some(idx), IntermediateKind::Identifier(identifier)) = (idx, array.as_ref())
                && let Some(elements) = box_ctx.array_elements.get(&identifier.name)
                && let Some(info) = elements.get(idx)
            {
                return Ok(info.clone());
            }
            if let (Some(idx), IntermediateKind::ArrayLiteral { items, .. }) = (idx, array.as_ref())
                && let Some(item) = items.get(idx)
            {
                return resolve_box_expr(item, box_ctx, box_registry);
            }
            Err(Diagnostic::new(
                "Unable to resolve boxed array element at compile time".to_string(),
            )
            .with_span(SourceSpan::default()))
        }
        _ => Err(
            Diagnostic::new("Unable to resolve boxed value at compile time".to_string())
                .with_span(SourceSpan::default()),
        ),
    }
}

pub(super) fn record_box_binding(
    binding: &IntermediateBinding,
    box_ctx: &mut BoxContext,
    box_registry: &mut BoxRegistry,
) -> Result<(), Diagnostic> {
    let name = binding.identifier.name.clone();
    match &binding.binding_type {
        IntermediateType::Box { .. } => {
            let info = resolve_box_expr(&binding.expr, box_ctx, box_registry)?;
            box_ctx.bindings.insert(name, info);
        }
        IntermediateType::Struct(fields) => {
            let mut field_map = std::collections::HashMap::new();
            match &binding.expr {
                IntermediateKind::Struct(items) => {
                    for (field_name, field_ty) in fields {
                        if !matches!(field_ty, IntermediateType::Box { .. }) {
                            continue;
                        }
                        let field_expr = items
                            .iter()
                            .find(|(id, _)| id.name == *field_name)
                            .map(|(_, expr)| expr)
                            .ok_or_else(|| {
                                Diagnostic::new(format!(
                                    "Missing field {} in boxed struct binding",
                                    field_name
                                ))
                                .with_span(SourceSpan::default())
                            })?;
                        let info = resolve_box_expr(field_expr, box_ctx, box_registry)?;
                        field_map.insert(field_name.clone(), info);
                    }
                }
                IntermediateKind::Identifier(identifier) => {
                    if let Some(existing) = box_ctx.struct_fields.get(&identifier.name) {
                        field_map = existing.clone();
                    }
                }
                _ => {
                    if fields
                        .iter()
                        .any(|(_, ty)| matches!(ty, IntermediateType::Box { .. }))
                    {
                        return Err(Diagnostic::new(
                            "Struct bindings containing boxes must be compile-time literals"
                                .to_string(),
                        )
                        .with_span(SourceSpan::default()));
                    }
                }
            }
            if !field_map.is_empty() {
                box_ctx.struct_fields.insert(name, field_map);
            }
        }
        IntermediateType::Array {
            element, length, ..
        } => {
            if !matches!(element.as_ref(), IntermediateType::Box { .. }) {
                return Ok(());
            }
            let mut element_map = Vec::new();
            match &binding.expr {
                IntermediateKind::ArrayLiteral { items, .. } => {
                    if items.len() != *length {
                        return Err(Diagnostic::new(
                            "Boxed array binding length mismatch".to_string(),
                        )
                        .with_span(SourceSpan::default()));
                    }
                    for item in items {
                        element_map.push(resolve_box_expr(item, box_ctx, box_registry)?);
                    }
                }
                IntermediateKind::Identifier(identifier) => {
                    if let Some(existing) = box_ctx.array_elements.get(&identifier.name) {
                        element_map = existing.clone();
                    }
                }
                _ => {
                    return Err(Diagnostic::new(
                        "Array bindings containing boxes must be compile-time literals".to_string(),
                    )
                    .with_span(SourceSpan::default()));
                }
            }
            if !element_map.is_empty() {
                box_ctx.array_elements.insert(name, element_map);
            }
        }
        _ => {}
    }
    Ok(())
}

fn sorted_intermediate_fields(
    fields: &[(String, IntermediateType)],
) -> Vec<(String, IntermediateType)> {
    let mut sorted = fields.to_vec();
    sorted.sort_by(|a, b| a.0.cmp(&b.0));
    sorted
}

fn encode_box_value(
    value: &IntermediateKind,
    ty: &IntermediateType,
) -> Result<Vec<u8>, Diagnostic> {
    let mut path = Vec::new();
    encode_box_value_with_path(value, ty, &mut path)
}

fn encode_box_value_with_path(
    value: &IntermediateKind,
    ty: &IntermediateType,
    path: &mut Vec<String>,
) -> Result<Vec<u8>, Diagnostic> {
    match ty {
        IntermediateType::I32 => {
            let number = match value {
                IntermediateKind::Literal(ExpressionLiteral::Number(value)) => *value,
                IntermediateKind::Literal(ExpressionLiteral::Boolean(value)) => {
                    if *value {
                        1
                    } else {
                        0
                    }
                }
                IntermediateKind::Literal(ExpressionLiteral::Char(value)) => i32::from(*value),
                _ => {
                    return Err(Diagnostic::new(
                        "Box allocation requires a constant i32 value".to_string(),
                    ));
                }
            };
            Ok(number.to_le_bytes().to_vec())
        }
        IntermediateType::U8 => {
            let number = match value {
                IntermediateKind::Literal(ExpressionLiteral::Char(value)) => *value,
                IntermediateKind::Literal(ExpressionLiteral::Number(value)) => u8::try_from(*value)
                    .map_err(|_| {
                        Diagnostic::new("Box allocation u8 is out of range".to_string())
                    })?,
                _ => {
                    return Err(Diagnostic::new(
                        "Box allocation requires a constant u8 value".to_string(),
                    ));
                }
            };
            Ok(vec![number])
        }
        IntermediateType::Box { .. } => Err(Diagnostic::new(
            "Nested box values are not supported in wasm exports".to_string(),
        )),
        IntermediateType::Struct(fields) => {
            let items = match value {
                IntermediateKind::Struct(items) => items.clone(),
                IntermediateKind::ArrayLiteral { items, field_names, .. } => {
                    if items.len() != field_names.len() {
                        return Err(Diagnostic::new(
                            "Box allocation requires a struct value".to_string(),
                        ));
                    }
                    let struct_names: std::collections::HashSet<&String> =
                        fields.iter().map(|(name, _)| name).collect();
                    let array_names: std::collections::HashSet<&String> =
                        field_names.iter().collect();
                    if struct_names.len() != fields.len()
                        || array_names.len() != field_names.len()
                        || struct_names != array_names
                    {
                        return Err(Diagnostic::new(
                            "Box allocation requires a struct value".to_string(),
                        ));
                    }
                    field_names
                        .iter()
                        .cloned()
                        .zip(items.iter().cloned())
                        .map(|(name, expr)| (Identifier::new(name), expr))
                        .collect()
                }
                other => {
                    return Err(Diagnostic::new(
                        "Box allocation requires a struct value".to_string(),
                    ));
                }
            };
            let mut bytes = Vec::new();
            let sorted_fields = sorted_intermediate_fields(fields);
            for (name, field_ty) in sorted_fields.iter() {
                let field_value = items
                    .iter()
                    .find(|(id, _)| id.name == *name)
                    .map(|(_, expr)| expr)
                    .ok_or_else(|| {
                        Diagnostic::new(format!("Missing field {name} in struct value"))
                    })?;
                path.push(name.clone());
                bytes.extend(encode_box_value_with_path(field_value, field_ty, path)?);
                path.pop();
            }
            Ok(bytes)
        }
        IntermediateType::Array {
            element, length, ..
        } => {
            let IntermediateKind::ArrayLiteral { items, .. } = value else {
                return Err(Diagnostic::new(
                    "Box allocation requires an array value".to_string(),
                ));
            };
            if items.len() != *length {
                return Err(Diagnostic::new(
                    "Box allocation array length mismatch".to_string(),
                ));
            }
            let mut bytes = Vec::new();
            for (index, item) in items.iter().enumerate() {
                path.push(index.to_string());
                bytes.extend(encode_box_value_with_path(item, element, path)?);
                path.pop();
            }
            Ok(bytes)
        }
    }
}

impl WasmType {
    pub(super) fn to_val_type(&self, ctx: &TypeContext) -> ValType {
        match self {
            WasmType::I32 => ValType::I32,
            WasmType::U8 => ValType::I32,
            WasmType::Box { .. } => ValType::I32,
            WasmType::Struct(_fields) => {
                let type_index = ctx.get_type_index(self).expect("Type should be registered");
                ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Concrete(type_index),
                })
            }
            WasmType::Array { .. } => {
                let type_index = ctx.get_type_index(self).expect("Type should be registered");
                ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Concrete(type_index),
                })
            }
        }
    }
}

pub(super) fn format_wasm_type(ty: &WasmType) -> String {
    let mut stack = vec![(ty, false)];
    let mut results: Vec<String> = Vec::new();

    while let Some((node, visited)) = stack.pop() {
        if !visited {
            stack.push((node, true));
            match node {
                WasmType::Struct(fields) => {
                    for (_, field_ty) in fields.iter().rev() {
                        stack.push((field_ty, false));
                    }
                }
                WasmType::Box { element } => {
                    stack.push((element, false));
                }
                WasmType::Array { element, .. } => {
                    stack.push((element, false));
                }
                _ => {}
            }
            continue;
        }

        match node {
            WasmType::I32 => results.push("i32".to_string()),
            WasmType::U8 => results.push("u8".to_string()),
            WasmType::Box { .. } => {
                let inner = results
                    .pop()
                    .expect("format_wasm_type should have box inner type");
                results.push(format!("box {}", inner));
            }
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
            WasmType::Array {
                length,
                field_names,
                ..
            } => {
                let element = results
                    .pop()
                    .expect("format_wasm_type should have element type");
                results.push(format!(
                    "array[{}] {} {{{}}}",
                    length,
                    element,
                    field_names.join(", ")
                ));
            }
        }
    }

    results
        .pop()
        .expect("format_wasm_type should produce one result")
}

pub(super) fn sorted_wasm_fields(fields: &[(String, WasmType)]) -> Vec<(String, WasmType)> {
    let mut sorted = fields.to_vec();
    sorted.sort_by(|a, b| a.0.cmp(&b.0));
    sorted
}

pub(super) fn wasm_type_size(ty: &WasmType) -> usize {
    match ty {
        WasmType::I32 => 4,
        WasmType::U8 => 1,
        WasmType::Box { .. } => 4,
        WasmType::Struct(fields) => sorted_wasm_fields(fields)
            .iter()
            .map(|(_, field)| wasm_type_size(field))
            .sum(),
        WasmType::Array {
            element, length, ..
        } => wasm_type_size(element) * length,
    }
}

pub(super) fn wasm_types_equivalent(left: &WasmType, right: &WasmType) -> bool {
    let mut left = left;
    let mut right = right;
    loop {
        match (left, right) {
            (WasmType::Box { element }, _) => {
                left = element;
            }
            (_, WasmType::Box { element }) => {
                right = element;
            }
            _ => break,
        }
    }
    matches!(
        (left, right),
        (WasmType::I32, WasmType::U8) | (WasmType::U8, WasmType::I32)
    ) || left == right
}

pub(super) fn strip_box_type(ty: &WasmType) -> &WasmType {
    let mut current = ty;
    loop {
        match current {
            WasmType::Box { element } => {
                current = element.as_ref();
            }
            _ => return current,
        }
    }
}

pub(super) fn strip_box_owned(ty: WasmType) -> WasmType {
    let mut current = ty;
    loop {
        match current {
            WasmType::Box { element } => {
                current = *element;
            }
            other => return other,
        }
    }
}

pub(super) fn memarg(offset: u32, align: u32, memory_index: u32) -> MemArg {
    MemArg {
        offset: u64::from(offset),
        align,
        memory_index,
    }
}

pub(super) fn pages_for_size(size: usize) -> u64 {
    if size == 0 {
        1
    } else {
        (size as u64).div_ceil(65536)
    }
}

pub(super) fn struct_fields_to_wasm_type(
    field_names: Vec<String>,
    field_types: Vec<WasmType>,
) -> WasmType {
    let mut wasm_fields: Vec<(String, WasmType)> =
        field_names.into_iter().zip(field_types).collect();
    wasm_fields.sort_by(|a, b| a.0.cmp(&b.0));
    WasmType::Struct(wasm_fields)
}

pub(super) fn intermediate_type_to_wasm(ty: &IntermediateType) -> WasmType {
    let mut stack = vec![(ty, false)];
    let mut results: Vec<WasmType> = Vec::new();

    while let Some((node, visited)) = stack.pop() {
        if !visited {
            stack.push((node, true));
            match node {
                IntermediateType::Struct(fields) => {
                    for (_, field_ty) in fields.iter().rev() {
                        stack.push((field_ty, false));
                    }
                }
                IntermediateType::Box { element } => {
                    stack.push((element, false));
                }
                IntermediateType::Array { element, .. } => {
                    stack.push((element, false));
                }
                IntermediateType::I32 | IntermediateType::U8 => {}
            }
            continue;
        }

        match node {
            IntermediateType::I32 => results.push(WasmType::I32),
            IntermediateType::U8 => results.push(WasmType::U8),
            IntermediateType::Box { .. } => {
                let element_type = results
                    .pop()
                    .expect("intermediate_type_to_wasm should have box element type");
                results.push(WasmType::Box {
                    element: Box::new(element_type),
                });
            }
            IntermediateType::Struct(fields) => {
                let mut field_names = Vec::with_capacity(fields.len());
                let mut field_types = Vec::with_capacity(fields.len());
                for (name, _) in fields.iter().rev() {
                    let ty = results
                        .pop()
                        .expect("intermediate_type_to_wasm should have struct field type");
                    field_names.push(name.clone());
                    field_types.push(ty);
                }
                field_names.reverse();
                field_types.reverse();
                results.push(struct_fields_to_wasm_type(field_names, field_types));
            }
            IntermediateType::Array {
                length,
                field_names,
                ..
            } => {
                let element_type = results
                    .pop()
                    .expect("intermediate_type_to_wasm should have array element type");
                results.push(WasmType::Array {
                    element: Box::new(element_type),
                    length: *length,
                    field_names: field_names.clone(),
                });
            }
        }
    }

    results
        .pop()
        .expect("intermediate_type_to_wasm should produce a result")
}

pub(super) struct TypeContext {
    pub(super) types: Vec<WasmType>,
    type_map: std::collections::HashMap<WasmType, u32>,
}

impl TypeContext {
    pub(super) fn new() -> Self {
        Self {
            types: Vec::new(),
            type_map: std::collections::HashMap::new(),
        }
    }

    pub(super) fn get_or_register_type(&mut self, ty: WasmType) -> u32 {
        let mut stack = vec![(ty, false)];
        let mut last_index = None;

        while let Some((current, visited)) = stack.pop() {
            if let Some(&index) = self.type_map.get(&current) {
                last_index = Some(index);
                continue;
            }

            if !visited {
                stack.push((current.clone(), true));
                match &current {
                    WasmType::Struct(fields) => {
                        for (_, field_type) in fields.iter().rev() {
                            if !matches!(field_type, WasmType::I32 | WasmType::U8)
                                && !self.type_map.contains_key(field_type)
                            {
                                stack.push((field_type.clone(), false));
                            }
                        }
                    }
                    WasmType::Array { element, .. } => {
                        if !matches!(element.as_ref(), WasmType::I32 | WasmType::U8)
                            && !self.type_map.contains_key(element.as_ref())
                        {
                            stack.push((element.as_ref().clone(), false));
                        }
                    }
                    WasmType::Box { element } => {
                        if !matches!(element.as_ref(), WasmType::I32 | WasmType::U8)
                            && !self.type_map.contains_key(element.as_ref())
                        {
                            stack.push((element.as_ref().clone(), false));
                        }
                    }
                    WasmType::I32 | WasmType::U8 => {
                        continue;
                    }
                }
                continue;
            }

            if let Some(&index) = self.type_map.get(&current) {
                last_index = Some(index);
                continue;
            }

            if matches!(current, WasmType::I32 | WasmType::U8 | WasmType::Box { .. }) {
                continue;
            }
            let index = self.types.len() as u32;
            self.types.push(current.clone());
            self.type_map.insert(current, index);
            last_index = Some(index);
        }

        last_index.expect("Type should be registered")
    }

    pub(super) fn get_type_index(&self, ty: &WasmType) -> Option<u32> {
        self.type_map.get(ty).copied()
    }
}

pub(super) struct WasmFunctionParam {
    pub(super) name: String,
    pub(super) ty: WasmType,
}

pub(super) struct WasmFunctionDef {
    pub(super) params: Vec<WasmFunctionParam>,
    pub(super) body: IntermediateKind,
    pub(super) return_type: WasmType,
}

pub(super) struct WasmFunctionExport {
    pub(super) name: String,
    pub(super) index: usize,
}

pub(super) struct WasmFunctionImport {
    pub(super) import_name: String,
    pub(super) export_name: String,
    pub(super) params: Vec<WasmFunctionParam>,
    pub(super) return_type: WasmType,
}

pub(super) struct WasmGlobalExport {
    pub(super) name: String,
    pub(super) ty: WasmType,
    pub(super) init: IntermediateKind,
    pub(super) intermediate_ty: IntermediateType,
    pub(super) exported: bool,
}

pub(super) struct WasmExports {
    pub(super) imports: Vec<WasmFunctionImport>,
    pub(super) functions: Vec<WasmFunctionExport>,
    pub(super) globals: Vec<WasmGlobalExport>,
}
