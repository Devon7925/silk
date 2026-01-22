use std::collections::HashMap;

use crate::{
    diagnostics::Diagnostic,
    intermediate::{
        IntermediateBinding, IntermediateExportType, IntermediateIntrinsicOperation,
        IntermediateKind, IntermediateLValue, IntermediateResult, IntermediateType,
    },
    parsing::{
        BinaryIntrinsicOperator, BindingPattern, ExpressionLiteral, Identifier, TargetLiteral,
    },
};

#[derive(Clone, Debug)]
enum WgslValueType {
    I32,
    Bool,
    Box(Box<WgslValueType>),
    Struct(IntermediateType),
    Array(IntermediateType),
}

impl WgslValueType {
    fn from_intermediate(ty: &IntermediateType) -> Self {
        match ty {
            IntermediateType::I32 => WgslValueType::I32,
            IntermediateType::U8 => WgslValueType::I32,
            IntermediateType::Box { element } => {
                WgslValueType::Box(Box::new(WgslValueType::from_intermediate(element)))
            }
            IntermediateType::Struct(fields) => {
                WgslValueType::Struct(IntermediateType::Struct(fields.clone()))
            }
            IntermediateType::Array {
                element,
                length,
                field_names,
            } => WgslValueType::Array(IntermediateType::Array {
                element: element.clone(),
                length: *length,
                field_names: field_names.clone(),
            }),
        }
    }

    fn to_wgsl_type(&self, registry: &mut TypeRegistry) -> String {
        match self {
            WgslValueType::I32 => "i32".to_string(),
            WgslValueType::Bool => "bool".to_string(),
            WgslValueType::Box(inner) => {
                let element = inner.as_ref().as_intermediate_type();
                registry.box_wrapper_name(&element)
            }
            WgslValueType::Struct(ty) | WgslValueType::Array(ty) => registry.wgsl_type(ty),
        }
    }

    fn as_intermediate_type(&self) -> IntermediateType {
        match self {
            WgslValueType::I32 | WgslValueType::Bool => IntermediateType::I32,
            WgslValueType::Box(inner) => IntermediateType::Box {
                element: Box::new(inner.as_intermediate_type()),
            },
            WgslValueType::Struct(ty) | WgslValueType::Array(ty) => ty.clone(),
        }
    }
}

struct TypeRegistry {
    next_id: usize,
    named_types: HashMap<String, String>,
    declarations: Vec<String>,
}

impl TypeRegistry {
    fn new() -> Self {
        Self {
            next_id: 0,
            named_types: HashMap::new(),
            declarations: Vec::new(),
        }
    }

    fn type_signature(&self, ty: &IntermediateType) -> String {
        match ty {
            IntermediateType::I32 => "i32".to_string(),
            IntermediateType::U8 => "u8".to_string(),
            IntermediateType::Box { element } => format!("box({})", self.type_signature(element)),
            IntermediateType::Struct(fields) => {
                let mut parts = Vec::with_capacity(fields.len());
                for (name, field_ty) in fields {
                    parts.push(format!("{}:{}", name, self.type_signature(field_ty)));
                }
                format!("struct{{{}}}", parts.join(","))
            }
            IntermediateType::Array {
                element,
                length,
                field_names,
            } => format!(
                "array[{}]{}{{{}}}",
                length,
                self.type_signature(element),
                field_names.join(",")
            ),
        }
    }

    fn wgsl_type(&mut self, ty: &IntermediateType) -> String {
        match ty {
            IntermediateType::I32 => "i32".to_string(),
            IntermediateType::U8 => "i32".to_string(),
            IntermediateType::Box { element } => self.box_wrapper_name(element),
            IntermediateType::Struct(fields) => {
                if fields.is_empty() {
                    "i32".to_string()
                } else {
                    self.struct_name(fields)
                }
            }
            IntermediateType::Array {
                element, length, ..
            } => {
                format!("array<{}, {}>", self.wgsl_type(element), length)
            }
        }
    }

    fn struct_name(&mut self, fields: &[(String, IntermediateType)]) -> String {
        let signature = {
            let ty = IntermediateType::Struct(fields.to_vec());
            self.type_signature(&ty)
        };
        if let Some(name) = self.named_types.get(&signature) {
            return name.clone();
        }
        let name = format!("SilkStruct{}", self.next_id);
        self.next_id += 1;
        let mut lines = Vec::new();
        lines.push(format!("struct {} {{", name));
        for (field_name, field_ty) in fields {
            lines.push(format!("  {}: {},", field_name, self.wgsl_type(field_ty)));
        }
        lines.push("};".to_string());
        self.declarations.push(lines.join("\n"));
        self.named_types.insert(signature, name.clone());
        name
    }

    fn box_wrapper_name(&mut self, element: &IntermediateType) -> String {
        let signature = format!("box_wrapper:{}", self.type_signature(element));
        if let Some(name) = self.named_types.get(&signature) {
            return name.clone();
        }
        let name = format!("SilkBox{}", self.next_id);
        self.next_id += 1;
        let element_ty = self.wgsl_type(element);
        let declaration = format!("struct {} {{\n  value: {},\n}};", name, element_ty);
        self.declarations.push(declaration);
        self.named_types.insert(signature, name.clone());
        name
    }

    fn buffer_wrapper_name(&mut self, element: &IntermediateType) -> String {
        let signature = format!("buffer_wrapper:{}", self.type_signature(element));
        if let Some(name) = self.named_types.get(&signature) {
            return name.clone();
        }
        let name = format!("SilkBuffer{}", self.next_id);
        self.next_id += 1;
        let element_ty = self.wgsl_type(element);
        let declaration = format!("struct {} {{\n  value: {},\n}};", name, element_ty);
        self.declarations.push(declaration);
        self.named_types.insert(signature, name.clone());
        name
    }
}
#[derive(Clone, Debug)]
struct StorageBinding {
    name: String,
    binding_index: u32,
    type_name: String,
}

#[derive(Clone, Debug)]
struct BoxInit {
    name: String,
    expr: String,
}

struct WgslModuleBuilder {
    type_registry: TypeRegistry,
    bindings: Vec<StorageBinding>,
    box_inits: Vec<BoxInit>,
    box_types: HashMap<String, IntermediateType>,
    globals: HashMap<String, IntermediateType>,
    next_binding: u32,
}

impl WgslModuleBuilder {
    fn new(intermediate: &IntermediateResult) -> Self {
        let mut globals = HashMap::new();
        for global in &intermediate.globals {
            globals.insert(global.name.clone(), global.ty.clone());
        }
        Self {
            type_registry: TypeRegistry::new(),
            bindings: Vec::new(),
            box_inits: Vec::new(),
            box_types: HashMap::new(),
            globals,
            next_binding: 0,
        }
    }

    fn register_binding(&mut self, name: String, type_name: String) -> u32 {
        let binding_index = self.next_binding;
        self.next_binding = self.next_binding.saturating_add(1);
        self.bindings.push(StorageBinding {
            name,
            binding_index,
            type_name,
        });
        binding_index
    }

    fn register_box_binding(
        &mut self,
        name: String,
        element_type: &IntermediateType,
        init_expr: String,
    ) -> Result<(), Diagnostic> {
        if self.box_types.contains_key(&name) {
            return Err(Diagnostic::new(format!(
                "Duplicate wgsl box binding `{}`",
                name
            )));
        }
        let type_name = self.type_registry.box_wrapper_name(element_type);
        self.register_binding(name.clone(), type_name);
        self.box_types.insert(name.clone(), element_type.clone());
        self.box_inits.push(BoxInit {
            name,
            expr: init_expr,
        });
        Ok(())
    }

    fn register_box_alloc(
        &mut self,
        value: &IntermediateKind,
        element_type: &IntermediateType,
    ) -> Result<String, Diagnostic> {
        let name = format!("silk_box_{}", self.box_types.len());
        let init_expr = compile_const_value(value, element_type, &mut self.type_registry)?;
        self.register_box_binding(name.clone(), element_type, init_expr)?;
        Ok(name)
    }
}

struct WgslValue {
    expr: String,
    ty: WgslValueType,
}

struct FunctionCompiler<'a> {
    builder: &'a mut WgslModuleBuilder,
    intermediate: &'a IntermediateResult,
    locals: HashMap<String, WgslValueType>,
    box_aliases: HashMap<String, String>,
    temp_counter: usize,
}
impl<'a> FunctionCompiler<'a> {
    fn new(builder: &'a mut WgslModuleBuilder, intermediate: &'a IntermediateResult) -> Self {
        Self {
            builder,
            intermediate,
            locals: HashMap::new(),
            box_aliases: HashMap::new(),
            temp_counter: 0,
        }
    }

    fn compile_function(&mut self, index: usize) -> Result<String, Diagnostic> {
        let function = &self.intermediate.functions[index];
        let (param_name, destructuring) =
            self.flatten_parameters(&function.parameter, index, function.input_type.as_ref())?;
        let param_type = self
            .builder
            .type_registry
            .wgsl_type(function.input_type.as_ref());
        let return_type = self
            .builder
            .type_registry
            .wgsl_type(function.return_type.as_ref());

        self.register_parameter_types(&function.parameter, function.input_type.as_ref());

        let mut lines = Vec::new();
        lines.push(format!(
            "fn silk_fn_{}({}: {}) -> {} {{",
            index, param_name, param_type, return_type
        ));
        for line in destructuring {
            lines.push(format!("  {}", line));
        }
        let mut body_lines = Vec::new();
        let result = self.emit_expr(&function.body, &mut body_lines, 1)?;
        for line in body_lines {
            lines.push(format!("  {}", line));
        }
        lines.push(format!("  return {};", result.expr));
        lines.push("}".to_string());
        Ok(lines.join("\n"))
    }

    fn flatten_parameters(
        &self,
        pattern: &BindingPattern,
        index: usize,
        input_type: &IntermediateType,
    ) -> Result<(String, Vec<String>), Diagnostic> {
        if let Some(name) = simple_pattern_name(pattern) {
            return Ok((name, Vec::new()));
        }
        let param_name = format!("silk_param_{}", index);
        let mut destructuring = Vec::new();
        self.emit_pattern_destructuring(pattern, input_type, &param_name, &mut destructuring)?;
        Ok((param_name, destructuring))
    }

    fn emit_pattern_destructuring(
        &self,
        pattern: &BindingPattern,
        value_type: &IntermediateType,
        source: &str,
        out: &mut Vec<String>,
    ) -> Result<(), Diagnostic> {
        match pattern {
            BindingPattern::Identifier(id, _) => {
                out.push(format!("let {} = {};", id.name, source));
            }
            BindingPattern::TypeHint(inner, _, _) => {
                self.emit_pattern_destructuring(inner, value_type, source, out)?;
            }
            BindingPattern::Annotated { pattern, .. } => {
                self.emit_pattern_destructuring(pattern, value_type, source, out)?;
            }
            BindingPattern::Struct(fields, _) => {
                let field_types = match value_type {
                    IntermediateType::Struct(fields) => fields.clone(),
                    IntermediateType::Array {
                        element,
                        field_names,
                        ..
                    } => field_names
                        .iter()
                        .cloned()
                        .map(|name| (name, (*element.clone())))
                        .collect(),
                    _ => {
                        return Err(Diagnostic::new(
                            "Struct pattern used on non-struct type".to_string(),
                        ));
                    }
                };
                for (position, (field_id, field_pattern)) in fields.iter().enumerate() {
                    let fallback_index = position;
                    let (property_name, field_type) = field_types
                        .iter()
                        .find(|(name, _)| name == &field_id.name)
                        .map(|(name, ty)| (name.clone(), ty.clone()))
                        .or_else(|| {
                            field_types
                                .get(fallback_index)
                                .map(|(name, ty)| (name.clone(), ty.clone()))
                        })
                        .ok_or_else(|| {
                            Diagnostic::new(format!(
                                "Missing field {} in struct pattern",
                                field_id.name
                            ))
                        })?;
                    let field_expr = format!("{}.{}", source, property_name);
                    self.emit_pattern_destructuring(field_pattern, &field_type, &field_expr, out)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn register_parameter_types(&mut self, pattern: &BindingPattern, ty: &IntermediateType) {
        match pattern {
            BindingPattern::Identifier(id, _) => {
                self.locals
                    .insert(id.name.clone(), WgslValueType::from_intermediate(ty));
            }
            BindingPattern::TypeHint(inner, _, _) => self.register_parameter_types(inner, ty),
            BindingPattern::Annotated { pattern, .. } => self.register_parameter_types(pattern, ty),
            BindingPattern::Struct(fields, _) => {
                let field_types = match ty {
                    IntermediateType::Struct(fields) => fields.clone(),
                    IntermediateType::Array {
                        element,
                        field_names,
                        ..
                    } => field_names
                        .iter()
                        .cloned()
                        .map(|name| (name, (*element.clone())))
                        .collect(),
                    _ => Vec::new(),
                };
                for (position, (field_id, field_pattern)) in fields.iter().enumerate() {
                    let fallback_index = position;
                    let field_type = field_types
                        .iter()
                        .find(|(name, _)| name == &field_id.name)
                        .map(|(_, ty)| ty.clone())
                        .or_else(|| field_types.get(fallback_index).map(|(_, ty)| ty.clone()))
                        .unwrap_or_else(|| IntermediateType::I32);
                    self.register_parameter_types(field_pattern, &field_type);
                }
            }
            _ => {}
        }
    }
    fn emit_expr(
        &mut self,
        expr: &IntermediateKind,
        lines: &mut Vec<String>,
        indent: usize,
    ) -> Result<WgslValue, Diagnostic> {
        match expr {
            IntermediateKind::Literal(lit) => match lit {
                ExpressionLiteral::Number(value) => Ok(WgslValue {
                    expr: value.to_string(),
                    ty: WgslValueType::I32,
                }),
                ExpressionLiteral::Boolean(value) => Ok(WgslValue {
                    expr: value.to_string(),
                    ty: WgslValueType::Bool,
                }),
                ExpressionLiteral::Char(value) => Ok(WgslValue {
                    expr: (*value as i32).to_string(),
                    ty: WgslValueType::I32,
                }),
                _ => Err(Diagnostic::new(
                    "Unsupported literal in wgsl backend".to_string(),
                )),
            },
            IntermediateKind::Identifier(identifier) => {
                let (expr, ty) = self.resolve_identifier(identifier)?;
                Ok(WgslValue { expr, ty })
            }
            IntermediateKind::Binding(binding) => {
                self.emit_binding(binding, lines, indent)?;
                Ok(WgslValue {
                    expr: "true".to_string(),
                    ty: WgslValueType::Bool,
                })
            }
            IntermediateKind::Block(exprs) => {
                if exprs.is_empty() {
                    return Ok(WgslValue {
                        expr: "0".to_string(),
                        ty: WgslValueType::I32,
                    });
                }
                let mut last_value = None;
                for (idx, expr) in exprs.iter().enumerate() {
                    let value = self.emit_expr(expr, lines, indent)?;
                    if idx == exprs.len() - 1 {
                        last_value = Some(value);
                    }
                }
                Ok(last_value.expect("block should have a value"))
            }
            IntermediateKind::IntrinsicOperation(op) => self.emit_intrinsic(op, lines, indent),
            IntermediateKind::If {
                condition,
                then_branch,
                else_branch,
            } => self.emit_if(condition, then_branch, else_branch, lines, indent),
            IntermediateKind::FunctionCall { function, argument } => {
                let arg_value = self.emit_expr(argument, lines, indent)?;
                let arg_value = self.unbox_value(arg_value);
                let return_type = self
                    .intermediate
                    .functions
                    .get(*function)
                    .map(|f| WgslValueType::from_intermediate(f.return_type.as_ref()))
                    .ok_or_else(|| {
                        Diagnostic::new("Unknown function call target in wgsl backend".to_string())
                    })?;
                Ok(WgslValue {
                    expr: format!("silk_fn_{}({})", function, arg_value.expr),
                    ty: return_type,
                })
            }
            IntermediateKind::Struct(fields) => {
                if fields.is_empty() {
                    return Ok(WgslValue {
                        expr: "0".to_string(),
                        ty: WgslValueType::I32,
                    });
                }
                let mut field_types = Vec::with_capacity(fields.len());
                let mut field_values = Vec::with_capacity(fields.len());
                for (field_id, field_expr) in fields {
                    let value = self.emit_expr(field_expr, lines, indent)?;
                    field_types.push((field_id.name.clone(), value.ty.as_intermediate_type()));
                    field_values.push((field_id.name.clone(), value.expr));
                }
                let struct_ty = IntermediateType::Struct(field_types);
                let struct_name = self.builder.type_registry.struct_name(match &struct_ty {
                    IntermediateType::Struct(fields) => fields,
                    _ => unreachable!(),
                });
                let mut parts = Vec::new();
                for (name, expr) in field_values {
                    parts.push(format!("{}: {}", name, expr));
                }
                Ok(WgslValue {
                    expr: format!("{}({})", struct_name, parts.join(", ")),
                    ty: WgslValueType::Struct(struct_ty),
                })
            }
            IntermediateKind::ArrayLiteral {
                items,
                element_type,
                field_names,
            } => {
                let mut item_exprs = Vec::with_capacity(items.len());
                for item in items {
                    let value = self.emit_expr(item, lines, indent)?;
                    item_exprs.push(self.unbox_value(value).expr);
                }
                let array_ty = IntermediateType::Array {
                    element: Box::new(element_type.clone()),
                    length: items.len(),
                    field_names: field_names.clone(),
                };
                let element_wgsl = self.builder.type_registry.wgsl_type(element_type);
                Ok(WgslValue {
                    expr: format!(
                        "array<{}, {}>({})",
                        element_wgsl,
                        items.len(),
                        item_exprs.join(", ")
                    ),
                    ty: WgslValueType::Array(array_ty),
                })
            }
            IntermediateKind::TypePropertyAccess { object, property } => {
                let value = self.emit_expr(object, lines, indent)?;
                let (object_expr, object_type) = self.unbox_value(value).split();
                match object_type {
                    WgslValueType::Struct(IntermediateType::Struct(fields)) => {
                        let field_type = fields
                            .iter()
                            .find(|(name, _)| name == property)
                            .map(|(_, ty)| ty.clone())
                            .ok_or_else(|| {
                                Diagnostic::new(format!("Field `{}` not found in struct", property))
                            })?;
                        Ok(WgslValue {
                            expr: format!("{}.{}", object_expr, property),
                            ty: WgslValueType::from_intermediate(&field_type),
                        })
                    }
                    WgslValueType::Array(IntermediateType::Array {
                        element,
                        field_names,
                        ..
                    }) => {
                        let index = field_names
                            .iter()
                            .position(|name| name == property)
                            .ok_or_else(|| {
                                Diagnostic::new(format!("Field `{}` not found in array", property))
                            })?;
                        Ok(WgslValue {
                            expr: format!("{}[{}]", object_expr, index),
                            ty: WgslValueType::from_intermediate(element.as_ref()),
                        })
                    }
                    _ => Err(Diagnostic::new(
                        "Type property access on non-struct type".to_string(),
                    )),
                }
            }
            IntermediateKind::ArrayIndex { array, index } => {
                let array_value = self.emit_expr(array, lines, indent)?;
                let index_value = self.emit_expr(index, lines, indent)?;
                let index_value = self.unbox_value(index_value);
                let (array_expr, array_type) = self.unbox_value(array_value).split();
                match array_type {
                    WgslValueType::Array(IntermediateType::Array { element, .. }) => {
                        Ok(WgslValue {
                            expr: format!("{}[{}]", array_expr, index_value.expr),
                            ty: WgslValueType::from_intermediate(element.as_ref()),
                        })
                    }
                    _ => Err(Diagnostic::new("Indexing on non-array type".to_string())),
                }
            }
            IntermediateKind::Assignment { target, expr } => {
                let value = self.emit_expr(expr, lines, indent)?;
                let value = self.unbox_value(value);
                let (lval, lval_type) = self.emit_lvalue(target, lines, indent)?;
                if matches!(lval_type, WgslValueType::Box(..)) {
                    return Err(Diagnostic::new(
                        "Assignment to boxed values is not supported in wgsl".to_string(),
                    ));
                }
                lines.push(format!("{}{} = {};", indent_pad(indent), lval, value.expr));
                Ok(WgslValue {
                    expr: value.expr,
                    ty: value.ty,
                })
            }
            IntermediateKind::BoxAlloc {
                value,
                element_type,
            } => {
                let name = self.builder.register_box_alloc(value, element_type)?;
                Ok(WgslValue {
                    expr: name,
                    ty: WgslValueType::Box(Box::new(WgslValueType::from_intermediate(
                        element_type,
                    ))),
                })
            }
            _ => Err(Diagnostic::new(
                "Unsupported expression type for wgsl backend".to_string(),
            )),
        }
    }
    fn emit_intrinsic(
        &mut self,
        op: &IntermediateIntrinsicOperation,
        lines: &mut Vec<String>,
        indent: usize,
    ) -> Result<WgslValue, Diagnostic> {
        match op {
            IntermediateIntrinsicOperation::Binary(left, right, op) => {
                let left_val = self.emit_expr(left, lines, indent)?;
                let right_val = self.emit_expr(right, lines, indent)?;
                let left_val = self.unbox_value(left_val);
                let right_val = self.unbox_value(right_val);
                let (left_expr, left_ty) = left_val.split();
                let (right_expr, right_ty) = right_val.split();
                let expr = match op {
                    BinaryIntrinsicOperator::I32Add => format!("({} + {})", left_expr, right_expr),
                    BinaryIntrinsicOperator::I32Subtract => {
                        format!("({} - {})", left_expr, right_expr)
                    }
                    BinaryIntrinsicOperator::I32Multiply => {
                        format!("({} * {})", left_expr, right_expr)
                    }
                    BinaryIntrinsicOperator::I32Divide => {
                        format!("({} / {})", left_expr, right_expr)
                    }
                    BinaryIntrinsicOperator::I32Equal => {
                        format!("({} == {})", left_expr, right_expr)
                    }
                    BinaryIntrinsicOperator::I32NotEqual => {
                        format!("({} != {})", left_expr, right_expr)
                    }
                    BinaryIntrinsicOperator::I32LessThan => {
                        format!("({} < {})", left_expr, right_expr)
                    }
                    BinaryIntrinsicOperator::I32GreaterThan => {
                        format!("({} > {})", left_expr, right_expr)
                    }
                    BinaryIntrinsicOperator::I32LessThanOrEqual => {
                        format!("({} <= {})", left_expr, right_expr)
                    }
                    BinaryIntrinsicOperator::I32GreaterThanOrEqual => {
                        format!("({} >= {})", left_expr, right_expr)
                    }
                    BinaryIntrinsicOperator::BooleanAnd => {
                        let left_expr = self.ensure_bool(left_expr, &left_ty);
                        let right_expr = self.ensure_bool(right_expr, &right_ty);
                        format!("({} && {})", left_expr, right_expr)
                    }
                    BinaryIntrinsicOperator::BooleanOr => {
                        let left_expr = self.ensure_bool(left_expr, &left_ty);
                        let right_expr = self.ensure_bool(right_expr, &right_ty);
                        format!("({} || {})", left_expr, right_expr)
                    }
                    BinaryIntrinsicOperator::BooleanXor => {
                        let left_expr = self.ensure_bool(left_expr, &left_ty);
                        let right_expr = self.ensure_bool(right_expr, &right_ty);
                        format!("({} != {})", left_expr, right_expr)
                    }
                };
                let ty = match op {
                    BinaryIntrinsicOperator::I32Equal
                    | BinaryIntrinsicOperator::I32NotEqual
                    | BinaryIntrinsicOperator::I32LessThan
                    | BinaryIntrinsicOperator::I32GreaterThan
                    | BinaryIntrinsicOperator::I32LessThanOrEqual
                    | BinaryIntrinsicOperator::I32GreaterThanOrEqual
                    | BinaryIntrinsicOperator::BooleanAnd
                    | BinaryIntrinsicOperator::BooleanOr
                    | BinaryIntrinsicOperator::BooleanXor => WgslValueType::Bool,
                    _ => WgslValueType::I32,
                };
                Ok(WgslValue { expr, ty })
            }
            _ => Err(Diagnostic::new(
                "Unsupported intrinsic operation in wgsl backend".to_string(),
            )),
        }
    }

    fn emit_if(
        &mut self,
        condition: &IntermediateKind,
        then_branch: &IntermediateKind,
        else_branch: &IntermediateKind,
        lines: &mut Vec<String>,
        indent: usize,
    ) -> Result<WgslValue, Diagnostic> {
        let cond_val = self.emit_expr(condition, lines, indent)?;
        let cond_val = self.unbox_value(cond_val);
        let cond_expr = self.ensure_bool(cond_val.expr, &cond_val.ty);

        let mut then_lines = Vec::new();
        let then_value = self.emit_expr(then_branch, &mut then_lines, indent + 1)?;
        let mut else_lines = Vec::new();
        let else_value = self.emit_expr(else_branch, &mut else_lines, indent + 1)?;

        if then_value.ty.to_wgsl_type(&mut self.builder.type_registry)
            != else_value.ty.to_wgsl_type(&mut self.builder.type_registry)
        {
            return Err(Diagnostic::new(
                "if branches must return the same type".to_string(),
            ));
        }

        let temp_name = format!("silk_tmp_{}", self.temp_counter);
        self.temp_counter += 1;
        let temp_type = then_value.ty.to_wgsl_type(&mut self.builder.type_registry);
        lines.push(format!(
            "{}var {}: {};",
            indent_pad(indent),
            temp_name,
            temp_type
        ));
        lines.push(format!("{}if ({}) {{", indent_pad(indent), cond_expr));
        for line in then_lines {
            lines.push(format!("{}{}", indent_pad(indent + 1), line));
        }
        lines.push(format!(
            "{}{} = {};",
            indent_pad(indent + 1),
            temp_name,
            then_value.expr
        ));
        lines.push(format!("{}}} else {{", indent_pad(indent)));
        for line in else_lines {
            lines.push(format!("{}{}", indent_pad(indent + 1), line));
        }
        lines.push(format!(
            "{}{} = {};",
            indent_pad(indent + 1),
            temp_name,
            else_value.expr
        ));
        lines.push(format!("{}}}", indent_pad(indent)));
        Ok(WgslValue {
            expr: temp_name,
            ty: then_value.ty,
        })
    }
    fn emit_binding(
        &mut self,
        binding: &IntermediateBinding,
        lines: &mut Vec<String>,
        indent: usize,
    ) -> Result<(), Diagnostic> {
        if matches!(binding.binding_type, IntermediateType::Box { .. }) {
            let binding_name = binding.identifier.name.clone();
            let box_name = self.resolve_box_expr(&binding.expr)?;
            self.box_aliases.insert(binding_name, box_name);
            self.locals.insert(
                binding.identifier.name.clone(),
                WgslValueType::from_intermediate(&binding.binding_type),
            );
            return Ok(());
        }
        let value = self.emit_expr(&binding.expr, lines, indent)?;
        let value = self.unbox_value(value);
        let var_type = WgslValueType::from_intermediate(&binding.binding_type);
        let var_type_str = var_type.to_wgsl_type(&mut self.builder.type_registry);
        lines.push(format!(
            "{}var {}: {} = {};",
            indent_pad(indent),
            binding.identifier.name,
            var_type_str,
            value.expr
        ));
        self.locals
            .insert(binding.identifier.name.clone(), var_type);
        Ok(())
    }

    fn resolve_box_expr(&mut self, expr: &IntermediateKind) -> Result<String, Diagnostic> {
        match expr {
            IntermediateKind::BoxAlloc {
                value,
                element_type,
            } => self.builder.register_box_alloc(value, element_type),
            IntermediateKind::Identifier(identifier) => {
                if let Some(alias) = self.box_aliases.get(&identifier.name) {
                    return Ok(alias.clone());
                }
                if let Some(ty) = self.builder.globals.get(&identifier.name) {
                    if matches!(ty, IntermediateType::Box { .. }) {
                        return Ok(identifier.name.clone());
                    }
                }
                Err(Diagnostic::new(format!(
                    "Unknown boxed identifier `{}` in wgsl lowering",
                    identifier.name
                )))
            }
            _ => Err(Diagnostic::new(
                "Unable to resolve boxed value at compile time".to_string(),
            )),
        }
    }

    fn resolve_identifier(
        &self,
        identifier: &Identifier,
    ) -> Result<(String, WgslValueType), Diagnostic> {
        if let Some(alias) = self.box_aliases.get(&identifier.name) {
            let ty = self.builder.box_types.get(alias).ok_or_else(|| {
                Diagnostic::new(format!(
                    "Unknown boxed identifier `{}` in wgsl backend",
                    alias
                ))
            })?;
            return Ok((
                alias.clone(),
                WgslValueType::Box(Box::new(WgslValueType::from_intermediate(ty))),
            ));
        }
        if let Some(ty) = self.locals.get(&identifier.name) {
            return Ok((identifier.name.clone(), ty.clone()));
        }
        if let Some(ty) = self.builder.box_types.get(&identifier.name) {
            return Ok((
                identifier.name.clone(),
                WgslValueType::Box(Box::new(WgslValueType::from_intermediate(ty))),
            ));
        }
        if let Some(ty) = self.builder.globals.get(&identifier.name) {
            return Ok((
                identifier.name.clone(),
                WgslValueType::from_intermediate(ty),
            ));
        }
        Err(Diagnostic::new(format!(
            "Unknown identifier `{}` in wgsl backend",
            identifier.name
        )))
    }

    fn emit_lvalue(
        &mut self,
        lvalue: &IntermediateLValue,
        lines: &mut Vec<String>,
        indent: usize,
    ) -> Result<(String, WgslValueType), Diagnostic> {
        match lvalue {
            IntermediateLValue::Identifier(id, _) => {
                let (expr, ty) = self.resolve_identifier(id)?;
                Ok((expr, ty))
            }
            IntermediateLValue::TypePropertyAccess {
                object, property, ..
            } => {
                let (obj_expr, obj_ty) = self.emit_lvalue(object, lines, indent)?;
                let obj_ty = self.unbox_type(obj_ty);
                match obj_ty {
                    WgslValueType::Struct(IntermediateType::Struct(fields)) => {
                        let field_ty = fields
                            .iter()
                            .find(|(name, _)| name == property)
                            .map(|(_, ty)| ty.clone())
                            .ok_or_else(|| {
                                Diagnostic::new(format!("Field `{}` not found in struct", property))
                            })?;
                        Ok((
                            format!("{}.{}", obj_expr, property),
                            WgslValueType::from_intermediate(&field_ty),
                        ))
                    }
                    _ => Err(Diagnostic::new(
                        "Type property access on non-struct type".to_string(),
                    )),
                }
            }
            IntermediateLValue::ArrayIndex { array, index, .. } => {
                let (array_expr, array_ty) = self.emit_lvalue(array, lines, indent)?;
                let index_value = self.emit_expr(index, lines, indent)?;
                let index_value = self.unbox_value(index_value);
                let array_ty = self.unbox_type(array_ty);
                match array_ty {
                    WgslValueType::Array(IntermediateType::Array { element, .. }) => Ok((
                        format!("{}[{}]", array_expr, index_value.expr),
                        WgslValueType::from_intermediate(element.as_ref()),
                    )),
                    _ => Err(Diagnostic::new("Indexing on non-array type".to_string())),
                }
            }
        }
    }

    fn unbox_value(&self, value: WgslValue) -> WgslValue {
        match value.ty {
            WgslValueType::Box(inner) => WgslValue {
                expr: format!("({}).value", value.expr),
                ty: *inner,
            },
            _ => value,
        }
    }

    fn unbox_type(&self, ty: WgslValueType) -> WgslValueType {
        match ty {
            WgslValueType::Box(inner) => *inner,
            other => other,
        }
    }

    fn ensure_bool(&self, expr: String, ty: &WgslValueType) -> String {
        match ty {
            WgslValueType::Bool => expr,
            _ => format!("({} != 0)", expr),
        }
    }
}
trait WgslValueExt {
    fn split(self) -> (String, WgslValueType);
}

impl WgslValueExt for WgslValue {
    fn split(self) -> (String, WgslValueType) {
        (self.expr, self.ty)
    }
}

fn simple_pattern_name(pattern: &BindingPattern) -> Option<String> {
    match pattern {
        BindingPattern::Identifier(id, _) => Some(id.name.clone()),
        BindingPattern::TypeHint(inner, _, _) => simple_pattern_name(inner),
        BindingPattern::Annotated { pattern, .. } => simple_pattern_name(pattern),
        _ => None,
    }
}

fn indent_pad(indent: usize) -> String {
    "  ".repeat(indent)
}
fn compile_const_value(
    value: &IntermediateKind,
    ty: &IntermediateType,
    registry: &mut TypeRegistry,
) -> Result<String, Diagnostic> {
    match ty {
        IntermediateType::I32 => match value {
            IntermediateKind::Literal(ExpressionLiteral::Number(val)) => Ok(val.to_string()),
            IntermediateKind::Literal(ExpressionLiteral::Boolean(val)) => {
                Ok(if *val { "1" } else { "0" }.to_string())
            }
            IntermediateKind::Literal(ExpressionLiteral::Char(val)) => {
                Ok((*val as i32).to_string())
            }
            _ => Err(Diagnostic::new(
                "Box allocation requires a constant i32 value".to_string(),
            )),
        },
        IntermediateType::U8 => match value {
            IntermediateKind::Literal(ExpressionLiteral::Char(val)) => {
                Ok((*val as i32).to_string())
            }
            IntermediateKind::Literal(ExpressionLiteral::Number(val)) => Ok(val.to_string()),
            _ => Err(Diagnostic::new(
                "Box allocation requires a constant u8 value".to_string(),
            )),
        },
        IntermediateType::Struct(fields) => {
            if fields.is_empty() {
                return Ok("0".to_string());
            }
            let IntermediateKind::Struct(items) = value else {
                return Err(Diagnostic::new(
                    "Box allocation requires a struct value".to_string(),
                ));
            };
            let struct_name = registry.struct_name(fields);
            let mut parts = Vec::new();
            for (name, field_ty) in fields {
                let field_value = items
                    .iter()
                    .find(|(id, _)| id.name == *name)
                    .map(|(_, expr)| expr)
                    .ok_or_else(|| {
                        Diagnostic::new(format!("Missing field {name} in struct value"))
                    })?;
                let field_expr = compile_const_value(field_value, field_ty, registry)?;
                parts.push(format!("{}: {}", name, field_expr));
            }
            Ok(format!("{}({})", struct_name, parts.join(", ")))
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
            let mut parts = Vec::new();
            for item in items {
                parts.push(compile_const_value(item, element, registry)?);
            }
            let element_ty = registry.wgsl_type(element);
            Ok(format!(
                "array<{}, {}>({})",
                element_ty,
                length,
                parts.join(", ")
            ))
        }
        IntermediateType::Box { .. } => Err(Diagnostic::new(
            "Nested box values are not supported in wgsl exports".to_string(),
        )),
    }
}

fn is_empty_struct(ty: &IntermediateType) -> bool {
    matches!(ty, IntermediateType::Struct(fields) if fields.is_empty())
}
pub fn compile_exports(intermediate: &IntermediateResult) -> Result<String, Diagnostic> {
    let wgsl_exports: Vec<_> = intermediate
        .exports
        .iter()
        .filter(|export| export.target == TargetLiteral::WgslTarget)
        .collect();

    if wgsl_exports.is_empty() {
        return Ok(String::new());
    }

    let mut builder = WgslModuleBuilder::new(intermediate);

    for export in &wgsl_exports {
        if export.export_type != IntermediateExportType::Global {
            continue;
        }
        let global = intermediate
            .globals
            .get(export.index)
            .ok_or_else(|| Diagnostic::new("Missing global export".to_string()))?;
        let IntermediateType::Box { element } = &global.ty else {
            return Err(Diagnostic::new(
                "WGSL global exports must be boxed values".to_string(),
            ));
        };
        let init_expr = match &global.value {
            IntermediateKind::BoxAlloc { value, .. } => {
                compile_const_value(value, element, &mut builder.type_registry)?
            }
            other => compile_const_value(other, element, &mut builder.type_registry)?,
        };
        builder.register_box_binding(global.name.clone(), element, init_expr)?;
    }

    let mut function_exports = Vec::new();
    for export in &wgsl_exports {
        if export.export_type != IntermediateExportType::Function {
            continue;
        }
        let function = intermediate
            .functions
            .get(export.index)
            .ok_or_else(|| Diagnostic::new("Missing function export".to_string()))?;
        let input_binding = if is_empty_struct(function.input_type.as_ref()) {
            None
        } else {
            let buffer_type = builder
                .type_registry
                .buffer_wrapper_name(function.input_type.as_ref());
            let name = format!("silk_in_{}", export.name);
            let binding_index = builder.register_binding(name.clone(), buffer_type);
            Some(StorageBinding {
                name,
                binding_index,
                type_name: String::new(),
            })
        };
        let output_binding = if is_empty_struct(function.return_type.as_ref()) {
            None
        } else {
            let output_buffer_type = builder
                .type_registry
                .buffer_wrapper_name(function.return_type.as_ref());
            let output_name = format!("silk_out_{}", export.name);
            let output_binding_index =
                builder.register_binding(output_name.clone(), output_buffer_type);
            Some(StorageBinding {
                name: output_name,
                binding_index: output_binding_index,
                type_name: String::new(),
            })
        };
        function_exports.push((
            export.name.clone(),
            export.index,
            input_binding,
            output_binding,
            function.clone(),
        ));
    }

    let mut functions = Vec::new();
    for (idx, _function) in intermediate.functions.iter().enumerate() {
        let mut compiler = FunctionCompiler::new(&mut builder, intermediate);
        let compiled = compiler.compile_function(idx)?;
        functions.push(compiled);
    }

    let mut output = String::new();
    for decl in &builder.type_registry.declarations {
        output.push_str(decl);
        output.push('\n');
    }

    for binding in &builder.bindings {
        output.push_str(&format!(
            "@group(0) @binding({}) var<storage, read_write> {}: {};\n",
            binding.binding_index, binding.name, binding.type_name
        ));
    }

    for func in functions {
        output.push('\n');
        output.push_str(&func);
        output.push('\n');
    }

    for (export_name, function_index, input_binding, output_binding, function) in function_exports {
        output.push('\n');
        output.push_str(&format!(
            "@compute @workgroup_size(1)\nfn {}() {{\n",
            export_name
        ));
        for init in &builder.box_inits {
            output.push_str(&format!("  {}.value = {};\n", init.name, init.expr));
        }
        let param_expr = if let Some(input_binding) = &input_binding {
            format!("{}.value", input_binding.name)
        } else if is_empty_struct(function.input_type.as_ref()) {
            "0".to_string()
        } else {
            "0".to_string()
        };
        if let Some(output_binding) = &output_binding {
            output.push_str(&format!(
                "  let result = silk_fn_{}({});\n",
                function_index, param_expr
            ));
            output.push_str(&format!("  {}.value = result;\n", output_binding.name));
        } else {
            output.push_str(&format!(
                "  let silk_unused = silk_fn_{}({});\n",
                function_index, param_expr
            ));
        }
        output.push_str("}\n");
    }

    Ok(output)
}
