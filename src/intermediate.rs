use std::collections::HashMap;

use crate::{
    SourceSpan,
    interpret::{self, BindingContext, Context, PreserveBehavior},
    parsing::{
        BinaryIntrinsicOperator, Binding, BindingAnnotation, BindingPattern, DivergeExpressionType,
        Expression, ExpressionKind, ExpressionLiteral, Identifier, IntrinsicType, LValue,
        TargetLiteral, UnaryIntrinsicOperator,
    },
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntermediateKind {
    IntrinsicOperation(IntermediateIntrinsicOperation),
    If {
        condition: Box<IntermediateKind>,
        then_branch: Box<IntermediateKind>,
        else_branch: Box<IntermediateKind>,
    },
    Struct(Vec<(Identifier, IntermediateKind)>),
    Literal(ExpressionLiteral),
    Identifier(Identifier),
    Assignment {
        target: LValue,
        expr: Box<IntermediateKind>,
    },
    FunctionCall {
        function: usize,
        argument: Box<IntermediateKind>,
    },
    PropertyAccess {
        object: Box<IntermediateKind>,
        property: String,
    },
    Binding(Box<IntermediateBinding>),
    Block(Vec<IntermediateKind>),
    Diverge {
        value: Box<IntermediateKind>,
        divergance_type: DivergeExpressionType,
    },
    Loop {
        body: Box<IntermediateKind>,
    },
    Unreachable,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntermediateBindingPattern {
    Identifier(Identifier, SourceSpan),
    Literal(ExpressionLiteral, SourceSpan),
    Struct(Vec<(Identifier, IntermediateBindingPattern)>, SourceSpan),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntermediateIntrinsicOperation {
    Binary(
        Box<IntermediateKind>,
        Box<IntermediateKind>,
        BinaryIntrinsicOperator,
    ),
    Unary(Box<IntermediateKind>, UnaryIntrinsicOperator),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntermediateBinding {
    pub pattern: IntermediateBindingPattern,
    pub binding_type: IntermediateType,
    pub expr: IntermediateKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntermediateType {
    I32,
    Struct(Vec<(String, IntermediateType)>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntermediateFunction {
    pub input_type: Box<IntermediateType>,
    pub return_type: Box<IntermediateType>,
    pub parameter: IntermediateBindingPattern,
    pub body: Box<IntermediateKind>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntermediateExportType {
    Function,
    Global,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntermediateExport {
    pub target: TargetLiteral,
    pub name: String,
    pub export_type: IntermediateExportType,
    pub index: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntermediateGlobal {
    pub name: String,
    pub ty: IntermediateType,
    pub value: IntermediateKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntermediateResult {
    pub functions: Vec<IntermediateFunction>,
    pub globals: Vec<IntermediateGlobal>,
    pub exports: Vec<IntermediateExport>,
}

pub fn context_to_intermediate(context: &Context) -> IntermediateResult {
    let mut builder = IntermediateBuilder::new(context.clone());
    builder.collect_type_aliases(context);
    builder.collect_bindings(context);

    IntermediateResult {
        functions: builder.functions,
        globals: builder.globals,
        exports: builder.exports,
    }
}

pub fn expression_to_intermediate(
    expr: Expression,
    builder: &mut IntermediateBuilder,
) -> IntermediateKind {
    let span = &expr.span;
    match expr.kind {
        ExpressionKind::IntrinsicOperation(op) => IntermediateKind::IntrinsicOperation(match op {
            crate::parsing::IntrinsicOperation::Binary(left, right, operator) => {
                IntermediateIntrinsicOperation::Binary(
                    Box::new(expression_to_intermediate(*left, builder)),
                    Box::new(expression_to_intermediate(*right, builder)),
                    operator,
                )
            }
            crate::parsing::IntrinsicOperation::Unary(operand, operator) => {
                IntermediateIntrinsicOperation::Unary(
                    Box::new(expression_to_intermediate(*operand, builder)),
                    operator,
                )
            }
        }),
        ExpressionKind::Match { value, branches } => {
            let match_value = *value;
            let match_value_span = match_value.span;
            let match_value_type = builder.expression_value_type(&match_value);
            let lowered_value = expression_to_intermediate(match_value, builder);
            let temp_identifier = builder.next_match_temp_identifier();

            let match_binding = IntermediateKind::Binding(Box::new(IntermediateBinding {
                pattern: IntermediateBindingPattern::Identifier(
                    temp_identifier.clone(),
                    match_value_span,
                ),
                binding_type: match_value_type.clone(),
                expr: lowered_value,
            }));

            let lowered_branches = branches
                .into_iter()
                .map(|(pattern, body)| (pattern, expression_to_intermediate(body, builder)))
                .collect::<Vec<_>>();

            let mut match_expr = IntermediateKind::Unreachable;
            for (pattern, branch) in lowered_branches.into_iter().rev() {
                let condition = builder.lower_binding_pattern_match(
                    pattern,
                    IntermediateKind::Identifier(temp_identifier.clone()),
                    match_value_type.clone(),
                );

                match_expr = IntermediateKind::If {
                    condition: Box::new(condition),
                    then_branch: Box::new(branch),
                    else_branch: Box::new(match_expr),
                };
            }

            IntermediateKind::Block(vec![match_binding, match_expr])
        }
        ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => IntermediateKind::If {
            condition: Box::new(expression_to_intermediate(*condition, builder)),
            then_branch: Box::new(expression_to_intermediate(*then_branch, builder)),
            else_branch: Box::new(expression_to_intermediate(*else_branch, builder)),
        },
        ExpressionKind::Struct(fields) => IntermediateKind::Struct(
            fields
                .into_iter()
                .map(|(id, value)| (id, expression_to_intermediate(value, builder)))
                .collect(),
        ),
        ExpressionKind::Literal(lit) => IntermediateKind::Literal(lit),
        ExpressionKind::Identifier(identifier) => {
            if let Some(inlined) = builder.inline_bindings.get(&identifier) {
                return expression_to_intermediate(inlined.clone(), builder);
            }
            IntermediateKind::Identifier(identifier)
        }
        ExpressionKind::Assignment { target, expr } => IntermediateKind::Assignment {
            target,
            expr: Box::new(expression_to_intermediate(*expr, builder)),
        },
        ExpressionKind::FunctionCall { function, argument } => {
            if let ExpressionKind::EnumConstructor {
                enum_type,
                variant,
                variant_index,
                ..
            } = &function.kind
            {
                let enum_value = ExpressionKind::EnumValue {
                    enum_type: enum_type.clone(),
                    variant: variant.clone(),
                    variant_index: *variant_index,
                    payload: argument.clone(),
                }
                .with_span(*span);
                return expression_to_intermediate(enum_value, builder);
            }
            if let ExpressionKind::EnumAccess { enum_expr, variant } = &function.kind
                && let Some(enum_type) = builder.resolve_enum_type_expression(enum_expr)
                && let Some((variant_index, _)) = enum_variant_info(&enum_type, variant)
            {
                let enum_value = ExpressionKind::EnumValue {
                    enum_type: Box::new(enum_type),
                    variant: variant.clone(),
                    variant_index,
                    payload: argument.clone(),
                }
                .with_span(*span);
                return expression_to_intermediate(enum_value, builder);
            }
            let function_index = match *function {
                Expression {
                    kind: ExpressionKind::Identifier(identifier),
                    ..
                } => builder
                    .resolve_function_index(&identifier)
                    .unwrap_or_else(|| {
                        panic!(
                            "Function `{}` was not lowered to an intermediate function",
                            identifier.name
                        )
                    }),
                function_expr @ Expression {
                    kind: ExpressionKind::Function { .. },
                    ..
                } => builder.register_function(None, function_expr),
                other => panic!(
                    "Unsupported function call target in intermediate lowering: {:?}",
                    other.kind
                ),
            };

            IntermediateKind::FunctionCall {
                function: function_index,
                argument: Box::new(expression_to_intermediate(*argument, builder)),
            }
        }
        ExpressionKind::PropertyAccess { object, property } => IntermediateKind::PropertyAccess {
            object: Box::new(expression_to_intermediate(*object, builder)),
            property,
        },
        ExpressionKind::Binding(binding) => {
            let Binding { pattern, expr } = *binding;
            let expr_span = expr.span;
            let binding_type = builder.infer_binding_type(&pattern, &expr);
            let stripped_pattern = builder.strip_binding_pattern(pattern.clone());
            let lowered_expr = expression_to_intermediate(expr, builder);

            if matches!(stripped_pattern, BindingPattern::EnumVariant { .. }) {
                let temp_identifier = builder.next_match_temp_identifier();
                let temp_binding = IntermediateKind::Binding(Box::new(IntermediateBinding {
                    pattern: IntermediateBindingPattern::Identifier(
                        temp_identifier.clone(),
                        expr_span,
                    ),
                    binding_type: binding_type.clone(),
                    expr: lowered_expr,
                }));
                let condition = builder.lower_binding_pattern_match(
                    pattern,
                    IntermediateKind::Identifier(temp_identifier.clone()),
                    binding_type,
                );
                IntermediateKind::Block(vec![temp_binding, condition])
            } else {
                IntermediateKind::Binding(Box::new(IntermediateBinding {
                    pattern: builder.lower_binding_pattern(pattern),
                    binding_type,
                    expr: lowered_expr,
                }))
            }
        }
        ExpressionKind::Block(expressions) => IntermediateKind::Block(
            expressions
                .into_iter()
                .map(|expr| expression_to_intermediate(expr, builder))
                .collect(),
        ),
        ExpressionKind::Diverge {
            value,
            divergance_type,
        } => IntermediateKind::Diverge {
            value: Box::new(expression_to_intermediate(*value, builder)),
            divergance_type,
        },
        ExpressionKind::Loop { body } => IntermediateKind::Loop {
            body: Box::new(expression_to_intermediate(*body, builder)),
        },
        ExpressionKind::EnumAccess { enum_expr, variant } => {
            if let Some(enum_type) = builder.resolve_enum_type_expression(&enum_expr)
                && let Some((variant_index, payload_type)) = enum_variant_info(&enum_type, &variant)
                && let ExpressionKind::Struct(fields) = &payload_type.kind
                && fields.is_empty()
            {
                let enum_value = ExpressionKind::EnumValue {
                    enum_type: Box::new(enum_type),
                    variant,
                    variant_index,
                    payload: Box::new(ExpressionKind::Struct(Vec::new()).with_span(*span)),
                }
                .with_span(*span);
                return expression_to_intermediate(enum_value, builder);
            }
            panic!("Unsupported enum constructor reference in intermediate lowering");
        }
        enum_expr @ ExpressionKind::EnumValue { .. } => {
            if let Some(lowered) = materialize_enum_value(&enum_expr, span) {
                expression_to_intermediate(lowered, builder)
            } else {
                IntermediateKind::Literal(ExpressionLiteral::Number(0))
            }
        }
        ExpressionKind::AttachImplementation { type_expr, .. } => {
            expression_to_intermediate(*type_expr, builder)
        }
        ExpressionKind::IntrinsicType(..)
        | ExpressionKind::EnumType(..)
        | ExpressionKind::EnumConstructor { .. }
        | ExpressionKind::Function { .. }
        | ExpressionKind::FunctionType { .. }
        | ExpressionKind::Operation { .. } => panic!("Unsupported type to intermediate lowering"),
    }
}

pub struct IntermediateBuilder {
    functions: Vec<IntermediateFunction>,
    globals: Vec<IntermediateGlobal>,
    exports: Vec<IntermediateExport>,
    function_indices: HashMap<Identifier, usize>,
    inline_bindings: HashMap<Identifier, Expression>,
    type_aliases: HashMap<Identifier, Expression>,
    enum_context: Context,
    match_temp_counter: usize,
}

impl IntermediateBuilder {
    fn new(enum_context: Context) -> Self {
        Self {
            functions: Vec::new(),
            globals: Vec::new(),
            exports: Vec::new(),
            function_indices: HashMap::new(),
            inline_bindings: HashMap::new(),
            type_aliases: HashMap::new(),
            enum_context,
            match_temp_counter: 0,
        }
    }

    fn next_match_temp_identifier(&mut self) -> Identifier {
        let name = format!("__match_val_{}", self.match_temp_counter);
        self.match_temp_counter += 1;
        Identifier::new(name)
    }

    fn collect_type_aliases(&mut self, context: &Context) {
        for scope in &context.bindings {
            for (identifier, (binding, _)) in scope {
                let BindingContext::Bound(value, preserve_behavior) = binding else {
                    continue;
                };

                if *preserve_behavior != PreserveBehavior::Inline {
                    continue;
                }

                if is_type_expression(value) {
                    self.type_aliases.insert(identifier.clone(), value.clone());
                }
            }
        }
    }

    fn collect_bindings(&mut self, context: &Context) {
        let Some(scope) = context.bindings.last() else {
            return;
        };

        for (identifier, (binding, annotations)) in scope {
            let BindingContext::Bound(value, preserve_behavior) = binding else {
                continue;
            };

            let export_targets = export_targets(annotations);
            let is_function = matches!(value.kind, ExpressionKind::Function { .. });
            let should_materialize =
                *preserve_behavior != PreserveBehavior::Inline || !export_targets.is_empty();

            if is_function && should_materialize {
                let index = self.register_function(Some(identifier.clone()), value.clone());
                for target in export_targets {
                    self.exports.push(IntermediateExport {
                        target,
                        name: identifier.name.clone(),
                        export_type: IntermediateExportType::Function,
                        index,
                    });
                }
                continue;
            }

            if !is_function && !export_targets.is_empty() {
                let index = self.register_global(identifier.clone(), value.clone());
                for target in export_targets {
                    self.exports.push(IntermediateExport {
                        target,
                        name: identifier.name.clone(),
                        export_type: IntermediateExportType::Global,
                        index,
                    });
                }
            }

            if *preserve_behavior == PreserveBehavior::Inline {
                self.inline_bindings
                    .insert(identifier.clone(), value.clone());
            }
        }
    }

    fn resolve_function_index(&mut self, identifier: &Identifier) -> Option<usize> {
        if let Some(index) = self.function_indices.get(identifier) {
            return Some(*index);
        }

        if let Some(value) = self.inline_bindings.get(identifier)
            && matches!(value.kind, ExpressionKind::Function { .. })
        {
            let index = self.register_function(Some(identifier.clone()), value.clone());
            return Some(index);
        }

        for scope in self.enum_context.bindings.iter().rev() {
            if let Some((BindingContext::Bound(value, _), _)) = scope.get(identifier)
                && matches!(value.kind, ExpressionKind::Function { .. })
            {
                let index = self.register_function(Some(identifier.clone()), value.clone());
                return Some(index);
            }
        }

        None
    }

    fn register_function(&mut self, name: Option<Identifier>, function_expr: Expression) -> usize {
        let ExpressionKind::Function {
            parameter,
            return_type,
            body,
        } = function_expr.kind
        else {
            panic!("Expected function expression for lowering");
        };
        let body_expr = *body;

        if let Some(name) = &name
            && let Some(index) = self.function_indices.get(name)
        {
            return *index;
        }

        let index = self.functions.len();
        if let Some(name) = name.clone() {
            self.function_indices.insert(name, index);
        }

        let lowered_parameter = self.lower_binding_pattern(parameter.clone());
        self.functions.push(IntermediateFunction {
            input_type: Box::new(IntermediateType::I32),
            return_type: Box::new(IntermediateType::I32),
            parameter: lowered_parameter.clone(),
            body: Box::new(IntermediateKind::Literal(ExpressionLiteral::Number(0))),
        });

        let input_type = Box::new(self.binding_pattern_type(&parameter));
        let return_type = Box::new(
            return_type
                .as_ref()
                .map(|ty| self.type_expr_to_intermediate(ty))
                .unwrap_or_else(|| self.expression_value_type(&body_expr)),
        );
        let lowered_body = Box::new(expression_to_intermediate(body_expr, self));

        self.functions[index] = IntermediateFunction {
            input_type,
            return_type,
            parameter: lowered_parameter,
            body: lowered_body,
        };

        index
    }

    fn register_global(&mut self, name: Identifier, value: Expression) -> usize {
        let ty = self.expression_value_type(&value);
        let lowered_value = expression_to_intermediate(value, self);
        let index = self.globals.len();
        self.globals.push(IntermediateGlobal {
            name: name.name,
            ty,
            value: lowered_value,
        });
        index
    }

    fn binding_pattern_type(&self, pattern: &BindingPattern) -> IntermediateType {
        match pattern {
            BindingPattern::Identifier(..) => IntermediateType::I32,
            BindingPattern::Literal(..) => IntermediateType::I32,
            BindingPattern::Struct(fields, _) => IntermediateType::Struct(
                fields
                    .iter()
                    .map(|(id, pat)| (id.name.clone(), self.binding_pattern_type(pat)))
                    .collect(),
            ),
            BindingPattern::EnumVariant { enum_type, .. } => {
                self.type_expr_to_intermediate(enum_type)
            }
            BindingPattern::TypeHint(_, type_expr, _) => self.type_expr_to_intermediate(type_expr),
            BindingPattern::Annotated { pattern, .. } => self.binding_pattern_type(pattern),
        }
    }

    fn infer_binding_type(
        &mut self,
        pattern: &BindingPattern,
        expr: &Expression,
    ) -> IntermediateType {
        match pattern {
            BindingPattern::EnumVariant { enum_type, .. } => {
                let enum_type = self
                    .resolve_enum_type_expression(enum_type)
                    .unwrap_or_else(|| *enum_type.clone());
                self.type_expr_to_intermediate(&enum_type)
            }
            BindingPattern::TypeHint(_, type_expr, _) => self.type_expr_to_intermediate(type_expr),
            BindingPattern::Annotated { pattern, .. } => self.infer_binding_type(pattern, expr),
            _ => self.expression_value_type(expr),
        }
    }

    fn expression_value_type(&mut self, expr: &Expression) -> IntermediateType {
        match &expr.kind {
            ExpressionKind::Literal(_) => IntermediateType::I32,
            ExpressionKind::IntrinsicOperation(_) => IntermediateType::I32,
            ExpressionKind::Struct(fields) => IntermediateType::Struct(
                fields
                    .iter()
                    .map(|(id, field_expr)| {
                        (id.name.clone(), self.expression_value_type(field_expr))
                    })
                    .collect(),
            ),
            ExpressionKind::If { then_branch, .. } => self.expression_value_type(then_branch),
            ExpressionKind::Block(exprs) => exprs
                .last()
                .map(|expr| self.expression_value_type(expr))
                .unwrap_or(IntermediateType::I32),
            ExpressionKind::Match { branches, .. } => branches
                .first()
                .map(|(_, expr)| self.expression_value_type(expr))
                .unwrap_or(IntermediateType::I32),
            ExpressionKind::EnumValue { enum_type, .. } => {
                let enum_type = self
                    .resolve_enum_type_expression(&enum_type)
                    .unwrap_or_else(|| *enum_type.clone());
                self.type_expr_to_intermediate(&enum_type)
            }
            ExpressionKind::EnumConstructor { enum_type, .. } => {
                let enum_type = self
                    .resolve_enum_type_expression(&enum_type)
                    .unwrap_or_else(|| *enum_type.clone());
                self.type_expr_to_intermediate(&enum_type)
            }
            _ => IntermediateType::I32,
        }
    }

    fn type_expr_to_intermediate(&self, expr: &Expression) -> IntermediateType {
        match &expr.kind {
            ExpressionKind::IntrinsicType(IntrinsicType::I32)
            | ExpressionKind::IntrinsicType(IntrinsicType::Boolean)
            | ExpressionKind::IntrinsicType(IntrinsicType::Target)
            | ExpressionKind::IntrinsicType(IntrinsicType::Type) => IntermediateType::I32,
            ExpressionKind::Struct(fields) => IntermediateType::Struct(
                fields
                    .iter()
                    .map(|(id, field_expr)| {
                        (id.name.clone(), self.type_expr_to_intermediate(field_expr))
                    })
                    .collect(),
            ),
            ExpressionKind::Identifier(identifier) => self
                .type_aliases
                .get(identifier)
                .map(|ty| self.type_expr_to_intermediate(ty))
                .unwrap_or(IntermediateType::I32),
            ExpressionKind::AttachImplementation { type_expr, .. } => {
                self.type_expr_to_intermediate(type_expr)
            }
            ExpressionKind::EnumType(variants) => {
                let payload = IntermediateType::Struct(
                    variants
                        .iter()
                        .map(|(name, ty)| (name.name.clone(), self.type_expr_to_intermediate(ty)))
                        .collect(),
                );
                IntermediateType::Struct(vec![
                    ("payload".to_string(), payload),
                    ("tag".to_string(), IntermediateType::I32),
                ])
            }
            _ => IntermediateType::I32,
        }
    }

    fn resolve_enum_type_expression(&mut self, enum_expr: &Expression) -> Option<Expression> {
        interpret::resolve_enum_type_expression(enum_expr, &mut self.enum_context)
    }

    fn strip_binding_pattern(&self, pattern: BindingPattern) -> BindingPattern {
        match pattern {
            BindingPattern::TypeHint(pattern, _, _) => self.strip_binding_pattern(*pattern),
            BindingPattern::Annotated { pattern, .. } => self.strip_binding_pattern(*pattern),
            other => other,
        }
    }

    fn lower_binding_pattern_match(
        &mut self,
        pattern: BindingPattern,
        value_expr: IntermediateKind,
        value_type: IntermediateType,
    ) -> IntermediateKind {
        match pattern {
            BindingPattern::TypeHint(pattern, _, _) => {
                self.lower_binding_pattern_match(*pattern, value_expr, value_type)
            }
            BindingPattern::Annotated { pattern, .. } => {
                self.lower_binding_pattern_match(*pattern, value_expr, value_type)
            }
            BindingPattern::EnumVariant {
                enum_type,
                variant,
                payload,
                span: _,
            } => {
                let enum_type = self
                    .resolve_enum_type_expression(&enum_type)
                    .unwrap_or(*enum_type.clone());
                let (variant_index, payload_type_expr) = enum_variant_info(&enum_type, &variant)
                    .unwrap_or_else(|| panic!("Unknown enum variant `{}`", variant.name));
                let payload_type = self.type_expr_to_intermediate(&payload_type_expr);

                let tag_expr = IntermediateKind::PropertyAccess {
                    object: Box::new(value_expr.clone()),
                    property: "tag".to_string(),
                };
                let tag_condition =
                    IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Binary(
                        Box::new(tag_expr),
                        Box::new(IntermediateKind::Literal(ExpressionLiteral::Number(
                            variant_index as i32,
                        ))),
                        BinaryIntrinsicOperator::I32Equal,
                    ));

                if let Some(payload_pattern) = payload {
                    let payload_access = IntermediateKind::PropertyAccess {
                        object: Box::new(IntermediateKind::PropertyAccess {
                            object: Box::new(value_expr),
                            property: "payload".to_string(),
                        }),
                        property: variant.name.clone(),
                    };
                    let payload_condition = self.lower_binding_pattern_match(
                        *payload_pattern,
                        payload_access,
                        payload_type,
                    );
                    IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Binary(
                        Box::new(tag_condition),
                        Box::new(payload_condition),
                        BinaryIntrinsicOperator::BooleanAnd,
                    ))
                } else {
                    tag_condition
                }
            }
            other => {
                let lowered_pattern = self.lower_binding_pattern(other);
                IntermediateKind::Binding(Box::new(IntermediateBinding {
                    pattern: lowered_pattern,
                    binding_type: value_type,
                    expr: value_expr,
                }))
            }
        }
    }

    fn lower_binding_pattern(&mut self, pattern: BindingPattern) -> IntermediateBindingPattern {
        match pattern {
            BindingPattern::Identifier(identifier, span) => {
                IntermediateBindingPattern::Identifier(identifier, span)
            }
            BindingPattern::Literal(literal, span) => {
                IntermediateBindingPattern::Literal(literal, span)
            }
            BindingPattern::Struct(fields, span) => IntermediateBindingPattern::Struct(
                fields
                    .into_iter()
                    .map(|(id, pat)| (id, self.lower_binding_pattern(pat)))
                    .collect(),
                span,
            ),
            BindingPattern::EnumVariant { .. } => {
                panic!("Enum binding patterns should be lowered via `lower_binding_pattern_match`")
            }
            BindingPattern::TypeHint(pattern, _, _) => self.lower_binding_pattern(*pattern),
            BindingPattern::Annotated { pattern, .. } => self.lower_binding_pattern(*pattern),
        }
    }
}

fn is_type_expression(expr: &Expression) -> bool {
    matches!(
        expr.kind,
        ExpressionKind::IntrinsicType(_)
            | ExpressionKind::Struct(_)
            | ExpressionKind::EnumType(_)
            | ExpressionKind::AttachImplementation { .. }
            | ExpressionKind::FunctionType { .. }
    )
}

fn export_targets(annotations: &[BindingAnnotation]) -> Vec<TargetLiteral> {
    let mut targets = Vec::new();
    for annotation in annotations {
        let BindingAnnotation::Export(expr, _) = annotation else {
            continue;
        };

        if let ExpressionKind::Literal(ExpressionLiteral::Target(target)) = &expr.kind {
            targets.push(target.clone());
        }
    }
    targets
}

fn default_value_for_type(ty: &Expression) -> Option<Expression> {
    match &ty.kind {
        ExpressionKind::IntrinsicType(IntrinsicType::I32 | IntrinsicType::Boolean) => {
            Some(ExpressionKind::Literal(ExpressionLiteral::Number(0)).with_span(ty.span))
        }
        ExpressionKind::Struct(fields) => {
            let mut values = Vec::new();
            for (id, field_ty) in fields {
                values.push((id.clone(), default_value_for_type(field_ty)?));
            }
            Some(Expression::new(ExpressionKind::Struct(values), ty.span))
        }
        ExpressionKind::AttachImplementation { type_expr, .. } => default_value_for_type(type_expr),
        ExpressionKind::EnumType(variants) => {
            if variants.is_empty() {
                return None;
            }
            let (first_variant, first_ty) = &variants[0];
            Some(
                ExpressionKind::EnumValue {
                    enum_type: Box::new(
                        ExpressionKind::EnumType(variants.clone()).with_span(ty.span),
                    ),
                    variant: first_variant.clone(),
                    variant_index: 0,
                    payload: Box::new(default_value_for_type(first_ty)?),
                }
                .with_span(ty.span),
            )
        }
        _ => None,
    }
}

fn materialize_enum_value(enum_value: &ExpressionKind, span: &SourceSpan) -> Option<Expression> {
    if let ExpressionKind::EnumValue {
        enum_type,
        variant: _variant,
        variant_index,
        payload,
    } = &enum_value
        && let ExpressionKind::EnumType(variants) = &enum_type.kind
    {
        let mut payload_fields = Vec::new();
        for (idx, (name, ty)) in variants.iter().enumerate() {
            let value = if idx == *variant_index {
                *payload.clone()
            } else {
                default_value_for_type(ty)?
            };
            payload_fields.push((Identifier::new(name.name.clone()), value));
        }

        let payload_struct = Expression::new(ExpressionKind::Struct(payload_fields), *span);
        let tag_expr = ExpressionKind::Literal(ExpressionLiteral::Number(*variant_index as i32))
            .with_span(*span);
        return Some(Expression::new(
            ExpressionKind::Struct(vec![
                (Identifier::new("payload".to_string()), payload_struct),
                (Identifier::new("tag".to_string()), tag_expr),
            ]),
            *span,
        ));
    }
    None
}

fn enum_variant_info(enum_type: &Expression, variant: &Identifier) -> Option<(usize, Expression)> {
    if let ExpressionKind::EnumType(variants) = &enum_type.kind {
        variants
            .iter()
            .enumerate()
            .find(|(_, (id, _))| id.name == variant.name)
            .map(|(idx, (_, ty))| (idx, ty.clone()))
    } else {
        None
    }
}
