use std::collections::HashMap;

use crate::{
    SourceSpan,
    interpret::{self, BindingContext, Context, PreserveBehavior, TraitPropSource},
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
    ArrayLiteral {
        items: Vec<IntermediateKind>,
        element_type: IntermediateType,
        field_names: Vec<String>,
    },
    Literal(ExpressionLiteral),
    Identifier(Identifier),
    Assignment {
        target: IntermediateLValue,
        expr: Box<IntermediateKind>,
    },
    FunctionCall {
        function: usize,
        argument: Box<IntermediateKind>,
    },
    ArrayIndex {
        array: Box<IntermediateKind>,
        index: Box<IntermediateKind>,
    },
    TypePropertyAccess {
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

fn binding_identifier(pattern: &BindingPattern) -> Option<Identifier> {
    match pattern {
        BindingPattern::Identifier(identifier, _) => Some(identifier.clone()),
        BindingPattern::TypeHint(inner, _, _) => binding_identifier(inner),
        BindingPattern::Annotated { pattern, .. } => binding_identifier(pattern),
        _ => None,
    }
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
    pub identifier: Identifier,
    pub binding_type: IntermediateType,
    pub expr: IntermediateKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntermediateLValue {
    Identifier(Identifier, SourceSpan),
    TypePropertyAccess {
        object: Box<IntermediateLValue>,
        property: String,
        span: SourceSpan,
    },
    ArrayIndex {
        array: Box<IntermediateLValue>,
        index: Box<IntermediateKind>,
        span: SourceSpan,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntermediateType {
    I32,
    U8,
    Struct(Vec<(String, IntermediateType)>),
    Array {
        element: Box<IntermediateType>,
        length: usize,
        field_names: Vec<String>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntermediateFunction {
    pub input_type: Box<IntermediateType>,
    pub return_type: Box<IntermediateType>,
    pub parameter: BindingPattern,
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

fn lower_lvalue(target: LValue, builder: &mut IntermediateBuilder) -> IntermediateLValue {
    match target {
        LValue::Identifier(identifier, span) => IntermediateLValue::Identifier(identifier, span),
        LValue::TypePropertyAccess {
            object,
            property,
            span,
        } => IntermediateLValue::TypePropertyAccess {
            object: Box::new(lower_lvalue(*object, builder)),
            property,
            span,
        },
        LValue::ArrayIndex { array, index, span } => IntermediateLValue::ArrayIndex {
            array: Box::new(lower_lvalue(*array, builder)),
            index: Box::new(expression_to_intermediate(*index, builder)),
            span,
        },
    }
}

pub fn expression_to_intermediate(
    expr: Expression,
    builder: &mut IntermediateBuilder,
) -> IntermediateKind {
    enum Frame {
        Enter(Expression),
        FinishIntrinsicBinary(BinaryIntrinsicOperator),
        FinishIntrinsicUnary(UnaryIntrinsicOperator),
        FinishIf,
        FinishStruct {
            field_ids: Vec<Identifier>,
            array_fields: Option<(Vec<String>, IntermediateType)>,
        },
        FinishAssignment(IntermediateLValue),
        FinishFunctionCall(usize),
        FinishArrayIndex,
        FinishTypePropertyAccess(String),
        FinishBinding {
            pattern: BindingPattern,
            binding_type: IntermediateType,
            is_complex: bool,
        },
        FinishBlock(usize),
        FinishMatch {
            match_value_type: IntermediateType,
            temp_identifier: Identifier,
            branch_patterns: Vec<BindingPattern>,
        },
        FinishDiverge(DivergeExpressionType),
        FinishLoop,
    }

    let mut stack = Vec::new();
    let mut values = Vec::new();
    stack.push(Frame::Enter(expr));

    while let Some(frame) = stack.pop() {
        match frame {
            Frame::Enter(expr) => {
                let span = expr.span;
                match expr.kind {
                    ExpressionKind::IntrinsicOperation(op) => match op {
                        crate::parsing::IntrinsicOperation::Binary(left, right, operator) => {
                            stack.push(Frame::FinishIntrinsicBinary(operator));
                            stack.push(Frame::Enter(*right));
                            stack.push(Frame::Enter(*left));
                        }
                        crate::parsing::IntrinsicOperation::Unary(operand, operator) => {
                            stack.push(Frame::FinishIntrinsicUnary(operator));
                            stack.push(Frame::Enter(*operand));
                        }
                    },
                    ExpressionKind::Match { value, branches } => {
                        let match_value = *value;
                        let match_value_type = builder.expression_value_type(&match_value);
                        let temp_identifier = builder.next_match_temp_identifier();
                        let branch_patterns = branches
                            .iter()
                            .map(|(pattern, _)| pattern.clone())
                            .collect();
                        stack.push(Frame::FinishMatch {
                            match_value_type,
                            temp_identifier,
                            branch_patterns,
                        });
                        for (_, branch_expr) in branches.into_iter().rev() {
                            stack.push(Frame::Enter(branch_expr));
                        }
                        stack.push(Frame::Enter(match_value));
                    }
                    ExpressionKind::If {
                        condition,
                        then_branch,
                        else_branch,
                    } => {
                        stack.push(Frame::FinishIf);
                        stack.push(Frame::Enter(*else_branch));
                        stack.push(Frame::Enter(*then_branch));
                        stack.push(Frame::Enter(*condition));
                    }
                    ExpressionKind::Struct(fields) => {
                        let field_ids = fields.iter().map(|(id, _)| id.clone()).collect();
                        let struct_expr = ExpressionKind::Struct(fields.clone()).with_span(span);
                        let array_fields = match builder.expression_value_type(&struct_expr) {
                            IntermediateType::Array {
                                field_names,
                                element,
                                ..
                            } => Some((field_names, *element)),
                            _ => None,
                        };
                        stack.push(Frame::FinishStruct {
                            field_ids,
                            array_fields,
                        });
                        for (_, field_expr) in fields.into_iter().rev() {
                            stack.push(Frame::Enter(field_expr));
                        }
                    }
                    ExpressionKind::Literal(lit) => match lit {
                        ExpressionLiteral::String(bytes) => {
                            let items = bytes
                                .iter()
                                .copied()
                                .map(|value| {
                                    IntermediateKind::Literal(ExpressionLiteral::Char(value))
                                })
                                .collect();
                            let field_names =
                                (0..bytes.len()).map(|index| index.to_string()).collect();
                            values.push(IntermediateKind::ArrayLiteral {
                                items,
                                element_type: IntermediateType::U8,
                                field_names,
                            });
                        }
                        _ => values.push(IntermediateKind::Literal(lit)),
                    },
                    ExpressionKind::Identifier(identifier) => {
                        if let Some(inlined) = builder.inline_bindings.get(&identifier).cloned() {
                            stack.push(Frame::Enter(inlined));
                        } else {
                            values.push(IntermediateKind::Identifier(identifier));
                        }
                    }
                    ExpressionKind::Assignment { target, expr } => {
                        let lowered_target = lower_lvalue(target, builder);
                        stack.push(Frame::FinishAssignment(lowered_target));
                        stack.push(Frame::Enter(*expr));
                    }
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
                            .with_span(span);
                            stack.push(Frame::Enter(enum_value));
                            continue;
                        }
                        if let ExpressionKind::TypePropertyAccess { object, property } =
                            &function.kind
                        {
                            if let Some(method_expr) = builder.resolve_impl_method(object, property)
                            {
                                let function_index = builder.register_function(None, method_expr);
                                stack.push(Frame::FinishFunctionCall(function_index));
                                stack.push(Frame::Enter(*argument));
                                continue;
                            }

                            if let Some(enum_type) = builder.resolve_enum_type_expression(object) {
                                let variant = Identifier::new(property.clone());
                                if let Some((variant_index, _)) =
                                    enum_variant_info(&enum_type, &variant)
                                {
                                    let enum_value = ExpressionKind::EnumValue {
                                        enum_type: Box::new(enum_type),
                                        variant,
                                        variant_index,
                                        payload: argument.clone(),
                                    }
                                    .with_span(span);
                                    stack.push(Frame::Enter(enum_value));
                                    continue;
                                }
                            }
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

                        stack.push(Frame::FinishFunctionCall(function_index));
                        stack.push(Frame::Enter(*argument));
                    }
                    ExpressionKind::ArrayIndex { array, index } => {
                        stack.push(Frame::FinishArrayIndex);
                        stack.push(Frame::Enter(*index));
                        stack.push(Frame::Enter(*array));
                    }
                    ExpressionKind::TypePropertyAccess { object, property } => {
                        stack.push(Frame::FinishTypePropertyAccess(property));
                        stack.push(Frame::Enter(*object));
                    }
                    ExpressionKind::Binding(binding) => {
                        let Binding { pattern, expr } = *binding;
                        if let Some(identifier) = binding_identifier(&pattern)
                            && let Some(scope) = builder.enum_context.bindings.last_mut()
                        {
                            scope.insert(
                                identifier,
                                (
                                    BindingContext::Bound(
                                        expr.clone(),
                                        PreserveBehavior::Inline,
                                        None,
                                    ),
                                    Vec::new(),
                                ),
                            );
                        }
                        let binding_type = builder.infer_binding_type(&pattern, &expr);
                        let stripped_pattern = builder.strip_binding_pattern(pattern.clone());
                        let is_complex = matches!(
                            stripped_pattern,
                            BindingPattern::EnumVariant { .. } | BindingPattern::Struct { .. }
                        );
                        stack.push(Frame::FinishBinding {
                            pattern,
                            binding_type,
                            is_complex,
                        });
                        stack.push(Frame::Enter(expr));
                    }
                    ExpressionKind::Block(expressions) => {
                        stack.push(Frame::FinishBlock(expressions.len()));
                        for expr in expressions.into_iter().rev() {
                            stack.push(Frame::Enter(expr));
                        }
                    }
                    ExpressionKind::Diverge {
                        value,
                        divergance_type,
                    } => {
                        stack.push(Frame::FinishDiverge(divergance_type));
                        stack.push(Frame::Enter(*value));
                    }
                    ExpressionKind::Loop { body } => {
                        stack.push(Frame::FinishLoop);
                        stack.push(Frame::Enter(*body));
                    }
                    enum_expr @ ExpressionKind::EnumValue { .. } => {
                        if let Some(lowered) = materialize_enum_value(&enum_expr, &span) {
                            stack.push(Frame::Enter(lowered));
                        } else {
                            values.push(IntermediateKind::Literal(ExpressionLiteral::Number(0)));
                        }
                    }
                    ExpressionKind::AttachImplementation { type_expr, .. } => {
                        stack.push(Frame::Enter(*type_expr));
                    }
                    ExpressionKind::IntrinsicType(..)
                    | ExpressionKind::EnumType(..)
                    | ExpressionKind::EnumConstructor { .. }
                    | ExpressionKind::Function { .. }
                    | ExpressionKind::FunctionType { .. }
                    | ExpressionKind::Operation { .. } => {
                        panic!("Unsupported type to intermediate lowering")
                    }
                }
            }
            Frame::FinishIntrinsicBinary(operator) => {
                let right = values.pop().expect("Missing binary operand");
                let left = values.pop().expect("Missing binary operand");
                values.push(IntermediateKind::IntrinsicOperation(
                    IntermediateIntrinsicOperation::Binary(
                        Box::new(left),
                        Box::new(right),
                        operator,
                    ),
                ));
            }
            Frame::FinishIntrinsicUnary(operator) => {
                let operand = values.pop().expect("Missing unary operand");
                values.push(IntermediateKind::IntrinsicOperation(
                    IntermediateIntrinsicOperation::Unary(Box::new(operand), operator),
                ));
            }
            Frame::FinishIf => {
                let else_branch = values.pop().expect("Missing if else branch");
                let then_branch = values.pop().expect("Missing if then branch");
                let condition = values.pop().expect("Missing if condition");
                values.push(IntermediateKind::If {
                    condition: Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                });
            }
            Frame::FinishStruct {
                field_ids,
                array_fields,
            } => {
                let mut field_values = Vec::with_capacity(field_ids.len());
                for _ in 0..field_ids.len() {
                    field_values.push(values.pop().expect("Missing struct field value"));
                }
                field_values.reverse();
                if let Some((field_names, element_type)) = array_fields {
                    values.push(IntermediateKind::ArrayLiteral {
                        items: field_values,
                        element_type,
                        field_names,
                    });
                } else {
                    let fields = field_ids
                        .into_iter()
                        .zip(field_values.into_iter())
                        .collect();
                    values.push(IntermediateKind::Struct(fields));
                }
            }
            Frame::FinishAssignment(target) => {
                let expr = values.pop().expect("Missing assignment value");
                values.push(IntermediateKind::Assignment {
                    target,
                    expr: Box::new(expr),
                });
            }
            Frame::FinishFunctionCall(function) => {
                let argument = values.pop().expect("Missing function call argument");
                values.push(IntermediateKind::FunctionCall {
                    function,
                    argument: Box::new(argument),
                });
            }
            Frame::FinishArrayIndex => {
                let index = values.pop().expect("Missing array index");
                let array = values.pop().expect("Missing array value");
                values.push(IntermediateKind::ArrayIndex {
                    array: Box::new(array),
                    index: Box::new(index),
                });
            }
            Frame::FinishTypePropertyAccess(property) => {
                let object = values.pop().expect("Missing property access object");
                values.push(IntermediateKind::TypePropertyAccess {
                    object: Box::new(object),
                    property,
                });
            }
            Frame::FinishBinding {
                pattern,
                binding_type,
                is_complex,
            } => {
                let lowered_expr = values.pop().expect("Missing binding expression");
                if is_complex {
                    let temp_identifier = builder.next_match_temp_identifier();
                    let temp_binding = IntermediateKind::Binding(Box::new(IntermediateBinding {
                        identifier: temp_identifier.clone(),
                        binding_type: binding_type.clone(),
                        expr: lowered_expr,
                    }));
                    let condition = builder.lower_binding_pattern_match(
                        pattern,
                        IntermediateKind::Identifier(temp_identifier),
                        binding_type,
                    );
                    values.push(IntermediateKind::Block(vec![temp_binding, condition]));
                } else {
                    let lowered =
                        builder.lower_binding_pattern_match(pattern, lowered_expr, binding_type);
                    values.push(lowered);
                }
            }
            Frame::FinishBlock(count) => {
                let mut items = Vec::with_capacity(count);
                for _ in 0..count {
                    items.push(values.pop().expect("Missing block expression"));
                }
                items.reverse();
                values.push(IntermediateKind::Block(items));
            }
            Frame::FinishMatch {
                match_value_type,
                temp_identifier,
                branch_patterns,
            } => {
                let mut lowered_branches = Vec::with_capacity(branch_patterns.len());
                for _ in 0..branch_patterns.len() {
                    lowered_branches.push(values.pop().expect("Missing match branch"));
                }
                lowered_branches.reverse();
                let lowered_value = values.pop().expect("Missing match value");

                let match_binding = IntermediateKind::Binding(Box::new(IntermediateBinding {
                    identifier: temp_identifier.clone(),
                    binding_type: match_value_type.clone(),
                    expr: lowered_value,
                }));

                let mut match_expr = IntermediateKind::Unreachable;
                for (pattern, branch) in branch_patterns
                    .into_iter()
                    .zip(lowered_branches.into_iter())
                    .rev()
                {
                    let condition = builder.lower_binding_pattern_match(
                        pattern,
                        IntermediateKind::Identifier(temp_identifier.clone()),
                        match_value_type.clone(),
                    );

                    match condition {
                        IntermediateKind::Binding(binding) => {
                            match_expr = IntermediateKind::Block(vec![
                                IntermediateKind::Binding(binding),
                                branch,
                            ]);
                        }
                        other => {
                            match_expr = IntermediateKind::If {
                                condition: Box::new(other),
                                then_branch: Box::new(branch),
                                else_branch: Box::new(match_expr),
                            };
                        }
                    }
                }

                values.push(IntermediateKind::Block(vec![match_binding, match_expr]));
            }
            Frame::FinishDiverge(divergance_type) => {
                let value = values.pop().expect("Missing diverge value");
                values.push(IntermediateKind::Diverge {
                    value: Box::new(value),
                    divergance_type,
                });
            }
            Frame::FinishLoop => {
                let body = values.pop().expect("Missing loop body");
                values.push(IntermediateKind::Loop {
                    body: Box::new(body),
                });
            }
        }
    }

    values.pop().expect("Missing lowered expression")
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
                let BindingContext::Bound(value, preserve_behavior, _) = binding else {
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
            let BindingContext::Bound(value, preserve_behavior, _) = binding else {
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
            if let Some((BindingContext::Bound(value, _, _), _)) = scope.get(identifier)
                && matches!(value.kind, ExpressionKind::Function { .. })
            {
                let index = self.register_function(Some(identifier.clone()), value.clone());
                return Some(index);
            }
        }

        None
    }

    fn resolve_impl_method(&mut self, object: &Expression, property: &str) -> Option<Expression> {
        let object_type = interpret::get_type_of_expression(object, &self.enum_context).ok()?;
        let (trait_prop, prop_source) = interpret::get_trait_prop_of_type(
            &object_type,
            property,
            object.span(),
            &self.enum_context,
        )
        .ok()?;
        if matches!(prop_source, TraitPropSource::ImplementationField)
            && matches!(trait_prop.kind, ExpressionKind::Function { .. })
        {
            Some(trait_prop)
        } else {
            None
        }
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

        let parameter_clone = parameter.clone();
        self.functions.push(IntermediateFunction {
            input_type: Box::new(IntermediateType::I32),
            return_type: Box::new(IntermediateType::I32),
            parameter: parameter_clone.clone(),
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
            parameter: parameter_clone,
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
        enum Frame {
            Enter(BindingPattern),
            FinishStruct(Vec<String>),
        }

        let mut stack = Vec::new();
        let mut values = Vec::new();
        stack.push(Frame::Enter(pattern.clone()));

        while let Some(frame) = stack.pop() {
            match frame {
                Frame::Enter(pattern) => match pattern {
                    BindingPattern::Identifier(..) | BindingPattern::Literal(..) => {
                        values.push(IntermediateType::I32);
                    }
                    BindingPattern::Struct(fields, _) => {
                        let field_names = fields.iter().map(|(id, _)| id.name.clone()).collect();
                        stack.push(Frame::FinishStruct(field_names));
                        for (_, pat) in fields.into_iter().rev() {
                            stack.push(Frame::Enter(pat));
                        }
                    }
                    BindingPattern::EnumVariant { enum_type, .. } => {
                        values.push(self.type_expr_to_intermediate(&enum_type));
                    }
                    BindingPattern::TypeHint(_, type_expr, _) => {
                        values.push(self.type_expr_to_intermediate(&type_expr));
                    }
                    BindingPattern::Annotated { pattern, .. } => {
                        stack.push(Frame::Enter(*pattern));
                    }
                },
                Frame::FinishStruct(field_names) => {
                    let mut field_types = Vec::with_capacity(field_names.len());
                    for _ in 0..field_names.len() {
                        field_types.push(values.pop().expect("Missing struct field type"));
                    }
                    field_types.reverse();
                    let first_type = field_types.first().cloned();
                    let mut homogeneous = true;
                    if let Some(first) = &first_type {
                        for ty in field_types.iter().skip(1) {
                            if ty != first {
                                homogeneous = false;
                                break;
                            }
                        }
                    } else {
                        homogeneous = false;
                    }

                    if let Some(first) = first_type
                        && homogeneous
                    {
                        values.push(IntermediateType::Array {
                            element: Box::new(first),
                            length: field_types.len(),
                            field_names,
                        });
                    } else {
                        let fields = field_names
                            .into_iter()
                            .zip(field_types.into_iter())
                            .collect();
                        values.push(IntermediateType::Struct(fields));
                    }
                }
            }
        }

        values.pop().unwrap_or(IntermediateType::I32)
    }

    fn infer_binding_type(
        &mut self,
        pattern: &BindingPattern,
        expr: &Expression,
    ) -> IntermediateType {
        let mut current = pattern;
        loop {
            match current {
                BindingPattern::EnumVariant { enum_type, .. } => {
                    let enum_type = self
                        .resolve_enum_type_expression(enum_type)
                        .unwrap_or_else(|| *enum_type.clone());
                    return self.type_expr_to_intermediate(&enum_type);
                }
                BindingPattern::TypeHint(_, type_expr, _) => {
                    return self.type_expr_to_intermediate(type_expr);
                }
                BindingPattern::Annotated { pattern, .. } => {
                    current = pattern;
                }
                _ => return self.expression_value_type(expr),
            }
        }
    }

    fn expression_value_type(&mut self, expr: &Expression) -> IntermediateType {
        enum Frame {
            Enter(Expression),
            FinishStruct(Vec<String>),
            FinishArrayIndex,
        }

        let mut stack = Vec::new();
        let mut values = Vec::new();
        stack.push(Frame::Enter(expr.clone()));

        while let Some(frame) = stack.pop() {
            match frame {
                Frame::Enter(expr) => match expr.kind {
                    ExpressionKind::Literal(lit) => {
                        let value = match lit {
                            ExpressionLiteral::Char(_) => IntermediateType::U8,
                            ExpressionLiteral::String(bytes) => IntermediateType::Array {
                                element: Box::new(IntermediateType::U8),
                                length: bytes.len(),
                                field_names: (0..bytes.len())
                                    .map(|index| index.to_string())
                                    .collect(),
                            },
                            ExpressionLiteral::Number(_)
                            | ExpressionLiteral::Boolean(_)
                            | ExpressionLiteral::Target(_) => IntermediateType::I32,
                        };
                        values.push(value);
                    }
                    ExpressionKind::IntrinsicOperation(_) => {
                        values.push(IntermediateType::I32);
                    }
                    ExpressionKind::Struct(fields) => {
                        let field_names = fields.iter().map(|(id, _)| id.name.clone()).collect();
                        stack.push(Frame::FinishStruct(field_names));
                        for (_, field_expr) in fields.into_iter().rev() {
                            stack.push(Frame::Enter(field_expr));
                        }
                    }
                    ExpressionKind::ArrayIndex { array, .. } => {
                        stack.push(Frame::FinishArrayIndex);
                        stack.push(Frame::Enter(*array));
                    }
                    ExpressionKind::If { then_branch, .. } => {
                        stack.push(Frame::Enter(*then_branch));
                    }
                    ExpressionKind::Block(exprs) => {
                        if let Some(last) = exprs.last() {
                            stack.push(Frame::Enter(last.clone()));
                        } else {
                            values.push(IntermediateType::I32);
                        }
                    }
                    ExpressionKind::Match { branches, .. } => {
                        if let Some((_, first_expr)) = branches.first() {
                            stack.push(Frame::Enter(first_expr.clone()));
                        } else {
                            values.push(IntermediateType::I32);
                        }
                    }
                    ExpressionKind::EnumValue { enum_type, .. }
                    | ExpressionKind::EnumConstructor { enum_type, .. } => {
                        let enum_type = self
                            .resolve_enum_type_expression(&enum_type)
                            .unwrap_or(*enum_type);
                        values.push(self.type_expr_to_intermediate(&enum_type));
                    }
                    _ => values.push(IntermediateType::I32),
                },
                Frame::FinishStruct(field_names) => {
                    let mut field_types = Vec::with_capacity(field_names.len());
                    for _ in 0..field_names.len() {
                        field_types.push(values.pop().expect("Missing struct field type"));
                    }
                    field_types.reverse();
                    let first_type = field_types.first().cloned();
                    let mut homogeneous = true;
                    if let Some(first) = &first_type {
                        for ty in field_types.iter().skip(1) {
                            if ty != first {
                                homogeneous = false;
                                break;
                            }
                        }
                    } else {
                        homogeneous = false;
                    }

                    if let Some(first) = first_type
                        && homogeneous
                    {
                        values.push(IntermediateType::Array {
                            element: Box::new(first),
                            length: field_types.len(),
                            field_names,
                        });
                    } else {
                        let fields = field_names
                            .into_iter()
                            .zip(field_types.into_iter())
                            .collect();
                        values.push(IntermediateType::Struct(fields));
                    }
                }
                Frame::FinishArrayIndex => {
                    let array_type = values.pop().expect("Missing array type");
                    if let IntermediateType::Array { element, .. } = array_type {
                        values.push(*element);
                        continue;
                    }
                    values.push(IntermediateType::I32);
                }
            }
        }

        values.pop().unwrap_or(IntermediateType::I32)
    }

    fn type_expr_to_intermediate(&self, expr: &Expression) -> IntermediateType {
        enum Frame {
            Enter(Expression),
            FinishStruct(Vec<String>),
            FinishEnumType(Vec<String>),
        }

        let mut stack = Vec::new();
        let mut values = Vec::new();
        stack.push(Frame::Enter(expr.clone()));

        while let Some(frame) = stack.pop() {
            match frame {
                Frame::Enter(expr) => match expr.kind {
                    ExpressionKind::IntrinsicType(IntrinsicType::I32)
                    | ExpressionKind::IntrinsicType(IntrinsicType::Boolean)
                    | ExpressionKind::IntrinsicType(IntrinsicType::Target)
                    | ExpressionKind::IntrinsicType(IntrinsicType::Type) => {
                        values.push(IntermediateType::I32);
                    }
                    ExpressionKind::IntrinsicType(IntrinsicType::U8) => {
                        values.push(IntermediateType::U8);
                    }
                    ExpressionKind::Struct(fields) => {
                        let field_names = fields.iter().map(|(id, _)| id.name.clone()).collect();
                        stack.push(Frame::FinishStruct(field_names));
                        for (_, field_expr) in fields.into_iter().rev() {
                            stack.push(Frame::Enter(field_expr));
                        }
                    }
                    ExpressionKind::Identifier(identifier) => {
                        if let Some(ty) = self.type_aliases.get(&identifier).cloned() {
                            stack.push(Frame::Enter(ty));
                        } else {
                            values.push(IntermediateType::I32);
                        }
                    }
                    ExpressionKind::AttachImplementation { type_expr, .. } => {
                        stack.push(Frame::Enter(*type_expr));
                    }
                    ExpressionKind::EnumType(variants) => {
                        let variant_names =
                            variants.iter().map(|(name, _)| name.name.clone()).collect();
                        stack.push(Frame::FinishEnumType(variant_names));
                        for (_, ty) in variants.into_iter().rev() {
                            stack.push(Frame::Enter(ty));
                        }
                    }
                    _ => values.push(IntermediateType::I32),
                },
                Frame::FinishStruct(field_names) => {
                    let mut field_types = Vec::with_capacity(field_names.len());
                    for _ in 0..field_names.len() {
                        field_types.push(values.pop().expect("Missing struct field type"));
                    }
                    field_types.reverse();
                    let first_type = field_types.first().cloned();
                    let mut homogeneous = true;
                    if let Some(first) = &first_type {
                        for ty in field_types.iter().skip(1) {
                            if ty != first {
                                homogeneous = false;
                                break;
                            }
                        }
                    } else {
                        homogeneous = false;
                    }

                    if let Some(first) = first_type
                        && homogeneous
                    {
                        values.push(IntermediateType::Array {
                            element: Box::new(first),
                            length: field_types.len(),
                            field_names,
                        });
                    } else {
                        let fields = field_names
                            .into_iter()
                            .zip(field_types.into_iter())
                            .collect();
                        values.push(IntermediateType::Struct(fields));
                    }
                }
                Frame::FinishEnumType(variant_names) => {
                    let mut variant_types = Vec::with_capacity(variant_names.len());
                    for _ in 0..variant_names.len() {
                        variant_types.push(values.pop().expect("Missing enum variant type"));
                    }
                    variant_types.reverse();
                    let payload_fields = variant_names
                        .into_iter()
                        .zip(variant_types.into_iter())
                        .collect();
                    let payload = IntermediateType::Struct(payload_fields);
                    values.push(IntermediateType::Struct(vec![
                        ("payload".to_string(), payload),
                        ("tag".to_string(), IntermediateType::I32),
                    ]));
                }
            }
        }

        values.pop().unwrap_or(IntermediateType::I32)
    }

    fn resolve_enum_type_expression(&mut self, enum_expr: &Expression) -> Option<Expression> {
        interpret::resolve_enum_type_expression(enum_expr, &mut self.enum_context)
    }

    fn strip_binding_pattern(&self, pattern: BindingPattern) -> BindingPattern {
        let mut current = pattern;
        loop {
            match current {
                BindingPattern::TypeHint(pattern, _, _) => current = *pattern,
                BindingPattern::Annotated { pattern, .. } => current = *pattern,
                other => return other,
            }
        }
    }

    fn lower_binding_pattern_match(
        &mut self,
        pattern: BindingPattern,
        value_expr: IntermediateKind,
        value_type: IntermediateType,
    ) -> IntermediateKind {
        enum Frame {
            Enter {
                pattern: BindingPattern,
                value_expr: IntermediateKind,
                value_type: IntermediateType,
            },
            FinishStruct(usize),
            FinishEnumPayload(IntermediateKind),
        }

        let mut stack = Vec::new();
        let mut values = Vec::new();
        stack.push(Frame::Enter {
            pattern,
            value_expr,
            value_type,
        });

        while let Some(frame) = stack.pop() {
            match frame {
                Frame::Enter {
                    pattern,
                    value_expr,
                    value_type,
                } => match pattern {
                    BindingPattern::TypeHint(pattern, _, _)
                    | BindingPattern::Annotated { pattern, .. } => {
                        stack.push(Frame::Enter {
                            pattern: *pattern,
                            value_expr,
                            value_type,
                        });
                    }
                    BindingPattern::Struct(fields, span) => {
                        let field_types = match value_type {
                            IntermediateType::Struct(fields) => fields,
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
                                panic!("Struct pattern used on non-struct type at {:?}", span)
                            }
                        };

                        let field_count = fields.len();
                        stack.push(Frame::FinishStruct(field_count));
                        for (position, (field_identifier, field_pattern)) in
                            fields.into_iter().rev().enumerate()
                        {
                            let fallback_index = field_count.saturating_sub(position + 1);
                            let (property_name, field_type) = field_types
                                .iter()
                                .find(|(name, _)| name == &field_identifier.name)
                                .map(|(name, ty)| (name.clone(), ty.clone()))
                                .or_else(|| {
                                    field_types
                                        .get(fallback_index)
                                        .map(|(name, ty)| (name.clone(), ty.clone()))
                                })
                                .unwrap_or_else(|| {
                                    panic!(
                                        "Missing field {} in struct pattern at {:?}",
                                        field_identifier.name, span
                                    )
                                });
                            let field_expr = IntermediateKind::TypePropertyAccess {
                                object: Box::new(value_expr.clone()),
                                property: property_name,
                            };
                            stack.push(Frame::Enter {
                                pattern: field_pattern,
                                value_expr: field_expr,
                                value_type: field_type,
                            });
                        }
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
                        let (variant_index, payload_type_expr) =
                            enum_variant_info(&enum_type, &variant).unwrap_or_else(|| {
                                panic!("Unknown enum variant `{}`", variant.name)
                            });
                        let payload_type = self.type_expr_to_intermediate(&payload_type_expr);

                        let tag_expr = IntermediateKind::TypePropertyAccess {
                            object: Box::new(value_expr.clone()),
                            property: "tag".to_string(),
                        };
                        let tag_condition = IntermediateKind::IntrinsicOperation(
                            IntermediateIntrinsicOperation::Binary(
                                Box::new(tag_expr),
                                Box::new(IntermediateKind::Literal(ExpressionLiteral::Number(
                                    variant_index as i32,
                                ))),
                                BinaryIntrinsicOperator::I32Equal,
                            ),
                        );

                        if let Some(payload_pattern) = payload {
                            let payload_access = IntermediateKind::TypePropertyAccess {
                                object: Box::new(IntermediateKind::TypePropertyAccess {
                                    object: Box::new(value_expr),
                                    property: "payload".to_string(),
                                }),
                                property: variant.name.clone(),
                            };
                            stack.push(Frame::FinishEnumPayload(tag_condition));
                            stack.push(Frame::Enter {
                                pattern: *payload_pattern,
                                value_expr: payload_access,
                                value_type: payload_type,
                            });
                        } else {
                            values.push(tag_condition);
                        }
                    }
                    BindingPattern::Literal(literal, span) => {
                        let literal = match literal {
                            ExpressionLiteral::Number(_) => literal,
                            ExpressionLiteral::Boolean(_) => literal,
                            ExpressionLiteral::Char(_) => literal,
                            other => panic!(
                                "Unsupported literal pattern in binding lowering: {:?} at {:?}",
                                other, span
                            ),
                        };
                        values.push(IntermediateKind::IntrinsicOperation(
                            IntermediateIntrinsicOperation::Binary(
                                Box::new(value_expr),
                                Box::new(IntermediateKind::Literal(literal)),
                                BinaryIntrinsicOperator::I32Equal,
                            ),
                        ));
                    }
                    other => {
                        let identifier = self.lower_binding_pattern(other);
                        values.push(IntermediateKind::Binding(Box::new(IntermediateBinding {
                            identifier,
                            binding_type: value_type,
                            expr: value_expr,
                        })));
                    }
                },
                Frame::FinishStruct(count) => {
                    if count == 0 {
                        values.push(IntermediateKind::Literal(ExpressionLiteral::Boolean(true)));
                        continue;
                    }
                    let mut conditions = Vec::with_capacity(count);
                    for _ in 0..count {
                        conditions.push(values.pop().expect("Missing struct condition"));
                    }
                    conditions.reverse();
                    let mut iter = conditions.into_iter();
                    let mut combined = iter.next().expect("Missing struct condition");
                    for condition in iter {
                        combined = IntermediateKind::IntrinsicOperation(
                            IntermediateIntrinsicOperation::Binary(
                                Box::new(combined),
                                Box::new(condition),
                                BinaryIntrinsicOperator::BooleanAnd,
                            ),
                        );
                    }
                    values.push(combined);
                }
                Frame::FinishEnumPayload(tag_condition) => {
                    let payload_condition = values.pop().expect("Missing enum payload condition");
                    values.push(IntermediateKind::IntrinsicOperation(
                        IntermediateIntrinsicOperation::Binary(
                            Box::new(tag_condition),
                            Box::new(payload_condition),
                            BinaryIntrinsicOperator::BooleanAnd,
                        ),
                    ));
                }
            }
        }

        values.pop().expect("Missing lowered binding pattern match")
    }

    fn lower_binding_pattern(&mut self, pattern: BindingPattern) -> Identifier {
        let mut current = pattern;
        loop {
            match current {
                BindingPattern::Identifier(identifier, _) => return identifier,
                BindingPattern::Struct { .. } => {
                    panic!(
                        "Struct binding patterns should be lowered via `lower_binding_pattern_match`"
                    )
                }
                BindingPattern::EnumVariant { .. } => {
                    panic!(
                        "Enum binding patterns should be lowered via `lower_binding_pattern_match`"
                    )
                }
                BindingPattern::TypeHint(pattern, _, _)
                | BindingPattern::Annotated { pattern, .. } => {
                    current = *pattern;
                }
                other @ BindingPattern::Literal(..) => {
                    panic!("Unexpected pattern {:?} for identifier binding", other)
                }
            }
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
    enum Frame {
        Enter(Expression),
        FinishStruct(Vec<Identifier>, SourceSpan),
        FinishEnumType {
            enum_type: Expression,
            first_variant: Identifier,
            span: SourceSpan,
        },
    }

    let mut stack = Vec::new();
    let mut values: Vec<Option<Expression>> = Vec::new();
    stack.push(Frame::Enter(ty.clone()));

    while let Some(frame) = stack.pop() {
        match frame {
            Frame::Enter(expr) => match expr.kind {
                ExpressionKind::IntrinsicType(
                    IntrinsicType::I32 | IntrinsicType::U8 | IntrinsicType::Boolean,
                ) => {
                    values.push(Some(
                        ExpressionKind::Literal(ExpressionLiteral::Number(0)).with_span(expr.span),
                    ));
                }
                ExpressionKind::Struct(fields) => {
                    let field_ids = fields.iter().map(|(id, _)| id.clone()).collect();
                    stack.push(Frame::FinishStruct(field_ids, expr.span));
                    for (_, field_ty) in fields.into_iter().rev() {
                        stack.push(Frame::Enter(field_ty));
                    }
                }
                ExpressionKind::AttachImplementation { type_expr, .. } => {
                    stack.push(Frame::Enter(*type_expr));
                }
                ExpressionKind::EnumType(variants) => {
                    if variants.is_empty() {
                        values.push(None);
                    } else {
                        let (first_variant, first_ty) = &variants[0];
                        let first_variant = first_variant.clone();
                        let first_ty = first_ty.clone();
                        let enum_type_expr =
                            ExpressionKind::EnumType(variants).with_span(expr.span);
                        stack.push(Frame::FinishEnumType {
                            enum_type: enum_type_expr,
                            first_variant,
                            span: expr.span,
                        });
                        stack.push(Frame::Enter(first_ty));
                    }
                }
                _ => values.push(None),
            },
            Frame::FinishStruct(field_ids, span) => {
                let mut field_values = Vec::with_capacity(field_ids.len());
                let mut ok = true;
                for _ in 0..field_ids.len() {
                    let value = values.pop().unwrap_or(None);
                    if value.is_none() {
                        ok = false;
                    }
                    field_values.push(value);
                }
                field_values.reverse();
                if ok {
                    let fields = field_ids
                        .into_iter()
                        .zip(field_values.into_iter().map(|value| value.unwrap()))
                        .collect();
                    values.push(Some(Expression::new(ExpressionKind::Struct(fields), span)));
                } else {
                    values.push(None);
                }
            }
            Frame::FinishEnumType {
                enum_type,
                first_variant,
                span,
            } => {
                let payload = values.pop().unwrap_or(None);
                if let Some(payload) = payload {
                    values.push(Some(
                        ExpressionKind::EnumValue {
                            enum_type: Box::new(enum_type),
                            variant: first_variant,
                            variant_index: 0,
                            payload: Box::new(payload),
                        }
                        .with_span(span),
                    ));
                } else {
                    values.push(None);
                }
            }
        }
    }

    values.pop().unwrap_or(None)
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
