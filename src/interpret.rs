use std::collections::{HashMap, HashSet};

use crate::{
    diagnostics::{Diagnostic, SourceSpan},
    parsing::{
        BinaryIntrinsicOperator, Binding, BindingAnnotation, BindingPattern, DivergeExpressionType,
        Expression, ExpressionKind, ExpressionLiteral, Identifier, IntrinsicOperation,
        IntrinsicType, LValue, TargetLiteral, UnaryIntrinsicOperator,
    },
    uniquify,
};
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PreserveBehavior {
    PreserveUsage,
    PreserveUsageInLoops,
    PreserveBinding,
    Inline,
}

impl Ord for PreserveBehavior {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use PreserveBehavior::*;
        fn get_rank(behavior: &PreserveBehavior) -> u8 {
            match behavior {
                PreserveUsage => 3,
                PreserveUsageInLoops => 2,
                PreserveBinding => 1,
                Inline => 0,
            }
        }

        get_rank(self).cmp(&get_rank(other))
    }
}

impl PartialOrd for PreserveBehavior {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BindingContext {
    Bound(Expression, PreserveBehavior),
    UnboundWithType(Expression),
    UnboundWithoutType,
}

impl BindingContext {
    fn get_bound_type(&self, context: &Context) -> Result<Option<Expression>, Diagnostic> {
        match self {
            BindingContext::Bound(expression, _) => {
                Ok(Some(get_type_of_expression(expression, context)?))
            }
            BindingContext::UnboundWithType(expression) => Ok(Some(expression.clone())),
            BindingContext::UnboundWithoutType => Ok(None),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    pub bindings: Vec<HashMap<Identifier, (BindingContext, Vec<BindingAnnotation>)>>,
    pub in_loop: bool,
}

#[derive(Clone, Debug)]
pub struct AnnotatedBinding {
    pub identifier: Identifier,
    pub annotations: Vec<BindingAnnotation>,
    pub value: Expression,
}

impl Context {
    pub fn annotated_bindings(&self) -> Vec<AnnotatedBinding> {
        self.bindings
            .iter()
            .last()
            .unwrap()
            .iter()
            .filter(|(_, (_, annotations))| !annotations.is_empty())
            .filter_map(|(name, (binding, annotations))| match binding {
                BindingContext::Bound(value, _) => Some(AnnotatedBinding {
                    identifier: name.clone(),
                    annotations: annotations.clone(),
                    value: value.clone(),
                }),
                _ => None,
            })
            .collect()
    }

    fn empty() -> Context {
        Context {
            bindings: vec![],
            in_loop: false,
        }
    }

    pub fn get_identifier(
        &self,
        identifier: &Identifier,
    ) -> Option<&(BindingContext, Vec<BindingAnnotation>)> {
        self.bindings
            .iter()
            .rev()
            .find_map(|binding_context| binding_context.get(identifier))
    }

    pub fn get_mut_identifier(
        &mut self,
        identifier: &Identifier,
    ) -> Option<&mut (BindingContext, Vec<BindingAnnotation>)> {
        self.bindings
            .iter_mut()
            .rev()
            .find_map(|binding_context| binding_context.get_mut(identifier))
    }

    fn contains_identifier(&self, identifier: &Identifier) -> bool {
        self.bindings
            .iter()
            .rev()
            .any(|binding_context| binding_context.contains_key(identifier))
    }
}

fn diagnostic(message: impl Into<String>, span: SourceSpan) -> Diagnostic {
    Diagnostic::new(message).with_span(span)
}

fn dummy_span() -> SourceSpan {
    SourceSpan::default()
}

fn empty_struct_expr(span: SourceSpan) -> Expression {
    Expression::new(ExpressionKind::Struct(vec![]), span)
}

fn identifier_expr(name: &str) -> Expression {
    ExpressionKind::Identifier(Identifier::new(name.to_string())).with_span(dummy_span())
}

fn intrinsic_type_expr(ty: IntrinsicType) -> Expression {
    Expression::new(ExpressionKind::IntrinsicType(ty), dummy_span())
}

fn ensure_boolean_condition(
    condition: &Expression,
    span: SourceSpan,
    context: &Context,
    construct_name: &str,
) -> Result<(), Diagnostic> {
    let condition_type = get_type_of_expression(condition, &context.clone())?;
    let expected_bool = ExpressionKind::IntrinsicType(IntrinsicType::Boolean);

    if !types_equivalent(&condition_type.kind, &expected_bool) {
        return Err(diagnostic(
            format!(
                "{} condition did not resolve to a boolean value",
                construct_name
            ),
            span,
        ));
    }

    Ok(())
}

fn types_equivalent(left: &ExpressionKind, right: &ExpressionKind) -> bool {
    match (&left, &right) {
        (ExpressionKind::IntrinsicType(a), ExpressionKind::IntrinsicType(b)) => a == b,
        (ExpressionKind::Identifier(a), ExpressionKind::Identifier(b)) => a.name == b.name,
        (ExpressionKind::Struct(a_items), ExpressionKind::Struct(b_items)) => {
            if a_items.len() != b_items.len() {
                return false;
            }
            a_items
                .iter()
                .zip(b_items.iter())
                .all(|((aid, aexpr), (bid, bexpr))| {
                    aid.name == bid.name && types_equivalent(&aexpr.kind, &bexpr.kind)
                })
        }
        (
            ExpressionKind::FunctionType {
                parameter: a_param,
                return_type: a_ret,
            },
            ExpressionKind::FunctionType {
                parameter: b_param,
                return_type: b_ret,
            },
        ) => {
            types_equivalent(&a_param.kind, &b_param.kind)
                && types_equivalent(&a_ret.kind, &b_ret.kind)
        }
        (
            ExpressionKind::AttachImplementation {
                type_expr: a_type, ..
            },
            ExpressionKind::AttachImplementation {
                type_expr: b_type, ..
            },
        ) => types_equivalent(&a_type.kind, &b_type.kind),
        (ExpressionKind::AttachImplementation { type_expr, .. }, other) => {
            types_equivalent(&type_expr.kind, other)
        }
        (other, ExpressionKind::AttachImplementation { type_expr, .. }) => {
            types_equivalent(other, &type_expr.kind)
        }
        (ExpressionKind::EnumType(a_variants), ExpressionKind::EnumType(b_variants)) => {
            if a_variants.len() != b_variants.len() {
                return false;
            }
            a_variants
                .iter()
                .zip(b_variants.iter())
                .all(|((a_id, a_ty), (b_id, b_ty))| {
                    a_id.name == b_id.name && types_equivalent(&a_ty.kind, &b_ty.kind)
                })
        }
        _ => false,
    }
}

#[cfg(test)]
fn literal_number_expr(value: i32) -> Expression {
    ExpressionKind::Literal(ExpressionLiteral::Number(value)).with_span(dummy_span())
}

fn is_type_expression(expr: &ExpressionKind) -> bool {
    match &expr {
        ExpressionKind::IntrinsicType(_) => true,
        ExpressionKind::AttachImplementation { .. } => true,
        ExpressionKind::EnumType(_) => true,
        ExpressionKind::FunctionType { .. } => true,
        ExpressionKind::Struct(items) => items.iter().all(|(_, ty)| is_type_expression(&ty.kind)),
        ExpressionKind::Identifier(_) => true,
        _ => false,
    }
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

pub fn resolve_enum_type_expression(
    enum_expr: &Expression,
    context: &mut Context,
) -> Option<Expression> {
    interpret_expression(enum_expr.clone(), context).ok()
}

fn collect_bindings(expr: &Expression, context: &mut Context) -> Result<(), Diagnostic> {
    match &expr.kind {
        ExpressionKind::Binding(binding) => {
            let mut type_context = context.clone();
            let value_type = get_type_of_expression(&binding.expr, context)
                .or_else(|_| {
                    interpret_expression(binding.expr.clone(), &mut type_context)
                        .and_then(|evaluated| get_type_of_expression(&evaluated, &type_context))
                })
                .ok();

            bind_pattern_blanks(binding.pattern.clone(), context, Vec::new(), value_type)?;
        }
        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(
            left,
            right,
            BinaryIntrinsicOperator::BooleanAnd,
        )) => {
            collect_bindings(left, context)?;
            collect_bindings(right, context)?;
        }
        _ => {}
    }
    Ok(())
}

pub fn interpret_expression(
    expr: Expression,
    context: &mut Context,
) -> Result<Expression, Diagnostic> {
    let span = expr.span;
    let interpreted = match expr.kind {
        ExpressionKind::EnumType(variants) => {
            let mut evaluated_variants = Vec::with_capacity(variants.len());
            for (id, ty_expr) in variants {
                let evaluated_ty = interpret_expression(ty_expr, context)?;
                evaluated_variants.push((id, evaluated_ty));
            }
            Expression::new(ExpressionKind::EnumType(evaluated_variants), span)
        }
        ExpressionKind::Literal(lit) => Expression::new(ExpressionKind::Literal(lit), span),
        ExpressionKind::IntrinsicType(ty) => {
            Expression::new(ExpressionKind::IntrinsicType(ty), span)
        }
        ExpressionKind::Match { value, branches } => {
            let interpreted_value = interpret_expression(*value, context)?;

            if !is_resolved_constant(&interpreted_value) {
                return Ok(ExpressionKind::Match {
                    value: Box::new(interpreted_value),
                    branches,
                }
                .with_span(span));
            }

            for (pattern, branch) in branches {
                let mut branch_context = context.clone();
                let (matched, _preserve_behavior) = bind_pattern_from_value(
                    pattern.clone(),
                    &interpreted_value,
                    &mut branch_context,
                    Vec::new(),
                    PreserveBehavior::Inline,
                )?;

                if matched {
                    let branch_result = interpret_expression(branch, &mut branch_context)?;
                    *context = branch_context;
                    return Ok(branch_result);
                }
            }

            return Err(diagnostic("No match branches matched", span));
        }
        ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition_expr = *condition;
            let condition_for_pattern = condition_expr.clone();
            let interpreted_condition = interpret_expression(condition_expr, context)?;

            let mut then_context = context.clone();
            collect_bindings(&condition_for_pattern, &mut then_context)?;

            let (interpreted_then, then_type, then_diverges) =
                branch_type(&then_branch, &then_context)?;
            let else_context = context.clone();

            let interpreted_else = if let Some(else_branch) = else_branch {
                let (interpreted_else, else_type, else_diverges) =
                    branch_type(&else_branch, &else_context)?;

                if !types_equivalent(&then_type.kind, &else_type.kind)
                    && !then_diverges
                    && !else_diverges
                {
                    return Err(diagnostic("Type mismatch between if branches", span));
                }

                Some(Box::new(interpreted_else))
            } else {
                None
            };

            if let ExpressionKind::Literal(ExpressionLiteral::Boolean(condition_value)) =
                interpreted_condition.kind
            {
                if condition_value {
                    interpret_expression(*then_branch, context)?
                } else if let Some(interpreted_else) = interpreted_else {
                    interpret_expression(*interpreted_else, context)?
                } else {
                    empty_struct_expr(dummy_span())
                }
            } else {
                let interpreted = ExpressionKind::If {
                    condition: Box::new(interpreted_condition),
                    then_branch: Box::new(interpreted_then),
                    else_branch: interpreted_else,
                }
                .with_span(span);
                let possibly_mutated_values = get_possibly_mutated_values(&interpreted);
                for possibly_mutated_value in possibly_mutated_values {
                    let binding = context.get_identifier(&possibly_mutated_value).unwrap();
                    if let Some(binding_ty) = binding.0.get_bound_type(context)? {
                        let binding = context.get_mut_identifier(&possibly_mutated_value).unwrap();
                        binding.0 = BindingContext::UnboundWithType(binding_ty)
                    }
                }
                interpreted
            }
        }
        ExpressionKind::Identifier(identifier) => {
            if let Some((binding, _)) = context.get_identifier(&identifier) {
                match &binding {
                    BindingContext::Bound(value, PreserveBehavior::PreserveUsageInLoops) => {
                        if context.in_loop {
                            Expression::new(ExpressionKind::Identifier(identifier), span)
                        } else {
                            value.clone()
                        }
                    }
                    BindingContext::Bound(
                        expr,
                        PreserveBehavior::Inline | PreserveBehavior::PreserveBinding,
                    ) => expr.clone(),
                    BindingContext::UnboundWithType(_)
                    | BindingContext::UnboundWithoutType
                    | BindingContext::Bound(_, PreserveBehavior::PreserveUsage) => {
                        Expression::new(ExpressionKind::Identifier(identifier), span)
                    }
                }
            } else {
                return Err(diagnostic(
                    format!("Unbound identifier: {}", identifier.name),
                    span,
                ));
            }
        }
        ExpressionKind::Operation {
            operator,
            left,
            right,
        } => interpret_expression(
            ExpressionKind::FunctionCall {
                function: Box::new(
                    ExpressionKind::PropertyAccess {
                        object: left,
                        property: operator.clone(),
                    }
                    .with_span(span),
                ),
                argument: right,
            }
            .with_span(span),
            context,
        )?,
        ExpressionKind::Binding(binding) => {
            interpret_binding(*binding, context).map(|(binding_result, _)| binding_result)?
        }
        ExpressionKind::Diverge {
            value,
            divergance_type,
        } => {
            let evaluated_value = match value {
                Some(expr) => interpret_expression(*expr, context)?,
                None => empty_struct_expr(span),
            };

            ExpressionKind::Diverge {
                value: Some(Box::new(evaluated_value)),
                divergance_type,
            }
            .with_span(span)
        }
        ExpressionKind::Loop { body } => {
            let initial_context = context.clone();
            let mut iteration_count = 0usize;
            loop {
                if iteration_count > 10_000 {
                    return Err(diagnostic("Loop did not produce a return value", span));
                }

                iteration_count += 1;
                let prev_context = context.clone();
                let iteration_value = interpret_expression(*body.clone(), context)?;

                if let ExpressionKind::Diverge {
                    value,
                    divergance_type,
                } = &iteration_value.kind
                {
                    match divergance_type {
                        DivergeExpressionType::Return => break iteration_value,
                        DivergeExpressionType::Break => {
                            break *value.clone().unwrap_or_else(|| {
                                Box::new(empty_struct_expr(iteration_value.span))
                            });
                        }
                    }
                }
                let possible_divergence = expression_does_diverge(&iteration_value, true, false);

                // TODO: There is probably more efficient logic that could be used here instead of comparing large objects
                // Either no mutations occurred; meaning an infinite loop would occur
                // Or a divergence that could've occured did not, meaning the results may be incorrect
                if possible_divergence || prev_context.bindings == context.bindings {
                    *context = initial_context;
                    let was_in_loop_before = context.in_loop;
                    context.in_loop = true;
                    let interpreted_body = interpret_expression(*body, context)?;
                    context.in_loop = was_in_loop_before;
                    let possibly_mutated_values: HashSet<Identifier> =
                        get_possibly_mutated_values(&interpreted_body);
                    for possibly_mutated_value in possibly_mutated_values {
                        let binding = context.get_identifier(&possibly_mutated_value).unwrap();
                        if let Some(binding_ty) = binding.0.get_bound_type(context)? {
                            let binding =
                                context.get_mut_identifier(&possibly_mutated_value).unwrap();
                            binding.0 = BindingContext::UnboundWithType(binding_ty)
                        }
                    }
                    break ExpressionKind::Loop {
                        body: Box::new(interpreted_body),
                    }
                    .with_span(span);
                }
            }
        }
        ExpressionKind::Assignment { target, expr } => {
            let value = interpret_expression(*expr, context)?;
            apply_assignment(target, value, span, context)?
        }
        ExpressionKind::Block(expressions) => interpret_block(expressions, span, context)?.0,
        ExpressionKind::FunctionCall { function, argument } => {
            let function_value = interpret_expression(*function, context)?;
            let argument_value = interpret_expression(*argument, context)?;

            let effective_function = if let ExpressionKind::Identifier(ident) = &function_value.kind
            {
                context.get_identifier(ident).and_then(|b| match &b.0 {
                    BindingContext::Bound(v, _) => Some(v.clone()),
                    _ => None,
                })
            } else {
                Some(function_value.clone())
            };

            if let Some(Expression {
                kind:
                    ExpressionKind::Function {
                        parameter,
                        return_type,
                        body,
                    },
                ..
            }) = effective_function
            {
                let is_direct = matches!(function_value.kind, ExpressionKind::Function { .. });
                let returns_compile_time_type =
                    type_expression_contains_compile_time_data(&return_type.unwrap());
                let pattern_is_compile_time = pattern_contains_compile_time_data(&parameter);
                let argument_is_const = is_resolved_constant(&argument_value);

                if is_direct
                    || returns_compile_time_type
                    || pattern_is_compile_time
                    || argument_is_const
                {
                    let mut call_context = context.clone();
                    bind_pattern_from_value(
                        parameter,
                        &argument_value,
                        &mut call_context,
                        Vec::new(),
                        PreserveBehavior::Inline,
                    )?;
                    let body_value = interpret_expression(*body, &mut call_context)?;
                    if let ExpressionKind::Diverge {
                        value,
                        divergance_type: DivergeExpressionType::Return,
                    } = body_value.kind
                    {
                        return Ok(*value.expect("Return value should be populated"));
                    }
                    return Ok(body_value);
                }
            }

            if let ExpressionKind::EnumConstructor {
                enum_type,
                variant,
                variant_index,
                payload_type,
            } = function_value.kind.clone()
            {
                let argument_type = get_type_of_expression(&argument_value, context)?;
                if !types_equivalent(&payload_type.kind, &argument_type.kind) {
                    return Err(diagnostic("Enum variant payload type mismatch", span));
                }
                ExpressionKind::EnumValue {
                    enum_type,
                    variant,
                    variant_index,
                    payload: Some(Box::new(argument_value)),
                }
                .with_span(span.merge(&function_value.span))
            } else if is_resolved_constant(&function_value) {
                return Err(diagnostic("Attempted to call a non-function value", span));
            } else {
                ExpressionKind::FunctionCall {
                    function: Box::new(function_value),
                    argument: Box::new(argument_value),
                }
                .with_span(span)
            }
        }
        ExpressionKind::AttachImplementation {
            type_expr,
            implementation,
        } => ExpressionKind::AttachImplementation {
            type_expr: Box::new(interpret_expression(*type_expr, context)?),
            implementation: Box::new(interpret_expression(*implementation, context)?),
        }
        .with_span(span),
        ExpressionKind::Function {
            parameter,
            return_type: _,
            body,
        } => {
            let parameter = interpret_binding_pattern(parameter, context)?;

            let mut type_context = context.clone();

            bind_pattern_blanks(parameter.clone(), &mut type_context, Vec::new(), None)?;

            let interpreted_body = interpret_expression(*body, &mut type_context)?;

            let return_type = get_type_of_expression(&interpreted_body, &type_context)?;

            ExpressionKind::Function {
                parameter,
                return_type: Some(Box::new(return_type)),
                body: Box::new(interpreted_body),
            }
            .with_span(span)
        }
        ExpressionKind::Struct(items) => {
            let mut evaluated_items = Vec::with_capacity(items.len());
            for (identifier, value_expr) in items {
                let evaluated_value = interpret_expression(value_expr, context)?;
                evaluated_items.push((identifier, evaluated_value));
            }
            Expression::new(ExpressionKind::Struct(evaluated_items), span)
        }
        ExpressionKind::FunctionType {
            parameter,
            return_type,
        } => ExpressionKind::FunctionType {
            parameter: interpret_expression(*parameter, context)?.into(),
            return_type: interpret_expression(*return_type, context)?.into(),
        }
        .with_span(span),
        ExpressionKind::EnumAccess { enum_expr, variant } => {
            let interpreted_enum = interpret_expression(*enum_expr, context)?;
            if let Some((variant_index, payload_type)) =
                enum_variant_info(&interpreted_enum, &variant)
            {
                if let ExpressionKind::Struct(fields) = &payload_type.kind
                    && fields.is_empty()
                {
                    ExpressionKind::EnumValue {
                        enum_type: Box::new(interpreted_enum),
                        variant,
                        variant_index,
                        payload: None,
                    }
                } else {
                    ExpressionKind::EnumConstructor {
                        enum_type: Box::new(interpreted_enum),
                        variant,
                        variant_index,
                        payload_type: Box::new(payload_type),
                    }
                }
            } else {
                ExpressionKind::EnumAccess {
                    enum_expr: Box::new(interpreted_enum),
                    variant,
                }
            }
            .with_span(span)
        }
        ExpressionKind::EnumValue {
            enum_type,
            variant,
            variant_index,
            payload,
        } => ExpressionKind::EnumValue {
            enum_type: Box::new(interpret_expression(*enum_type, context)?),
            variant,
            variant_index,
            payload: match payload {
                Some(value) => Some(Box::new(interpret_expression(*value, context)?)),
                None => None,
            },
        }
        .with_span(span),
        ExpressionKind::EnumConstructor {
            enum_type,
            variant,
            variant_index,
            payload_type,
        } => ExpressionKind::EnumConstructor {
            enum_type: Box::new(interpret_expression(*enum_type, context)?),
            variant,
            variant_index,
            payload_type: Box::new(interpret_expression(*payload_type, context)?),
        }
        .with_span(span),
        ExpressionKind::IntrinsicOperation(intrinsic_operation) => match intrinsic_operation {
            IntrinsicOperation::Binary(left, right, operator) => {
                let evaluated_left = interpret_expression(*left, context)?;
                let evaluated_right = interpret_expression(*right, context)?;
                if !is_resolved_constant(&evaluated_left) || !is_resolved_constant(&evaluated_right)
                {
                    return Ok(Expression::new(
                        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(
                            Box::new(evaluated_left),
                            Box::new(evaluated_right),
                            operator,
                        )),
                        span,
                    ));
                }
                match operator {
                    BinaryIntrinsicOperator::I32Add
                    | BinaryIntrinsicOperator::I32Subtract
                    | BinaryIntrinsicOperator::I32Multiply => interpret_numeric_intrinsic(
                        evaluated_left,
                        evaluated_right,
                        span,
                        match operator {
                            BinaryIntrinsicOperator::I32Add => |l, r| l + r,
                            BinaryIntrinsicOperator::I32Subtract => |l, r| l - r,
                            BinaryIntrinsicOperator::I32Multiply => |l, r| l * r,
                            _ => unreachable!(),
                        },
                    )?,
                    BinaryIntrinsicOperator::I32Divide => {
                        interpret_divide_intrinsic(evaluated_left, evaluated_right, span)?
                    }
                    BinaryIntrinsicOperator::I32Equal
                    | BinaryIntrinsicOperator::I32NotEqual
                    | BinaryIntrinsicOperator::I32LessThan
                    | BinaryIntrinsicOperator::I32GreaterThan
                    | BinaryIntrinsicOperator::I32LessThanOrEqual
                    | BinaryIntrinsicOperator::I32GreaterThanOrEqual => {
                        interpret_comparison_intrinsic(
                            evaluated_left,
                            evaluated_right,
                            span,
                            match operator {
                                BinaryIntrinsicOperator::I32Equal => |l, r| l == r,
                                BinaryIntrinsicOperator::I32NotEqual => |l, r| l != r,
                                BinaryIntrinsicOperator::I32LessThan => |l, r| l < r,
                                BinaryIntrinsicOperator::I32GreaterThan => |l, r| l > r,
                                BinaryIntrinsicOperator::I32LessThanOrEqual => |l, r| l <= r,
                                BinaryIntrinsicOperator::I32GreaterThanOrEqual => |l, r| l >= r,
                                _ => unreachable!(),
                            },
                        )?
                    }
                    BinaryIntrinsicOperator::BooleanAnd
                    | BinaryIntrinsicOperator::BooleanOr
                    | BinaryIntrinsicOperator::BooleanXor => interpret_boolean_intrinsic(
                        evaluated_left,
                        evaluated_right,
                        span,
                        match operator {
                            BinaryIntrinsicOperator::BooleanAnd => |l, r| l && r,
                            BinaryIntrinsicOperator::BooleanOr => |l, r| l || r,
                            BinaryIntrinsicOperator::BooleanXor => |l, r| l ^ r,
                            _ => unreachable!(),
                        },
                    )?,
                }
            }
            IntrinsicOperation::Unary(operand, op) => {
                let evaluated_operand = interpret_expression(*operand, context)?;
                if !is_resolved_constant(&evaluated_operand) {
                    return Ok(Expression::new(
                        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(
                            Box::new(evaluated_operand),
                            op,
                        )),
                        span,
                    ));
                }
                match op {
                    UnaryIntrinsicOperator::BooleanNot => match evaluated_operand.kind {
                        ExpressionKind::Literal(ExpressionLiteral::Boolean(b)) => {
                            ExpressionKind::Literal(ExpressionLiteral::Boolean(!b))
                                .with_span(evaluated_operand.span)
                        }
                        _ => {
                            return Err(Diagnostic::new(
                                "Cannot perform boolean not on non boolean type",
                            )
                            .with_span(span));
                        }
                    },
                    UnaryIntrinsicOperator::EnumFromStruct => {
                        interpret_enum_from_struct(evaluated_operand, span, context)?
                    }
                }
            }
        },
        ExpressionKind::PropertyAccess { object, property } => {
            let evaluated_object = interpret_expression(*object, context)?;
            if let ExpressionKind::Struct(items) = &evaluated_object.kind {
                for (item_id, item_expr) in items {
                    if item_id.name == property {
                        return Ok(item_expr.clone());
                    }
                }
            }

            let object_type = get_type_of_expression(&evaluated_object, context)?;
            let trait_prop = get_trait_prop_of_type(&object_type, &property, span)?;
            match trait_prop.kind {
                ExpressionKind::Function { .. } => interpret_expression(
                    ExpressionKind::FunctionCall {
                        function: Box::new(trait_prop),
                        argument: Box::new(evaluated_object),
                    }
                    .with_span(span),
                    context,
                )?,
                _other => ExpressionKind::PropertyAccess {
                    object: Box::new(evaluated_object),
                    property,
                }
                .with_span(span),
            }
        }
    };
    Ok(interpreted)
}

fn get_possibly_mutated_values(body: &Expression) -> HashSet<Identifier> {
    fold_expression(body, HashSet::new(), &|expr, mut mutated| {
        if let ExpressionKind::Assignment { ref target, .. } = expr.kind {
            mutated.extend(target.get_used_identifiers());
        }
        mutated
    })
}

fn interpret_binding_pattern(
    parameter: BindingPattern,
    context: &mut Context,
) -> Result<BindingPattern, Diagnostic> {
    match parameter {
        pat @ BindingPattern::Identifier(..) => Ok(pat),
        pat @ BindingPattern::Literal(..) => Ok(pat),
        BindingPattern::Struct(items, source_span) => {
            let mut interpreted_items = Vec::with_capacity(items.len());
            for (field_id, field_pattern) in items {
                let interpreted_field_pattern = interpret_binding_pattern(field_pattern, context)?;
                interpreted_items.push((field_id, interpreted_field_pattern));
            }
            Ok(BindingPattern::Struct(interpreted_items, source_span))
        }
        BindingPattern::EnumVariant {
            enum_type,
            variant,
            payload,
            span,
        } => Ok(BindingPattern::EnumVariant {
            enum_type: Box::new(interpret_expression(*enum_type, context)?),
            variant,
            payload: match payload {
                Some(payload) => Some(Box::new(interpret_binding_pattern(*payload, context)?)),
                None => None,
            },
            span,
        }),
        BindingPattern::TypeHint(binding_pattern, expression, source_span) => {
            let interpreted_pattern = interpret_binding_pattern(*binding_pattern, context)?;
            let interpreted_type = interpret_expression(*expression, context)?;
            Ok(BindingPattern::TypeHint(
                Box::new(interpreted_pattern),
                Box::new(interpreted_type),
                source_span,
            ))
        }
        BindingPattern::Annotated {
            annotations,
            pattern,
            span,
        } => {
            let interpreted_pattern = interpret_binding_pattern(*pattern, context)?;
            let interpreted_annotations = annotations
                .into_iter()
                .map(|ann| parse_binding_annotation(ann, context))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(BindingPattern::Annotated {
                pattern: Box::new(interpreted_pattern),
                annotations: interpreted_annotations,
                span,
            })
        }
    }
}

fn get_type_of_expression(expr: &Expression, context: &Context) -> Result<Expression, Diagnostic> {
    let span = expr.span();
    let result = match &expr.kind {
        ExpressionKind::Literal(lit) => match lit {
            ExpressionLiteral::Number(_) => {
                interpret_expression(identifier_expr("i32"), &mut context.clone())?
            } // TODO: create variant of interpret that panics when it would need mutable context, and does not require it
            ExpressionLiteral::Boolean(_) => {
                interpret_expression(identifier_expr("bool"), &mut context.clone())?
            }
            ExpressionLiteral::Target(_) => {
                interpret_expression(identifier_expr("target"), &mut context.clone())?
            }
        },
        ExpressionKind::Identifier(identifier) => {
            let bound_value = context
                .get_identifier(identifier)
                .ok_or_else(|| {
                    diagnostic(format!("Unbound identifier: {}", identifier.name), span)
                })?
                .clone();

            match bound_value.0 {
                BindingContext::Bound(value, _) => get_type_of_expression(&value, context)?,
                BindingContext::UnboundWithType(type_expr) => {
                    interpret_expression(type_expr.clone(), &mut context.clone())?
                }
                BindingContext::UnboundWithoutType => {
                    return Err(diagnostic(
                        format!(
                            "Cannot determine type of unbound identifier: {}",
                            identifier.name
                        ),
                        span,
                    ));
                }
            }
        }
        ExpressionKind::Assignment {
            target,
            expr: value_expr,
        } => {
            let value_type = get_type_of_expression(value_expr, context)?;
            let target_type = get_lvalue_type(target, context, span)?;

            if !types_equivalent(&target_type.kind, &value_type.kind) {
                return Err(diagnostic(
                    format!(
                        "Cannot assign value of mismatched type to {}",
                        lvalue_display_name(target)
                    ),
                    span,
                ));
            }

            target_type
        }
        ExpressionKind::Diverge { value, .. } => {
            let inner_value = value
                .as_ref()
                .map(|expr| expr.as_ref().clone())
                .unwrap_or_else(|| empty_struct_expr(span));
            get_type_of_expression(&inner_value, context)?
        }
        ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            ensure_boolean_condition(condition, span, context, "If")?;

            let inferred_else = else_branch
                .as_ref()
                .map(|expr| expr.as_ref().clone())
                .unwrap_or_else(|| empty_struct_expr(SourceSpan::new(span.end(), 0)));
            let mut then_context = context.clone();
            collect_bindings(condition, &mut then_context)?;
            let then_type = get_type_of_expression(then_branch, &then_context)?;
            let else_type = get_type_of_expression(&inferred_else, context)?;
            if !types_equivalent(&then_type.kind, &else_type.kind) {
                let then_returns = expression_does_diverge(then_branch, false, false);
                let else_returns = expression_does_diverge(&inferred_else, false, false);

                if then_returns && !else_returns {
                    return Ok(else_type);
                } else if else_returns && !then_returns {
                    return Ok(then_type);
                } else {
                    return Err(diagnostic("Type mismatch between if branches", span));
                }
            }
            then_type
        }
        ExpressionKind::Match { value, branches } => {
            let value_type = get_type_of_expression(value, context)?;
            let mut branch_type: Option<Expression> = None;

            for (pattern, branch) in branches {
                let mut branch_context = context.clone();
                bind_pattern_blanks(
                    pattern.clone(),
                    &mut branch_context,
                    Vec::new(),
                    Some(value_type.clone()),
                )?;
                let branch_ty = get_type_of_expression(branch, &branch_context)?;
                if let Some(existing) = &branch_type {
                    if !types_equivalent(&existing.kind, &branch_ty.kind)
                        && !expression_does_diverge(branch, false, false)
                    {
                        return Err(diagnostic("Type mismatch between match branches", span));
                    }
                } else {
                    branch_type = Some(branch_ty.clone());
                }
            }

            branch_type.ok_or(diagnostic("Match has no branches", span))?
        }
        ExpressionKind::Binding(..) => {
            interpret_expression(identifier_expr("bool"), &mut context.clone())?
        }
        ExpressionKind::EnumAccess { enum_expr, variant } => {
            let enum_type = interpret_expression(enum_expr.as_ref().clone(), &mut context.clone())?;
            if let Some((_, payload_type)) = enum_variant_info(&enum_type, variant) {
                if let ExpressionKind::Struct(fields) = &payload_type.kind
                    && fields.is_empty()
                {
                    return Ok(enum_type);
                }

                ExpressionKind::FunctionType {
                    parameter: Box::new(payload_type),
                    return_type: Box::new(enum_type),
                }
                .with_span(span)
            } else {
                return Err(diagnostic("Unknown enum variant", span));
            }
        }
        ExpressionKind::EnumConstructor {
            enum_type,
            payload_type,
            ..
        } => ExpressionKind::FunctionType {
            parameter: payload_type.clone(),
            return_type: enum_type.clone(),
        }
        .with_span(span),
        ExpressionKind::EnumValue { enum_type, .. } => *enum_type.clone(),
        ExpressionKind::EnumType(_) => {
            ExpressionKind::IntrinsicType(IntrinsicType::Type).with_span(span)
        }
        ExpressionKind::Operation {
            operator,
            left,
            right,
        } => get_type_of_expression(
            &ExpressionKind::FunctionCall {
                function: Box::new(
                    ExpressionKind::PropertyAccess {
                        object: left.clone(),
                        property: operator.clone(),
                    }
                    .with_span(span),
                ),
                argument: right.clone(),
            }
            .with_span(span),
            context,
        )?,
        ExpressionKind::Block(exprs) => {
            let (value, block_context) =
                interpret_block(exprs.clone(), span, &mut context.clone())?;
            if let ExpressionKind::Block(expressions) = &value.kind {
                let Some(last_expr) = expressions.last() else {
                    return Err(Diagnostic::new(
                        "Cannot determine type of empty block".to_string(),
                    )
                    .with_span(value.span));
                };
                return get_type_of_expression(last_expr, &block_context);
            }
            get_type_of_expression(&value, &block_context)?
        }
        ExpressionKind::Loop { body } => {
            let break_values = collect_break_values(body);
            if break_values.is_empty() {
                get_type_of_expression(body, context)?
            } else {
                let mut first_type: Option<Expression> = None;
                for val in break_values {
                    let ty = get_type_of_expression(&val, context)?;
                    if let Some(ref first) = first_type {
                        if !types_equivalent(&first.kind, &ty.kind) {
                            return Err(diagnostic("Inconsistent break types in loop", val.span()));
                        }
                    } else {
                        first_type = Some(ty);
                    }
                }
                first_type.unwrap()
            }
        }
        ExpressionKind::FunctionCall { function, argument } => {
            if let ExpressionKind::EnumAccess { enum_expr, variant } = &function.kind {
                let enum_type =
                    interpret_expression(enum_expr.as_ref().clone(), &mut context.clone())?;
                if let Some((_, payload_type)) = enum_variant_info(&enum_type, variant) {
                    let argument_type = get_type_of_expression(argument, context)?;
                    if !types_equivalent(&payload_type.kind, &argument_type.kind) {
                        return Err(diagnostic("Enum variant payload type mismatch", span));
                    }
                    return Ok(enum_type);
                } else {
                    return Err(diagnostic("Unknown enum variant", span));
                }
            }
            let mut call_context = context.clone();
            let evaluated_function = interpret_expression(*function.clone(), &mut call_context)?;
            let evaluated_function_type =
                get_type_of_expression(&evaluated_function, &call_context)?;
            let ExpressionKind::FunctionType {
                parameter,
                return_type,
            } = &evaluated_function_type.kind
            else {
                return Err(diagnostic("Attempted to call a non-function value", span));
            };
            let argument_type = get_type_of_expression(argument, &call_context)?;
            if !types_equivalent(&parameter.kind, &argument_type.kind) {
                return Err(diagnostic(
                    format!(
                        "Function argument type mismatch type {:?} vs {:?}",
                        parameter, argument_type
                    ),
                    span,
                ));
            }
            interpret_expression(*return_type.clone(), &mut call_context)?
        }
        ExpressionKind::PropertyAccess { object, property } => {
            let object_type = get_type_of_expression(object, context)?;
            get_trait_prop_of_type(&object_type, property, span)?
        }
        ExpressionKind::IntrinsicType(intrinsic_type) => match intrinsic_type {
            IntrinsicType::I32
            | IntrinsicType::Boolean
            | IntrinsicType::Target
            | IntrinsicType::Type => {
                ExpressionKind::IntrinsicType(IntrinsicType::Type).with_span(span)
            }
        },
        ExpressionKind::AttachImplementation { type_expr, .. } => {
            get_type_of_expression(type_expr, context)?
        }
        ExpressionKind::Function {
            parameter,
            return_type,
            ..
        } => ExpressionKind::FunctionType {
            parameter: Box::new(get_type_of_binding_pattern(
                parameter,
                &mut context.clone(),
            )?),
            return_type: return_type.as_ref().unwrap().clone(),
        }
        .with_span(span),
        ExpressionKind::Struct(items) => {
            let mut struct_items = Vec::with_capacity(items.len());
            for (field_id, field_expr) in items {
                let field_type = get_type_of_expression(field_expr, context)?;
                struct_items.push((field_id.clone(), field_type));
            }
            Expression::new(ExpressionKind::Struct(struct_items), span)
        }
        ExpressionKind::FunctionType { .. } => {
            ExpressionKind::IntrinsicType(IntrinsicType::Type).with_span(span)
        }
        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(_, _, operator)) => {
            match operator {
                BinaryIntrinsicOperator::I32Add
                | BinaryIntrinsicOperator::I32Subtract
                | BinaryIntrinsicOperator::I32Multiply
                | BinaryIntrinsicOperator::I32Divide => {
                    interpret_expression(identifier_expr("i32"), &mut context.clone())?
                }
                BinaryIntrinsicOperator::I32Equal
                | BinaryIntrinsicOperator::I32NotEqual
                | BinaryIntrinsicOperator::I32LessThan
                | BinaryIntrinsicOperator::I32GreaterThan
                | BinaryIntrinsicOperator::I32LessThanOrEqual
                | BinaryIntrinsicOperator::I32GreaterThanOrEqual
                | BinaryIntrinsicOperator::BooleanAnd
                | BinaryIntrinsicOperator::BooleanOr
                | BinaryIntrinsicOperator::BooleanXor => {
                    interpret_expression(identifier_expr("bool"), &mut context.clone())?
                }
            }
        }
        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(
            _,
            UnaryIntrinsicOperator::BooleanNot,
        )) => interpret_expression(identifier_expr("bool"), &mut context.clone())?,
        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(
            _,
            UnaryIntrinsicOperator::EnumFromStruct,
        )) => interpret_expression(identifier_expr("type"), &mut context.clone())?,
    };
    Ok(result)
}

pub fn collect_break_values(expr: &Expression) -> Vec<Expression> {
    fold_expression(expr, Vec::new(), &|current_expr, mut values| {
        if let ExpressionKind::Diverge {
            value,
            divergance_type: DivergeExpressionType::Break,
        } = &current_expr.kind
        {
            let val = value
                .as_ref()
                .map(|v| *v.clone())
                .unwrap_or_else(|| empty_struct_expr(current_expr.span));
            values.push(val);
        }
        values
    })
}

fn get_type_of_binding_pattern(
    pattern: &BindingPattern,
    context: &mut Context,
) -> Result<Expression, Diagnostic> {
    match pattern {
        BindingPattern::Identifier(_, span) => Err(diagnostic(
            "Cannot determine type of untyped identifier",
            *span,
        )),
        BindingPattern::Literal(lit, span) => get_type_of_expression(
            &Expression::new(ExpressionKind::Literal(lit.clone()), *span),
            context,
        ),
        BindingPattern::Struct(pattern_items, span) => {
            let mut struct_items = Vec::with_capacity(pattern_items.len());
            for (field_identifier, field_pattern) in pattern_items {
                let field_type = get_type_of_binding_pattern(field_pattern, context)?;
                struct_items.push((field_identifier.clone(), field_type));
            }
            Ok(Expression::new(ExpressionKind::Struct(struct_items), *span))
        }
        BindingPattern::EnumVariant { enum_type, .. } => {
            interpret_expression(*enum_type.clone(), context)
        }
        BindingPattern::TypeHint(_, type_expr, _) => Ok(*type_expr.clone()),
        BindingPattern::Annotated { pattern, .. } => get_type_of_binding_pattern(pattern, context),
    }
}

pub fn expression_does_diverge(expr: &Expression, possibility: bool, in_inner_loop: bool) -> bool {
    match &expr.kind {
        ExpressionKind::Diverge {
            divergance_type: DivergeExpressionType::Break,
            ..
        } => !in_inner_loop,
        ExpressionKind::Diverge {
            divergance_type: DivergeExpressionType::Return,
            ..
        } => true,
        ExpressionKind::Block(exprs) => exprs
            .iter()
            .any(|expr| expression_does_diverge(expr, possibility, in_inner_loop)),
        ExpressionKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            let then_diverges = expression_does_diverge(then_branch, possibility, in_inner_loop);
            let else_diverges = else_branch
                .as_ref()
                .map(|branch| expression_does_diverge(branch, possibility, in_inner_loop))
                .unwrap_or(false);
            if possibility {
                then_diverges || else_diverges
            } else {
                then_diverges && else_diverges
            }
        }
        ExpressionKind::Binding(binding) => {
            expression_does_diverge(&binding.expr, possibility, in_inner_loop)
        }
        ExpressionKind::Assignment { expr, .. } => {
            expression_does_diverge(expr, possibility, in_inner_loop)
        }
        ExpressionKind::FunctionCall {
            function, argument, ..
        } => {
            expression_does_diverge(function, possibility, in_inner_loop)
                || expression_does_diverge(argument, possibility, in_inner_loop)
        }
        ExpressionKind::Loop { body } => expression_does_diverge(body, possibility, true),
        ExpressionKind::PropertyAccess { object, .. } => {
            expression_does_diverge(object, possibility, in_inner_loop)
        }
        ExpressionKind::Operation { left, right, .. } => {
            expression_does_diverge(left, possibility, in_inner_loop)
                || expression_does_diverge(right, possibility, in_inner_loop)
        }
        ExpressionKind::AttachImplementation {
            type_expr,
            implementation,
        } => {
            expression_does_diverge(type_expr, possibility, in_inner_loop)
                || expression_does_diverge(implementation, possibility, in_inner_loop)
        }
        ExpressionKind::EnumAccess { enum_expr, .. } => {
            expression_does_diverge(enum_expr, possibility, in_inner_loop)
        }
        ExpressionKind::EnumValue {
            enum_type, payload, ..
        } => {
            expression_does_diverge(enum_type, possibility, in_inner_loop)
                || payload
                    .as_ref()
                    .map(|payload| expression_does_diverge(payload, possibility, in_inner_loop))
                    .unwrap_or(false)
        }
        ExpressionKind::EnumConstructor {
            enum_type,
            payload_type,
            ..
        } => {
            expression_does_diverge(enum_type, possibility, in_inner_loop)
                || expression_does_diverge(payload_type, possibility, in_inner_loop)
        }
        ExpressionKind::Match {
            value, branches, ..
        } => {
            expression_does_diverge(value, possibility, in_inner_loop)
                || branches
                    .iter()
                    .all(|(_, branch)| expression_does_diverge(branch, possibility, in_inner_loop))
        }
        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(left, right, _)) => {
            expression_does_diverge(left, possibility, in_inner_loop)
                || expression_does_diverge(right, possibility, in_inner_loop)
        }
        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(operand, _)) => {
            expression_does_diverge(operand, possibility, in_inner_loop)
        }
        ExpressionKind::Literal(_)
        | ExpressionKind::Identifier(_)
        | ExpressionKind::IntrinsicType(_)
        | ExpressionKind::EnumType(_)
        | ExpressionKind::Function { .. }
        | ExpressionKind::FunctionType { .. }
        | ExpressionKind::Struct(_) => false,
    }
}

pub fn expression_exports(expr: &Expression) -> bool {
    match &expr.kind {
        ExpressionKind::Diverge {
            value: Some(value), ..
        } => expression_exports(value),
        ExpressionKind::Block(exprs) => exprs.iter().any(expression_exports),
        ExpressionKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            let then_diverges = expression_exports(then_branch);
            let else_diverges = else_branch
                .as_ref()
                .map(|else_branch| expression_exports(else_branch))
                .unwrap_or(false);
            then_diverges || else_diverges
        }
        ExpressionKind::Binding(binding) => {
            pattern_exports(&binding.pattern) || expression_exports(&binding.expr)
        }
        ExpressionKind::Assignment { expr, .. } => expression_exports(expr),
        ExpressionKind::FunctionCall {
            function, argument, ..
        } => expression_exports(function) || expression_exports(argument),
        ExpressionKind::Loop { body, .. } => expression_exports(body),
        ExpressionKind::PropertyAccess { object, .. } => expression_exports(object),
        ExpressionKind::Operation { left, right, .. } => {
            expression_exports(left) || expression_exports(right)
        }
        ExpressionKind::AttachImplementation {
            type_expr,
            implementation,
            ..
        } => expression_exports(type_expr) || expression_exports(implementation),
        ExpressionKind::EnumAccess { enum_expr, .. } => expression_exports(enum_expr),
        ExpressionKind::EnumValue {
            enum_type, payload, ..
        } => {
            expression_exports(enum_type)
                || payload
                    .as_ref()
                    .map(|else_branch| expression_exports(else_branch))
                    .unwrap_or(false)
        }
        ExpressionKind::EnumConstructor {
            enum_type,
            payload_type,
            ..
        } => expression_exports(enum_type) || expression_exports(payload_type),
        ExpressionKind::Match {
            value, branches, ..
        } => {
            expression_exports(value)
                || branches
                    .iter()
                    .all(|(_, branch)| expression_exports(branch))
        }
        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(left, right, _)) => {
            expression_exports(left) || expression_exports(right)
        }
        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(operand, _)) => {
            expression_exports(operand)
        }
        ExpressionKind::Diverge { value: None, .. }
        | ExpressionKind::Literal(_)
        | ExpressionKind::Identifier(_)
        | ExpressionKind::IntrinsicType(_)
        | ExpressionKind::EnumType(_)
        | ExpressionKind::Function { .. }
        | ExpressionKind::FunctionType { .. }
        | ExpressionKind::Struct(_) => false,
    }
}

fn pattern_exports(pattern: &BindingPattern) -> bool {
    match pattern {
        BindingPattern::Identifier(..) => false,
        BindingPattern::Literal(..) => false,
        BindingPattern::Struct(items, ..) => items.iter().any(|(_, item)| pattern_exports(item)),
        BindingPattern::EnumVariant {
            enum_type,
            payload: Some(payload),
            ..
        } => expression_exports(enum_type) || pattern_exports(payload),
        BindingPattern::EnumVariant {
            enum_type,
            payload: None,
            ..
        } => expression_exports(enum_type),
        BindingPattern::TypeHint(binding_pattern, expression, ..) => {
            pattern_exports(binding_pattern) || expression_exports(expression)
        }
        BindingPattern::Annotated {
            annotations,
            pattern,
            ..
        } => {
            annotations
                .iter()
                .any(|ann| matches!(ann, BindingAnnotation::Export(..)))
                || pattern_exports(pattern)
        }
    }
}

fn get_assigned_identifiers(expr: &Expression) -> HashSet<Identifier> {
    fold_expression(expr, HashSet::new(), &|current_expr, mut identifiers| {
        if let ExpressionKind::Assignment { target, .. } = &current_expr.kind {
            identifiers.extend(target.get_used_identifiers());
        }
        identifiers
    })
}

fn expression_contains_external_mutation(expr: &Expression, context: &Context) -> bool {
    let assigned = get_assigned_identifiers(expr);

    for identifier in assigned {
        if context.get_identifier(&identifier).is_some() {
            return true;
        }
    }
    false
}

fn pattern_contains_compile_time_data(pattern: &BindingPattern) -> bool {
    match pattern {
        BindingPattern::Identifier(_, _) => false,
        BindingPattern::Literal(_, _) => true,
        BindingPattern::Struct(items, _) => items
            .iter()
            .any(|(_, field_pattern)| pattern_contains_compile_time_data(field_pattern)),
        BindingPattern::EnumVariant { payload, .. } => {
            if let Some(payload) = payload {
                pattern_contains_compile_time_data(payload)
            } else {
                false
            }
        }
        BindingPattern::TypeHint(pattern, ty, _) => {
            pattern_contains_compile_time_data(pattern)
                || type_expression_contains_compile_time_data(ty)
        }
        BindingPattern::Annotated { pattern, .. } => pattern_contains_compile_time_data(pattern),
    }
}

fn type_expression_contains_compile_time_data(expr: &Expression) -> bool {
    match &expr.kind {
        ExpressionKind::Struct(items) => items
            .iter()
            .any(|(_, field_expr)| type_expression_contains_compile_time_data(field_expr)),
        ExpressionKind::FunctionType { .. } => true,
        ExpressionKind::AttachImplementation { type_expr, .. } => {
            type_expression_contains_compile_time_data(type_expr)
        }
        ExpressionKind::EnumType(cases) => cases
            .iter()
            .any(|(_, field_expr)| type_expression_contains_compile_time_data(field_expr)),
        ExpressionKind::IntrinsicType(IntrinsicType::Target | IntrinsicType::Type) => true,
        ExpressionKind::IntrinsicType(IntrinsicType::Boolean | IntrinsicType::I32) => false,
        other => panic!("Unsupported expression {:?} for resolved type", other),
    }
}

fn branch_type(
    branch: &Expression,
    context: &Context,
) -> Result<(Expression, Expression, bool), Diagnostic> {
    let mut branch_context = context.clone();
    let evaluated_branch = interpret_expression(branch.clone(), &mut branch_context)?;
    let branch_type = get_type_of_expression(&evaluated_branch, &branch_context)?;
    Ok((
        evaluated_branch.clone(),
        branch_type,
        matches!(evaluated_branch.kind, ExpressionKind::Diverge { .. }),
    ))
}

fn get_trait_prop_of_type(
    value_type: &Expression,
    trait_prop: &str,
    span: SourceSpan,
) -> Result<Expression, Diagnostic> {
    fn get_struct_field(
        items: &[(Identifier, Expression)],
        trait_prop: &str,
    ) -> Option<Expression> {
        items
            .iter()
            .find(|(field_id, _)| field_id.name == trait_prop)
            .map(|(_, expr)| expr.clone())
    }

    match &value_type.kind {
        ExpressionKind::Struct(items) => get_struct_field(items, trait_prop)
            .ok_or_else(|| diagnostic(format!("Missing field {} on type", trait_prop), span)),
        ExpressionKind::AttachImplementation {
            type_expr,
            implementation,
            ..
        } => {
            if let ExpressionKind::Struct(ref items) = implementation.kind
                && let Some(field) = get_struct_field(items, trait_prop)
            {
                return Ok(field);
            }

            get_trait_prop_of_type(type_expr, trait_prop, span)
        }
        ty => Err(diagnostic(
            format!(
                "Unsupported value type {:?} for `{}` operator lookup",
                ty, trait_prop
            ),
            span,
        )),
    }
}

fn collect_bound_identifiers_from_pattern(
    pattern: &BindingPattern,
    bound: &mut HashSet<Identifier>,
) {
    match pattern {
        BindingPattern::Identifier(identifier, _) => {
            bound.insert(identifier.clone());
        }
        BindingPattern::Struct(items, _) => {
            for (_, sub_pattern) in items {
                collect_bound_identifiers_from_pattern(sub_pattern, bound);
            }
        }
        BindingPattern::EnumVariant { payload, .. } => {
            if let Some(payload) = payload {
                collect_bound_identifiers_from_pattern(payload, bound);
            }
        }
        BindingPattern::TypeHint(inner, _, _) => {
            collect_bound_identifiers_from_pattern(inner, bound);
        }
        BindingPattern::Annotated { pattern, .. } => {
            collect_bound_identifiers_from_pattern(pattern, bound);
        }
        BindingPattern::Literal(_, _) => {}
    }
}

fn fold_expression<T, U: Fn(&Expression, T) -> T>(
    expr: &Expression,
    init: T,
    item_processor: &U,
) -> T {
    let new_state = item_processor(expr, init);
    match &expr.kind {
        ExpressionKind::IntrinsicType(..) => new_state,
        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(left, right, ..)) => {
            fold_expression(
                right,
                fold_expression(left, new_state, item_processor),
                item_processor,
            )
        }
        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(operand, ..)) => {
            fold_expression(operand, new_state, item_processor)
        }
        ExpressionKind::EnumType(items) => {
            items.iter().fold(new_state, |state, (_, field_expr)| {
                fold_expression(field_expr, state, item_processor)
            })
        }
        ExpressionKind::Match {
            value, branches, ..
        } => {
            let state_with_value = fold_expression(value, new_state, item_processor);
            branches
                .iter()
                .fold(state_with_value, |state, (_, branch)| {
                    fold_expression(branch, state, item_processor)
                })
        }
        ExpressionKind::EnumValue {
            enum_type, payload, ..
        } => {
            let state_with_type = fold_expression(enum_type, new_state, item_processor);
            if let Some(payload_expr) = payload {
                fold_expression(payload_expr, state_with_type, item_processor)
            } else {
                state_with_type
            }
        }
        ExpressionKind::EnumConstructor {
            enum_type,
            payload_type,
            ..
        } => {
            let state_with_type = fold_expression(enum_type, new_state, item_processor);
            fold_expression(payload_type, state_with_type, item_processor)
        }
        ExpressionKind::EnumAccess { enum_expr, .. } => {
            fold_expression(enum_expr, new_state, item_processor)
        }
        ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            let state_with_condition = fold_expression(condition, new_state, item_processor);
            let state_with_then =
                fold_expression(then_branch, state_with_condition, item_processor);
            if let Some(else_expr) = else_branch {
                fold_expression(else_expr, state_with_then, item_processor)
            } else {
                state_with_then
            }
        }
        ExpressionKind::AttachImplementation {
            type_expr,
            implementation,
            ..
        } => {
            let state_with_type = fold_expression(type_expr, new_state, item_processor);
            fold_expression(implementation, state_with_type, item_processor)
        }
        ExpressionKind::Function {
            return_type, body, ..
        } => {
            let state_with_return = if let Some(ret_type) = return_type {
                fold_expression(ret_type, new_state, item_processor)
            } else {
                new_state
            };
            fold_expression(body, state_with_return, item_processor)
        }
        ExpressionKind::FunctionType {
            parameter,
            return_type,
            ..
        } => {
            let state_with_param = fold_expression(parameter, new_state, item_processor);
            fold_expression(return_type, state_with_param, item_processor)
        }
        ExpressionKind::Struct(items) => items.iter().fold(new_state, |state, (_, field_expr)| {
            fold_expression(field_expr, state, item_processor)
        }),
        ExpressionKind::Literal(..) => new_state,
        ExpressionKind::Identifier(..) => new_state,
        ExpressionKind::Operation { left, right, .. } => {
            let state_with_left = fold_expression(left, new_state, item_processor);
            fold_expression(right, state_with_left, item_processor)
        }
        ExpressionKind::Assignment { expr, .. } => fold_expression(expr, new_state, item_processor),
        ExpressionKind::FunctionCall {
            function, argument, ..
        } => {
            let state_with_function = fold_expression(function, new_state, item_processor);
            fold_expression(argument, state_with_function, item_processor)
        }
        ExpressionKind::PropertyAccess { object, .. } => {
            fold_expression(object, new_state, item_processor)
        }
        ExpressionKind::Binding(binding) => {
            fold_expression(&binding.expr, new_state, item_processor)
        }
        ExpressionKind::Block(expressions) => expressions.iter().fold(new_state, |state, expr| {
            fold_expression(expr, state, item_processor)
        }),
        ExpressionKind::Diverge { value, .. } => {
            if let Some(value_expr) = value {
                fold_expression(value_expr, new_state, item_processor)
            } else {
                new_state
            }
        }
        ExpressionKind::Loop { body, .. } => fold_expression(body, new_state, item_processor),
    }
}

fn identifiers_created_or_modified(expr: &Expression) -> HashSet<Identifier> {
    fold_expression(
        expr,
        HashSet::new(),
        &|expr, mut identifiers| match &expr.kind {
            ExpressionKind::Binding(binding) => {
                collect_bound_identifiers_from_pattern(&binding.pattern, &mut identifiers);
                identifiers
            }
            ExpressionKind::Assignment { target, .. } => {
                identifiers.extend(target.get_used_identifiers());
                identifiers
            }
            _ => identifiers,
        },
    )
}

fn identifiers_used(expr: &Expression) -> HashSet<Identifier> {
    fold_expression(expr, HashSet::new(), &|current_expr, mut used| {
        match &current_expr.kind {
            ExpressionKind::Identifier(identifier) => {
                used.insert(identifier.clone());
            }
            ExpressionKind::Assignment { target, .. } => {
                used.extend(target.get_used_identifiers());
            }
            _ => {}
        }
        used
    })
}

fn interpret_block(
    expressions: Vec<Expression>,
    span: SourceSpan,
    context: &mut Context,
) -> Result<(Expression, Context), Diagnostic> {
    let outer_context = context.clone();
    context.bindings.push(HashMap::new());
    let mut interpreted_expressions = Vec::new();
    let mut preserved_expression_indicies = HashSet::new();

    for (expr_idx, expression) in expressions.into_iter().enumerate() {
        let value = interpret_expression(expression, context)?;
        if expression_contains_external_mutation(&value, &outer_context)
            || expression_does_diverge(&value, true, false)
            || expression_exports(&value)
        {
            preserved_expression_indicies.insert(expr_idx);
        }

        if matches!(
            value.kind,
            ExpressionKind::Diverge {
                divergance_type: DivergeExpressionType::Break | DivergeExpressionType::Return,
                ..
            }
        ) {
            let block_context = context.clone();
            context.bindings.pop();
            return Ok((value, block_context));
        }
        interpreted_expressions.push(value);
    }

    preserved_expression_indicies.insert(interpreted_expressions.len() - 1);

    let expression_usage: Vec<HashSet<Identifier>> = interpreted_expressions
        .iter()
        .map(identifiers_used)
        .collect();

    let expression_modifications: Vec<HashSet<Identifier>> = interpreted_expressions
        .iter()
        .map(identifiers_created_or_modified)
        .collect();

    let mut needed_identifiers: HashSet<Identifier> = HashSet::new();
    for idx in (0..interpreted_expressions.len()).rev() {
        let mut preserve_current = preserved_expression_indicies.contains(&idx);
        if !preserve_current
            && expression_modifications[idx]
                .iter()
                .any(|identifier| needed_identifiers.contains(identifier))
        {
            preserved_expression_indicies.insert(idx);
            preserve_current = true;
        }

        if preserve_current {
            // for identifier in &expression_modifications[idx] {
            //     needed_identifiers.remove(identifier);
            // }

            for identifier in &expression_usage[idx] {
                needed_identifiers.insert(identifier.clone());
            }
        }
    }

    let block_context = context.clone();
    context.bindings.pop();
    if preserved_expression_indicies.len() == 1 {
        // Only last expression preserved
        Ok((
            interpreted_expressions.into_iter().last().unwrap(),
            block_context,
        ))
    } else {
        // If we have preserved bindings, we need to return a Block that contains them
        // AND the final value.
        let preserved_expressions = interpreted_expressions
            .into_iter()
            .enumerate()
            .filter(|(idx, _)| preserved_expression_indicies.contains(idx))
            .map(|(_, expr)| expr)
            .collect();

        Ok((
            Expression::new(ExpressionKind::Block(preserved_expressions), span),
            block_context,
        ))
    }
}

pub fn interpret_program(
    expr: Expression,
    context: &mut Context,
) -> Result<(Expression, Context), Diagnostic> {
    match expr {
        Expression {
            kind: ExpressionKind::Block(expressions),
            span,
        } => interpret_block(expressions, span, context),
        other => interpret_expression(other, context).map(|value| (value, context.clone())),
    }
}

fn ensure_lvalue_mutable(
    target: &LValue,
    context: &Context,
    span: SourceSpan,
) -> Result<(), Diagnostic> {
    match target {
        LValue::Identifier(identifier, target_span) => {
            let (_, annotations) = context.get_identifier(identifier).ok_or_else(|| {
                diagnostic(
                    format!("Cannot assign to unbound identifier: {}", identifier.name),
                    *target_span,
                )
            })?;

            if !annotations
                .iter()
                .any(|ann| matches!(ann, BindingAnnotation::Mutable(_)))
            {
                return Err(diagnostic(
                    format!("Cannot assign to immutable identifier: {}", identifier.name),
                    span,
                ));
            }
            Ok(())
        }
        LValue::PropertyAccess { object, .. } => ensure_lvalue_mutable(object, context, span),
    }
}

fn lvalue_display_name(lvalue: &LValue) -> String {
    match lvalue {
        LValue::Identifier(Identifier { name, .. }, _) => name.clone(),
        LValue::PropertyAccess {
            object, property, ..
        } => {
            format!("{}.{}", lvalue_display_name(object), property)
        }
    }
}

fn get_lvalue_type(
    target: &LValue,
    context: &Context,
    span: SourceSpan,
) -> Result<Expression, Diagnostic> {
    ensure_lvalue_mutable(target, context, span)?;

    match target {
        LValue::Identifier(identifier, target_span) => {
            let (binding_ctx, _) = context.get_identifier(identifier).ok_or_else(|| {
                diagnostic(
                    format!("Cannot assign to unbound identifier: {}", identifier.name),
                    *target_span,
                )
            })?;

            match binding_ctx {
                BindingContext::Bound(value, _) => get_type_of_expression(value, context),
                BindingContext::UnboundWithType(type_expr) => Ok(type_expr.clone()),
                BindingContext::UnboundWithoutType => Err(diagnostic(
                    format!("Cannot determine type of {}", identifier.name),
                    *target_span,
                )),
            }
        }
        LValue::PropertyAccess {
            object,
            property,
            span: prop_span,
        } => {
            let object_type = get_lvalue_type(object, context, *prop_span)?;
            let Expression {
                kind: ExpressionKind::Struct(fields),
                ..
            } = object_type
            else {
                return Err(diagnostic("Property access on non-struct type", *prop_span));
            };

            let field_type = fields
                .iter()
                .find(|(id, _)| id.name == *property)
                .map(|(_, ty)| ty.clone())
                .ok_or_else(|| {
                    diagnostic(
                        format!("Field {} not found in struct type", property),
                        *prop_span,
                    )
                })?;
            Ok(field_type)
        }
    }
}

fn get_lvalue_value(
    target: &LValue,
    context: &mut Context,
) -> Result<Option<Expression>, Diagnostic> {
    match target {
        LValue::Identifier(identifier, target_span) => {
            let (binding_ctx, _) = context.get_identifier(identifier).ok_or_else(|| {
                diagnostic(
                    format!("Cannot assign to unbound identifier: {}", identifier.name),
                    *target_span,
                )
            })?;

            match binding_ctx {
                BindingContext::Bound(value, _) => {
                    if is_resolved_constant(value) {
                        Ok(Some(value.clone()))
                    } else {
                        Ok(None)
                    }
                }
                BindingContext::UnboundWithType(_) => Ok(None),
                BindingContext::UnboundWithoutType => Err(diagnostic(
                    format!(
                        "Cannot assign to uninitialized identifier: {}",
                        identifier.name
                    ),
                    *target_span,
                )),
            }
        }
        LValue::PropertyAccess {
            object,
            property,
            span: prop_span,
        } => {
            let Some(object_value) = get_lvalue_value(object, context)? else {
                return Ok(None);
            };
            let Expression {
                kind: ExpressionKind::Struct(fields),
                span: struct_span,
            } = object_value
            else {
                return Err(diagnostic(
                    "Property access on non-struct value",
                    *prop_span,
                ));
            };

            fields
                .into_iter()
                .find(|(id, _)| id.name == *property)
                .map(|(_, expr)| Some(expr))
                .ok_or_else(|| {
                    diagnostic(format!("Missing field {} in struct", property), struct_span)
                })
        }
    }
}

fn apply_lvalue_update(
    target: &LValue,
    value: Expression,
    context: &mut Context,
    span: SourceSpan,
) -> Result<(), Diagnostic> {
    match target {
        LValue::Identifier(identifier, _) => {
            let value_type = get_type_of_expression(&value, context).ok();
            let type_context = context.clone();

            let Some((binding_ctx, _annotations)) = context.get_mut_identifier(identifier) else {
                return Err(diagnostic(
                    format!("Cannot assign to unbound identifier: {}", identifier.name),
                    span,
                ));
            };

            let expected_type = match binding_ctx {
                BindingContext::Bound(existing, _) => {
                    get_type_of_expression(existing, &type_context).ok()
                }
                BindingContext::UnboundWithType(expected_ty) => Some(expected_ty.clone()),
                BindingContext::UnboundWithoutType => None,
            };

            if let (Some(expected_ty), Some(actual_ty)) = (&expected_type, &value_type)
                && !types_equivalent(&expected_ty.kind, &actual_ty.kind)
            {
                return Err(diagnostic(
                    format!(
                        "Cannot assign value of mismatched type to {}",
                        identifier.name
                    ),
                    span,
                ));
            }

            let binding_type = expected_type.or(value_type);

            if is_resolved_constant(&value) {
                *binding_ctx = BindingContext::Bound(value, PreserveBehavior::Inline);
            } else if let Some(binding_ty) = binding_type {
                *binding_ctx = BindingContext::UnboundWithType(binding_ty);
            } else {
                *binding_ctx = BindingContext::UnboundWithoutType;
            }

            Ok(())
        }
        LValue::PropertyAccess {
            object,
            property,
            span: prop_span,
        } => {
            let Some(current_object) = get_lvalue_value(object, context)? else {
                for invalidated_identifier in object.get_used_identifiers() {
                    let binding_to_invalidate =
                        context.get_identifier(&invalidated_identifier).unwrap();
                    if let Some(binding_ty) = binding_to_invalidate.0.get_bound_type(context)? {
                        let binding_to_invalidate =
                            context.get_mut_identifier(&invalidated_identifier).unwrap();
                        binding_to_invalidate.0 = BindingContext::UnboundWithType(binding_ty)
                    }
                }
                return Ok(());
            };
            let Expression {
                kind: ExpressionKind::Struct(mut fields),
                span: struct_span,
            } = current_object
            else {
                return Err(diagnostic(
                    "Property access on non-struct value",
                    *prop_span,
                ));
            };

            let mut found = false;
            for (field_id, field_expr) in fields.iter_mut() {
                if field_id.name == *property {
                    *field_expr = value.clone();
                    found = true;
                    break;
                }
            }

            if !found {
                return Err(diagnostic(
                    format!("Missing field {} in struct", property),
                    *prop_span,
                ));
            }

            let updated_struct = Expression::new(ExpressionKind::Struct(fields), struct_span);
            apply_lvalue_update(object, updated_struct, context, span)
        }
    }
}

fn apply_assignment(
    target: LValue,
    value: Expression,
    span: SourceSpan,
    context: &mut Context,
) -> Result<Expression, Diagnostic> {
    ensure_lvalue_mutable(&target, context, span)?;

    let value_type = get_type_of_expression(&value, context).ok();
    let target_type = get_lvalue_type(&target, context, span)?;

    if let Some(actual_type) = value_type
        && !types_equivalent(&target_type.kind, &actual_type.kind)
    {
        return Err(diagnostic(
            format!(
                "Cannot assign value of mismatched type to {}",
                lvalue_display_name(&target)
            ),
            span,
        ));
    }

    apply_lvalue_update(&target, value.clone(), context, span)?;

    Ok(Expression::new(
        ExpressionKind::Assignment {
            target,
            expr: Box::new(value),
        },
        span,
    ))
}

fn pattern_has_mutable_annotation(pattern: &BindingPattern) -> bool {
    match pattern {
        BindingPattern::Annotated {
            annotations,
            pattern,
            ..
        } => {
            annotations
                .iter()
                .any(|ann| matches!(ann, BindingAnnotation::Mutable(_)))
                || pattern_has_mutable_annotation(pattern)
        }
        BindingPattern::Struct(items, _) => items
            .iter()
            .any(|(_, pat)| pattern_has_mutable_annotation(pat)),
        BindingPattern::EnumVariant { payload, .. } => payload
            .as_ref()
            .map(|pat| pattern_has_mutable_annotation(pat))
            .unwrap_or(false),
        BindingPattern::TypeHint(inner, _, _) => pattern_has_mutable_annotation(inner),
        BindingPattern::Identifier(..) | BindingPattern::Literal(..) => false,
    }
}

fn interpret_binding(
    binding: Binding,
    context: &mut Context,
) -> Result<(Expression, PreserveBehavior), Diagnostic> {
    let interpreted_pattern = interpret_binding_pattern(binding.pattern, context)?;
    let value = interpret_expression(binding.expr.clone(), context)?;
    if let Ok(value_type) = get_type_of_expression(&value, context) {
        bind_pattern_blanks(
            interpreted_pattern.clone(),
            context,
            Vec::new(),
            Some(value_type),
        )?;
    }
    let value_is_constant = is_resolved_constant(&value);
    let (bound_success, preserve_behavior) = bind_pattern_from_value(
        interpreted_pattern.clone(),
        &value,
        context,
        Vec::new(),
        if value_is_constant {
            PreserveBehavior::Inline
        } else {
            PreserveBehavior::PreserveUsage
        },
    )?;
    let binding_expr = ExpressionKind::Binding(Box::new(Binding {
        pattern: interpreted_pattern,
        expr: value,
    }))
    .with_span(dummy_span());

    Ok((
        if preserve_behavior != PreserveBehavior::Inline || (!value_is_constant && !bound_success) {
            binding_expr
        } else {
            ExpressionKind::Literal(ExpressionLiteral::Boolean(bound_success))
                .with_span(dummy_span())
        },
        preserve_behavior,
    ))
}

fn parse_binding_annotation(
    ann: BindingAnnotation,
    context: &Context,
) -> Result<BindingAnnotation, Diagnostic> {
    let mut context = context.clone();
    match ann {
        BindingAnnotation::Export(expr, span) => Ok(BindingAnnotation::Export(
            interpret_expression(expr, &mut context)?,
            span,
        )),
        BindingAnnotation::Mutable(span) => Ok(BindingAnnotation::Mutable(span)),
    }
}

fn bind_pattern_blanks(
    pattern: BindingPattern,
    context: &mut Context,
    passed_annotations: Vec<BindingAnnotation>,
    type_hint: Option<Expression>,
) -> Result<(), Diagnostic> {
    match pattern {
        BindingPattern::Identifier(identifier, _) => {
            if let Some(type_expr) = type_hint {
                context.bindings.last_mut().unwrap().insert(
                    identifier,
                    (
                        BindingContext::UnboundWithType(type_expr),
                        passed_annotations.clone(),
                    ),
                );
            } else {
                context.bindings.last_mut().unwrap().insert(
                    identifier,
                    (
                        BindingContext::UnboundWithoutType,
                        passed_annotations.clone(),
                    ),
                );
            }
            Ok(())
        }
        BindingPattern::Literal(_, _) => Ok(()),
        BindingPattern::Struct(pattern_items, _) => {
            let mut type_lookup = None;
            if let Some(Expression {
                kind: ExpressionKind::Struct(type_fields),
                ..
            }) = &type_hint
            {
                type_lookup = Some(type_fields.clone());
            }

            for (field_identifier, field_pattern) in pattern_items {
                let field_type_hint = type_lookup.as_ref().and_then(|fields| {
                    fields
                        .iter()
                        .find(|(name, _)| name.name == field_identifier.name)
                        .map(|(_, ty)| ty.clone())
                });
                bind_pattern_blanks(
                    field_pattern,
                    context,
                    passed_annotations.clone(),
                    field_type_hint,
                )?;
            }

            Ok(())
        }
        BindingPattern::EnumVariant {
            enum_type,
            variant,
            payload,
            ..
        } => {
            let type_hint = type_hint.or_else(|| resolve_enum_type_expression(&enum_type, context));

            let payload_hint = type_hint
                .as_ref()
                .and_then(|hint| enum_variant_info(hint, &variant).map(|(_, ty)| ty));

            if let Some(payload_pattern) = payload {
                bind_pattern_blanks(*payload_pattern, context, passed_annotations, payload_hint)?;
            }
            Ok(())
        }
        BindingPattern::TypeHint(inner, type_hint, _) => {
            bind_pattern_blanks(*inner, context, passed_annotations, Some(*type_hint))
        }
        BindingPattern::Annotated {
            pattern,
            annotations,
            ..
        } => bind_pattern_blanks(
            *pattern,
            context,
            passed_annotations
                .into_iter()
                .chain(
                    annotations
                        .into_iter()
                        .map(|ann| parse_binding_annotation(ann, context))
                        .collect::<Result<Vec<_>, _>>()?,
                )
                .collect(),
            type_hint,
        ),
    }
}

fn value_preserve_behavior(
    value: &Expression,
    preserve_behavior: PreserveBehavior,
) -> PreserveBehavior {
    if let ExpressionKind::Function { parameter, .. } = &value.kind
        && pattern_has_mutable_annotation(parameter)
    {
        return PreserveBehavior::PreserveUsage;
    }
    preserve_behavior
}

fn bind_pattern_from_value(
    pattern: BindingPattern,
    value: &Expression,
    context: &mut Context,
    passed_annotations: Vec<BindingAnnotation>,
    preserve_behavior: PreserveBehavior,
) -> Result<(bool, PreserveBehavior), Diagnostic> {
    match pattern {
        BindingPattern::Identifier(identifier, _) => {
            let new_preserve_behavior = value_preserve_behavior(value, preserve_behavior);
            context.bindings.last_mut().unwrap().insert(
                identifier,
                (
                    BindingContext::Bound(value.clone(), new_preserve_behavior),
                    passed_annotations.clone(),
                ),
            );
            Ok((true, new_preserve_behavior))
        }
        BindingPattern::Literal(literal, _) => match (literal, &value.kind) {
            (
                ExpressionLiteral::Number(pattern_value),
                ExpressionKind::Literal(ExpressionLiteral::Number(value)),
            ) => Ok((pattern_value == *value, preserve_behavior)),
            (
                ExpressionLiteral::Boolean(pattern_value),
                ExpressionKind::Literal(ExpressionLiteral::Boolean(value)),
            ) => Ok((pattern_value == *value, preserve_behavior)),
            _ => Ok((false, preserve_behavior)),
        },
        BindingPattern::Struct(pattern_items, span) => {
            let ExpressionKind::Struct(struct_items) = &value.kind else {
                // not sure if it is ok to fail here, it could be an identifier of the struct type
                return Err(diagnostic("Struct pattern requires struct value", span));
            };

            let mut preserve_behavior = preserve_behavior;
            let mut overall_matched = true;

            for (field_identifier, field_pattern) in pattern_items {
                let field_span = field_pattern.span();
                let field_value = struct_items
                    .iter()
                    .find(|(value_identifier, _)| value_identifier.name == field_identifier.name)
                    .map(|(_, expr)| expr)
                    .ok_or_else(|| {
                        diagnostic(
                            format!("Missing field {}", field_identifier.name),
                            field_span,
                        )
                    })?;

                let (matched, new_preserve_behavior) = bind_pattern_from_value(
                    field_pattern,
                    field_value,
                    context,
                    passed_annotations.clone(),
                    preserve_behavior,
                )?;
                preserve_behavior = Ord::max(preserve_behavior, new_preserve_behavior);
                if !matched {
                    overall_matched = false;
                }
            }

            Ok((overall_matched, preserve_behavior))
        }
        BindingPattern::EnumVariant {
            enum_type,
            variant,
            payload,
            span,
        } => {
            // not sure if it is ok to fail here, it could be an identifier of the correct type type
            let Expression {
                kind:
                    ExpressionKind::EnumValue {
                        enum_type: value_enum,
                        variant: value_variant,
                        payload: value_payload,
                        ..
                    },
                ..
            } = value
            else {
                return Ok((false, preserve_behavior));
            };

            let enum_type_name = match enum_type.as_ref() {
                Expression {
                    kind: ExpressionKind::Identifier(id),
                    ..
                } => id.name.clone(),
                _ => "<unknown>".to_string(),
            };

            let expected_enum_type = resolve_enum_type_expression(enum_type.as_ref(), context)
                .ok_or_else(|| {
                    diagnostic(
                        format!("Enum pattern references unknown type: {}", enum_type_name),
                        span,
                    )
                })?;

            if !types_equivalent(&expected_enum_type.kind, &value_enum.kind) {
                return Ok((false, preserve_behavior));
            }

            if value_variant.name != variant.name {
                return Ok((false, preserve_behavior));
            }

            if let Some(payload_pattern) = payload {
                let payload_value_owned = value_payload
                    .as_ref()
                    .map(|v| v.as_ref().clone())
                    .unwrap_or_else(|| empty_struct_expr(span));
                bind_pattern_from_value(
                    *payload_pattern.clone(),
                    &payload_value_owned,
                    context,
                    passed_annotations,
                    preserve_behavior,
                )
            } else {
                Ok((true, preserve_behavior))
            }
        }
        BindingPattern::TypeHint(inner, _, _) => bind_pattern_from_value(
            *inner,
            value,
            context,
            passed_annotations,
            preserve_behavior,
        ),
        BindingPattern::Annotated {
            pattern,
            annotations,
            ..
        } => {
            let new_annotations = annotations
                .into_iter()
                .map(|ann| parse_binding_annotation(ann, context))
                .collect::<Result<Vec<_>, _>>()?;
            let new_preserve_behavior = if new_annotations
                .iter()
                .any(|ann| matches!(ann, BindingAnnotation::Export(_, _)))
            {
                preserve_behavior.max(PreserveBehavior::PreserveBinding)
            } else {
                preserve_behavior
            };
            let new_preserve_behavior = if new_annotations
                .iter()
                .any(|ann| matches!(ann, BindingAnnotation::Mutable(_)))
            {
                new_preserve_behavior.max(PreserveBehavior::PreserveUsageInLoops)
            } else {
                new_preserve_behavior
            };
            bind_pattern_from_value(
                *pattern,
                value,
                context,
                passed_annotations
                    .into_iter()
                    .chain(new_annotations)
                    .collect(),
                new_preserve_behavior,
            )
        }
    }
}

fn interpret_numeric_intrinsic<F>(
    left: Expression,
    right: Expression,
    span: SourceSpan,
    op: F,
) -> Result<Expression, Diagnostic>
where
    F: Fn(i32, i32) -> i32,
{
    let left_value = evaluate_numeric_operand(left)?;
    let right_value = evaluate_numeric_operand(right)?;
    Ok(Expression::new(
        ExpressionKind::Literal(ExpressionLiteral::Number(op(left_value, right_value))),
        span,
    ))
}

fn interpret_divide_intrinsic(
    left: Expression,
    right: Expression,
    span: SourceSpan,
) -> Result<Expression, Diagnostic> {
    let left_value = evaluate_numeric_operand(left)?;
    let right_value = evaluate_numeric_operand(right)?;

    if right_value == 0 {
        return Err(diagnostic("Division by zero", span));
    }

    Ok(Expression::new(
        ExpressionKind::Literal(ExpressionLiteral::Number(left_value / right_value)),
        span,
    ))
}

fn interpret_comparison_intrinsic<F>(
    left: Expression,
    right: Expression,
    span: SourceSpan,
    op: F,
) -> Result<Expression, Diagnostic>
where
    F: Fn(i32, i32) -> bool,
{
    let left_value = evaluate_numeric_operand(left)?;
    let right_value = evaluate_numeric_operand(right)?;
    Ok(
        ExpressionKind::Literal(ExpressionLiteral::Boolean(op(left_value, right_value)))
            .with_span(span),
    )
}

fn evaluate_numeric_operand(expr: Expression) -> Result<i32, Diagnostic> {
    match expr {
        Expression {
            kind: ExpressionKind::Literal(ExpressionLiteral::Number(value)),
            ..
        } => Ok(value),
        _ => Err(diagnostic(
            "Expected numeric literal during intrinsic operation",
            expr.span(),
        )),
    }
}

fn interpret_boolean_intrinsic<F>(
    left: Expression,
    right: Expression,
    span: SourceSpan,
    op: F,
) -> Result<Expression, Diagnostic>
where
    F: Fn(bool, bool) -> bool,
{
    let left_value = evaluate_boolean_operand(left)?;
    let right_value = evaluate_boolean_operand(right)?;
    Ok(Expression::new(
        ExpressionKind::Literal(ExpressionLiteral::Boolean(op(left_value, right_value))),
        span,
    ))
}

fn evaluate_boolean_operand(expr: Expression) -> Result<bool, Diagnostic> {
    match expr {
        Expression {
            kind: ExpressionKind::Literal(ExpressionLiteral::Boolean(value)),
            ..
        } => Ok(value),
        _ => Err(diagnostic(
            "Expected boolean literal during intrinsic operation",
            expr.span(),
        )),
    }
}

fn is_resolved_constant(expr: &Expression) -> bool {
    match &expr.kind {
        ExpressionKind::Literal(_) | ExpressionKind::IntrinsicType(_) => true,
        ExpressionKind::EnumType(variants) => {
            variants.iter().all(|(_, ty)| is_resolved_constant(ty))
        }
        ExpressionKind::EnumValue {
            enum_type, payload, ..
        } => {
            is_resolved_constant(enum_type)
                && payload
                    .as_ref()
                    .map(|p| is_resolved_constant(p))
                    .unwrap_or(true)
        }
        ExpressionKind::EnumConstructor {
            enum_type,
            payload_type,
            ..
        } => is_resolved_constant(enum_type) && is_resolved_constant(payload_type),
        ExpressionKind::Struct(items) => items
            .iter()
            .all(|(_, value_expr)| is_resolved_constant(value_expr)),
        ExpressionKind::AttachImplementation {
            type_expr,
            implementation,
            ..
        } => is_resolved_constant(type_expr) && is_resolved_constant(implementation),
        ExpressionKind::Function {
            parameter,
            return_type,
            body,
            ..
        } => {
            let new_function_context = {
                let mut ctx = Context::empty();
                ctx.bindings.push(HashMap::new());
                bind_pattern_blanks(parameter.clone(), &mut ctx, Vec::new(), None).unwrap();
                ctx
            };
            is_resolved_constant(return_type.as_ref().unwrap())
                && is_resolved_const_function_expression(body, &new_function_context)
        }
        ExpressionKind::FunctionType {
            parameter,
            return_type,
            ..
        } => is_resolved_constant(parameter) && is_resolved_constant(return_type),
        ExpressionKind::Assignment { .. } => false,
        _ => false,
    }
}

fn is_resolved_const_function_expression(expr: &Expression, function_context: &Context) -> bool {
    match &expr.kind {
        ExpressionKind::Literal(_) | ExpressionKind::IntrinsicType(_) => true,
        ExpressionKind::Struct(items) => items.iter().all(|(_, value_expr)| {
            is_resolved_const_function_expression(value_expr, function_context)
        }),
        ExpressionKind::AttachImplementation {
            type_expr,
            implementation,
            ..
        } => {
            is_resolved_const_function_expression(type_expr, function_context)
                && is_resolved_const_function_expression(implementation, function_context)
        }
        ExpressionKind::Function {
            parameter,
            return_type,
            body,
            ..
        } => {
            let new_function_context = {
                let mut ctx = function_context.clone();
                bind_pattern_blanks(parameter.clone(), &mut ctx, Vec::new(), None).unwrap();
                ctx
            };
            is_resolved_const_function_expression(
                return_type.as_ref().unwrap(),
                &new_function_context,
            ) && is_resolved_const_function_expression(body, &new_function_context)
        }
        ExpressionKind::FunctionType {
            parameter,
            return_type,
            ..
        } => {
            is_resolved_const_function_expression(parameter, function_context)
                && is_resolved_const_function_expression(return_type, function_context)
        }
        ExpressionKind::Identifier(ident) => function_context.contains_identifier(ident),
        ExpressionKind::IntrinsicOperation(intrinsic_operation) => match intrinsic_operation {
            IntrinsicOperation::Binary(left, right, _) => {
                is_resolved_const_function_expression(left, function_context)
                    && is_resolved_const_function_expression(right, function_context)
            }
            IntrinsicOperation::Unary(operand, _) => {
                is_resolved_const_function_expression(operand, function_context)
            }
        },
        ExpressionKind::EnumType(cases) => cases.iter().all(|(_, case_expr)| {
            is_resolved_const_function_expression(case_expr, function_context)
        }),
        ExpressionKind::EnumValue {
            enum_type, payload, ..
        } => {
            is_resolved_const_function_expression(enum_type, function_context)
                && payload
                    .as_ref()
                    .map(|p| is_resolved_const_function_expression(p, function_context))
                    .unwrap_or(true)
        }
        ExpressionKind::EnumConstructor {
            enum_type,
            payload_type,
            ..
        } => {
            is_resolved_const_function_expression(enum_type, function_context)
                && is_resolved_const_function_expression(payload_type, function_context)
        }
        ExpressionKind::FunctionCall {
            function, argument, ..
        } => {
            is_resolved_const_function_expression(function, function_context)
                && is_resolved_const_function_expression(argument, function_context)
        }
        _ => false,
    }
}

pub fn intrinsic_context() -> Context {
    let mut context = Context {
        bindings: vec![HashMap::new()],
        in_loop: false,
    };

    context.bindings.last_mut().unwrap().insert(
        Identifier::new("type"),
        (
            BindingContext::Bound(
                ExpressionKind::AttachImplementation {
                    type_expr: Box::new(intrinsic_type_expr(IntrinsicType::Type)),
                    implementation: Box::new(
                        ExpressionKind::Struct(vec![]).with_span(dummy_span()),
                    ),
                }
                .with_span(dummy_span()),
                PreserveBehavior::Inline,
            ),
            Vec::new(),
        ),
    );

    fn i32_binary_intrinsic(
        symbol: &str,
        operator: BinaryIntrinsicOperator,
    ) -> (Identifier, Expression) {
        let typed_pattern = |name: &str| {
            BindingPattern::TypeHint(
                Box::new(BindingPattern::Identifier(
                    Identifier::new(name.to_string()),
                    dummy_span(),
                )),
                Box::new(intrinsic_type_expr(IntrinsicType::I32)),
                dummy_span(),
            )
        };

        (
            Identifier::new(symbol.to_string()),
            ExpressionKind::Function {
                parameter: typed_pattern("self"),
                return_type: Some(Box::new(
                    ExpressionKind::FunctionType {
                        parameter: Box::new(intrinsic_type_expr(IntrinsicType::I32)),
                        return_type: Box::new(intrinsic_type_expr(IntrinsicType::I32)),
                    }
                    .with_span(dummy_span()),
                )),
                body: Box::new(
                    ExpressionKind::Function {
                        parameter: typed_pattern("other"),
                        return_type: Some(Box::new(intrinsic_type_expr(IntrinsicType::I32))),
                        body: Box::new(
                            ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(
                                Box::new(identifier_expr("self")),
                                Box::new(identifier_expr("other")),
                                operator,
                            ))
                            .with_span(dummy_span()),
                        ),
                    }
                    .with_span(dummy_span()),
                ),
            }
            .with_span(dummy_span()),
        )
    }

    context.bindings.last_mut().unwrap().insert(
        Identifier::new("i32"),
        (
            BindingContext::Bound(
                ExpressionKind::AttachImplementation {
                    type_expr: Box::new(intrinsic_type_expr(IntrinsicType::I32)),
                    implementation: Box::new(
                        ExpressionKind::Struct(vec![
                            i32_binary_intrinsic("+", BinaryIntrinsicOperator::I32Add),
                            i32_binary_intrinsic("-", BinaryIntrinsicOperator::I32Subtract),
                            i32_binary_intrinsic("*", BinaryIntrinsicOperator::I32Multiply),
                            i32_binary_intrinsic("/", BinaryIntrinsicOperator::I32Divide),
                            i32_binary_intrinsic("==", BinaryIntrinsicOperator::I32Equal),
                            i32_binary_intrinsic("!=", BinaryIntrinsicOperator::I32NotEqual),
                            i32_binary_intrinsic("<", BinaryIntrinsicOperator::I32LessThan),
                            i32_binary_intrinsic(">", BinaryIntrinsicOperator::I32GreaterThan),
                            i32_binary_intrinsic("<=", BinaryIntrinsicOperator::I32LessThanOrEqual),
                            i32_binary_intrinsic(
                                ">=",
                                BinaryIntrinsicOperator::I32GreaterThanOrEqual,
                            ),
                        ])
                        .with_span(dummy_span()),
                    ),
                }
                .with_span(dummy_span()),
                PreserveBehavior::Inline,
            ),
            Vec::new(),
        ),
    );

    fn bool_binary_intrinsic(
        symbol: &str,
        operator: BinaryIntrinsicOperator,
    ) -> (Identifier, Expression) {
        let typed_pattern = |name: &str| {
            BindingPattern::TypeHint(
                Box::new(BindingPattern::Identifier(
                    Identifier::new(name.to_string()),
                    dummy_span(),
                )),
                Box::new(intrinsic_type_expr(IntrinsicType::Boolean)),
                dummy_span(),
            )
        };

        (
            Identifier::new(symbol.to_string()),
            ExpressionKind::Function {
                parameter: typed_pattern("self"),
                return_type: Some(Box::new(Expression::new(
                    ExpressionKind::FunctionType {
                        parameter: Box::new(intrinsic_type_expr(IntrinsicType::Boolean)),
                        return_type: Box::new(intrinsic_type_expr(IntrinsicType::Boolean)),
                    },
                    dummy_span(),
                ))),
                body: Box::new(Expression::new(
                    ExpressionKind::Function {
                        parameter: typed_pattern("other"),
                        return_type: Some(Box::new(intrinsic_type_expr(IntrinsicType::Boolean))),
                        body: Box::new(Expression::new(
                            ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(
                                Box::new(identifier_expr("self")),
                                Box::new(identifier_expr("other")),
                                operator,
                            )),
                            dummy_span(),
                        )),
                    },
                    dummy_span(),
                )),
            }
            .with_span(dummy_span()),
        )
    }

    context.bindings.last_mut().unwrap().insert(
        Identifier::new("bool"),
        (
            BindingContext::Bound(
                ExpressionKind::AttachImplementation {
                    type_expr: Box::new(intrinsic_type_expr(IntrinsicType::Boolean)),
                    implementation: Box::new(
                        ExpressionKind::Struct(vec![
                            bool_binary_intrinsic("==", BinaryIntrinsicOperator::I32Equal),
                            bool_binary_intrinsic("!=", BinaryIntrinsicOperator::I32NotEqual),
                            bool_binary_intrinsic("&&", BinaryIntrinsicOperator::BooleanAnd),
                            bool_binary_intrinsic("||", BinaryIntrinsicOperator::BooleanOr),
                            bool_binary_intrinsic("^", BinaryIntrinsicOperator::BooleanXor),
                        ])
                        .with_span(dummy_span()),
                    ),
                }
                .with_span(dummy_span()),
                PreserveBehavior::Inline,
            ),
            Vec::new(),
        ),
    );

    context.bindings.last_mut().unwrap().insert(
        Identifier::new("true"),
        (
            BindingContext::Bound(
                ExpressionKind::Literal(ExpressionLiteral::Boolean(true)).with_span(dummy_span()),
                PreserveBehavior::Inline,
            ),
            Vec::new(),
        ),
    );

    context.bindings.last_mut().unwrap().insert(
        Identifier::new("false"),
        (
            BindingContext::Bound(
                ExpressionKind::Literal(ExpressionLiteral::Boolean(false)).with_span(dummy_span()),
                PreserveBehavior::Inline,
            ),
            Vec::new(),
        ),
    );

    context.bindings.last_mut().unwrap().insert(
        Identifier::new("js"),
        (
            BindingContext::Bound(
                ExpressionKind::Literal(ExpressionLiteral::Target(TargetLiteral::JSTarget))
                    .with_span(dummy_span()),
                PreserveBehavior::Inline,
            ),
            Vec::new(),
        ),
    );

    context.bindings.last_mut().unwrap().insert(
        Identifier::new("wasm"),
        (
            BindingContext::Bound(
                ExpressionKind::Literal(ExpressionLiteral::Target(TargetLiteral::WasmTarget))
                    .with_span(dummy_span()),
                PreserveBehavior::Inline,
            ),
            Vec::new(),
        ),
    );

    context.bindings.last_mut().unwrap().insert(
        Identifier::new("enum"),
        (
            BindingContext::Bound(
                ExpressionKind::Function {
                    parameter: BindingPattern::TypeHint(
                        Box::new(BindingPattern::Identifier(
                            Identifier::new("struct_arg"),
                            dummy_span(),
                        )),
                        Box::new(intrinsic_type_expr(IntrinsicType::Type)),
                        dummy_span(),
                    ),
                    return_type: Some(Box::new(intrinsic_type_expr(IntrinsicType::Type))),
                    body: Box::new(Expression::new(
                        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(
                            Box::new(Expression::new(
                                ExpressionKind::Identifier(Identifier::new("struct_arg")),
                                dummy_span(),
                            )),
                            UnaryIntrinsicOperator::EnumFromStruct,
                        )),
                        dummy_span(),
                    )),
                }
                .with_span(dummy_span()),
                PreserveBehavior::Inline,
            ),
            Vec::new(),
        ),
    );
    context
}

#[cfg(test)]
pub fn evaluate_text_to_raw_expression(program: &str) -> Result<(Expression, Context), Diagnostic> {
    let (expression, remaining) =
        crate::parsing::parse_block(program).expect("Failed to parse program text");
    assert!(
        remaining.trim().is_empty(),
        "Parser did not consume entire input, remaining: {remaining:?}"
    );

    let expression = uniquify::uniquify_program(expression);
    let mut context = intrinsic_context();
    interpret_program(expression, &mut context)
}

pub fn evaluate_text_to_expression(program: &str) -> Result<(Expression, Context), Diagnostic> {
    let (expression, remaining) = crate::parsing::parse_block(program)?;
    assert!(
        remaining.trim().is_empty(),
        "Parser did not consume entire input, remaining: {remaining:?}"
    );

    let expression = uniquify::uniquify_program(expression);
    let mut context = intrinsic_context();
    let (mut value, context) = interpret_program(expression, &mut context)?;

    println!("{}", value.pretty_print());

    while let ExpressionKind::Block(exprs) = value.kind {
        value = exprs.last().cloned().unwrap();
    }

    Ok((value, context))
}

#[cfg(test)]
fn evaluate_text_to_number(program: &str) -> i32 {
    match evaluate_text_to_expression(program)
        .unwrap_or_else(|e| {
            panic!(
                "Failed to interpret parsed expression: {:?}",
                e.render_with_source(program)
            )
        })
        .0
    {
        Expression {
            kind: ExpressionKind::Literal(ExpressionLiteral::Number(value)),
            ..
        } => value,
        _ => panic!("Expected numeric result"),
    }
}

#[test]
fn test_basic_binding_interpretation() {
    let mut context = intrinsic_context();

    let expr = ExpressionKind::Block(vec![
        ExpressionKind::Binding(Box::new(Binding {
            pattern: BindingPattern::TypeHint(
                Box::new(BindingPattern::Identifier(
                    Identifier::new("x"),
                    dummy_span(),
                )),
                Box::new(intrinsic_type_expr(IntrinsicType::I32)),
                dummy_span(),
            ),
            expr: literal_number_expr(5),
        }))
        .with_span(dummy_span()),
        identifier_expr("x"),
    ])
    .with_span(dummy_span());

    let result = interpret_expression(expr, &mut context).unwrap();

    if let ExpressionKind::Literal(ExpressionLiteral::Number(value)) = result.kind {
        assert_eq!(value, 5);
    } else {
        panic!("Expected a number literal as result");
    }
}

#[test]
fn interpret_text_basic_operations() {
    let cases = [
        ("5 + 10", 15),
        ("12 - 7", 5),
        ("3 * 4", 12),
        ("20 / 5", 4),
        ("2 + 3 * 4", 14),
    ];

    for (program, expected) in cases {
        assert_eq!(
            evaluate_text_to_number(program),
            expected,
            "program `{program}` produced unexpected value"
        );
    }
}

#[test]
fn test_basic_addition_interpretation() {
    let mut context = intrinsic_context();

    let expr = ExpressionKind::Operation {
        operator: "+".to_string(),
        left: Box::new(literal_number_expr(5)),
        right: Box::new(literal_number_expr(10)),
    }
    .with_span(dummy_span());

    let result = interpret_expression(expr, &mut context).unwrap();

    if let ExpressionKind::Literal(ExpressionLiteral::Number(value)) = result.kind {
        assert_eq!(value, 15);
    } else {
        panic!("Expected a number literal as result");
    }
}

#[test]
fn i32_is_constant() {
    let binding = intrinsic_context();
    let Some((BindingContext::Bound(expr, _), _)) = binding.get_identifier(&Identifier::new("i32"))
    else {
        panic!("i32 binding not found");
    };
    assert!(is_resolved_constant(expr));
}

#[test]
fn interpret_text_user_defined_function2() {
    let program = "
(bar: i32) => (
    bar + 1
)
    ";
    let (expr, _) = evaluate_text_to_expression(program).unwrap();
    assert!(matches!(expr.kind, ExpressionKind::Function { .. }));
}
#[test]
fn interpret_let_binding_function() {
    let program = "
Level2 := enum { Some = i32, None = {} };
Level1 := enum { Some = Level2, None = {} };

(export wasm) check := (x: i32) => (
    foo := if x > 0 then Level1::Some(Level2::Some(x)) else Level1::None;

    if Level1::Some(Level2::Some(b)) := foo then b else 0
);
{}
    ";
    let (expr, _) = evaluate_text_to_raw_expression(program).unwrap();
    println!("{expr:?}");
}

#[test]
fn interpret_text_user_defined_function() {
    let program = "
foo := (bar: i32) => (
    bar + 1
);
foo(123)
    ";
    assert_eq!(evaluate_text_to_number(program), 124);
}

#[test]
fn interpret_text_user_defined_tuple_arguments() {
    let program = "
foo2 := {bar1: i32, bar2: i32} => (
    bar1 + bar2
);
foo2{100, 24}
    ";
    assert_eq!(evaluate_text_to_number(program), 124);
}

#[test]
fn interpret_text_struct_property_access() {
    let program = "
point := { x = 5, y = 10 };
point.x
    ";
    assert_eq!(evaluate_text_to_number(program), 5);
}

#[test]
fn interpret_text_struct_property_call() {
    let program = "
container := {
    inc = (value: i32) => (
        value + 1
    )
};
container.inc(41)
    ";
    assert_eq!(evaluate_text_to_number(program), 42);
}

#[test]
fn interpret_binding_with_export_annotation() {
    let program = "
(export js) answer: i32 := 42;
answer
    ";
    assert_eq!(evaluate_text_to_number(program), 42);
}

#[test]
fn interpret_reports_unbound_identifier_span() {
    let source = "unknown";
    let (expr, remaining) = crate::parsing::parse_block(source).expect("parse should succeed");
    assert!(remaining.trim().is_empty());
    let mut context = intrinsic_context();
    let err = interpret_expression(expr, &mut context).expect_err("expected unbound identifier");
    let rendered = err.render_with_source(source);
    assert!(rendered.contains("Unbound identifier: unknown"));
    assert!(rendered.contains("line 1, column 1"));
}

#[test]
fn interpret_reports_calling_non_function_span() {
    let source = "5(1)";
    let (expr, remaining) = crate::parsing::parse_block(source).expect("parse should succeed");
    assert!(remaining.trim().is_empty());
    let mut context = intrinsic_context();
    let err = interpret_expression(expr, &mut context).expect_err("expected non-function call");
    let rendered = err.render_with_source(source);
    assert!(rendered.contains("Attempted to call a non-function value"));
    assert!(rendered.contains("line 1, column 1"));
}

#[test]
fn interpret_basic_enum_flow() {
    let program = "
    IntOption := enum { Some = i32, None = {} };
    (export wasm) pick_positive := (x: i32) => (
        opt := if x > 0 then IntOption::Some(x) else IntOption::None;

        if IntOption::Some(value) := opt then value else 0
    );
    pick_positive(3)
    ";

    let (ast, remaining) = crate::parsing::parse_block(program).unwrap();
    println!("{}", ast.pretty_print());
    assert!(remaining.trim().is_empty());
    let mut context = intrinsic_context();
    if let ExpressionKind::Block(exprs) = ast.kind {
        let mut last = None;
        for expr in exprs {
            last = Some(interpret_expression(expr, &mut context).unwrap());
        }
        match last.unwrap().kind {
            ExpressionKind::Literal(ExpressionLiteral::Number(v)) => assert_eq!(v, 3),
            other => panic!("unexpected result {other:?}"),
        }
    } else {
        panic!("expected block");
    }
}

#[test]
fn enum_intrinsic_exposes_function_type() {
    let mut context = intrinsic_context();
    let enum_binding = context
        .get_identifier(&Identifier::new("enum"))
        .expect("enum intrinsic should be present")
        .clone();

    match &enum_binding.0 {
        BindingContext::Bound(expr, _) => {
            match get_type_of_expression(expr, &mut context).unwrap().kind {
                ExpressionKind::FunctionType {
                    parameter,
                    return_type,
                } => {
                    assert!(parameter == return_type)
                }
                other => panic!("unexpected enum intrinsic type {other:?}"),
            }
        }
        other => panic!("unexpected enum intrinsic binding {other:?}"),
    }
}

#[test]
fn enum_intrinsic_can_be_aliased() {
    let program = "
    Alias := enum;
    IntOption := Alias { Some = i32, None = {} };
    IntOption::None
    ";

    let (value, _context) = evaluate_text_to_expression(program).unwrap();
    match value.kind {
        ExpressionKind::EnumValue { variant, .. } => assert_eq!(variant.name, "None"),
        other => panic!("unexpected alias result {other:?}"),
    }
}

#[test]
fn enum_pattern_rejects_unknown_type() {
    let program = "
    Opt := enum { Some = i32, None = {} };
    value := Opt::Some(1);
    if Missing::Some(v) := value then v else 0
    ";

    let error = match evaluate_text_to_expression(program) {
        Ok(_) => panic!("Expected error for unknown enum"),
        Err(err) => err,
    };
    if !error.message.contains("Unbound identifier: Missing") {
        panic!("unexpected error message: {}", error.message);
    }
}

#[test]
fn enum_pattern_requires_matching_type() {
    let program = "
    First := enum { Some = i32, None = {} };
    Second := enum { Some = {}, None = {} };
    check := {} => (
        value := First::Some(5);
        if Second::Some := value then 1 else 0
    );
    check{}
    ";

    let (value, _context) = evaluate_text_to_expression(program)
        .unwrap_or_else(|e| panic!("{}", e.render_with_source(program)));
    match value.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(result)) => assert_eq!(result, 0),
        other => panic!("unexpected result {other:?}"),
    }
}

#[test]
fn enum_rejects_value_payloads() {
    let program = "
    Bad := enum { Value = 1 };
    {};
    ";

    let (ast, remaining) = crate::parsing::parse_block(program).unwrap();
    assert!(remaining.trim().is_empty());
    let mut context = intrinsic_context();
    let result = interpret_program(ast, &mut context);
    assert!(result.is_err(), "expected enum construction to fail");
    let error = result.err().unwrap();
    assert!(
        error
            .message
            .contains("Enum variant payload must be a type"),
        "unexpected error: {}",
        error.message
    );
}

fn interpret_enum_from_struct(
    argument_value: Expression,
    span: SourceSpan,
    context: &mut Context,
) -> Result<Expression, Diagnostic> {
    let ExpressionKind::Struct(variants) = &argument_value.kind else {
        return Err(diagnostic(
            format!(
                "enum expects a struct of variants, got {:?}",
                argument_value
            ),
            span,
        ));
    };

    let mut evaluated_variants = Vec::with_capacity(variants.len());
    for (variant_name, variant_type) in variants {
        let evaluated_type = interpret_expression(variant_type.clone(), context)?;
        if !is_type_expression(&evaluated_type.kind) {
            return Err(diagnostic(
                "Enum variant payload must be a type",
                variant_type.span(),
            ));
        }
        evaluated_variants.push((variant_name.clone(), evaluated_type));
    }

    Ok(ExpressionKind::EnumType(evaluated_variants).with_span(span))
}
