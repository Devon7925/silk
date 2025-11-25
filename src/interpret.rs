use crate::{
    diagnostics::{Diagnostic, SourceSpan},
    enum_normalization::normalize_enum_application,
    parsing::{
        BinaryIntrinsicOperator, Binding, BindingAnnotation, BindingPattern, Expression,
        ExpressionLiteral, Identifier, IntrinsicOperation, IntrinsicType, LValue, TargetLiteral,
    },
};
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PreserveBehavior {
    PreserveUsage,
    PreserveBinding,
    Inline,
}

impl PartialOrd for PreserveBehavior {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use PreserveBehavior::*;
        Some(match (self, other) {
            (PreserveUsage, PreserveUsage) => std::cmp::Ordering::Equal,
            (PreserveUsage, _) => std::cmp::Ordering::Greater,
            (_, PreserveUsage) => std::cmp::Ordering::Less,
            (PreserveBinding, PreserveBinding) => std::cmp::Ordering::Equal,
            (PreserveBinding, Inline) => std::cmp::Ordering::Greater,
            (Inline, PreserveBinding) => std::cmp::Ordering::Less,
            (Inline, Inline) => std::cmp::Ordering::Equal,
        })
    }
}

impl Ord for PreserveBehavior {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use PreserveBehavior::*;
        match (self, other) {
            (PreserveUsage, PreserveUsage) => std::cmp::Ordering::Equal,
            (PreserveUsage, _) => std::cmp::Ordering::Greater,
            (_, PreserveUsage) => std::cmp::Ordering::Less,
            (PreserveBinding, PreserveBinding) => std::cmp::Ordering::Equal,
            (PreserveBinding, Inline) => std::cmp::Ordering::Greater,
            (Inline, PreserveBinding) => std::cmp::Ordering::Less,
            (Inline, Inline) => std::cmp::Ordering::Equal,
        }
    }
}

#[derive(Clone, Debug)]
pub enum BindingContext {
    Bound(Expression, PreserveBehavior),
    UnboundWithType(Expression),
    UnboundWithoutType,
}

#[derive(Clone)]
pub struct Context {
    pub bindings: std::collections::HashMap<String, (BindingContext, Vec<BindingAnnotation>)>,
}

#[derive(Clone, Debug)]
pub struct AnnotatedBinding {
    pub name: String,
    pub annotations: Vec<BindingAnnotation>,
    pub value: Expression,
}

impl Context {
    pub fn annotated_bindings(&self) -> Vec<AnnotatedBinding> {
        self.bindings
            .iter()
            .filter(|(_, (_, annotations))| !annotations.is_empty())
            .filter_map(|(name, (binding, annotations))| match binding {
                BindingContext::Bound(value, _) => Some(AnnotatedBinding {
                    name: name.clone(),
                    annotations: annotations.clone(),
                    value: value.clone(),
                }),
                _ => None,
            })
            .collect()
    }

    fn empty() -> Context {
        Context {
            bindings: std::collections::HashMap::new(),
        }
    }
}

fn diagnostic(message: impl Into<String>, span: SourceSpan) -> Diagnostic {
    Diagnostic::new(message).with_span(span)
}

fn dummy_span() -> SourceSpan {
    SourceSpan::default()
}

fn empty_struct_expr(span: SourceSpan) -> Expression {
    Expression::Struct(vec![], span)
}

fn identifier_expr(name: &str) -> Expression {
    Expression::Identifier(Identifier(name.to_string()), dummy_span())
}

fn intrinsic_type_expr(ty: IntrinsicType) -> Expression {
    Expression::IntrinsicType(ty, dummy_span())
}

fn types_equivalent(left: &Expression, right: &Expression) -> bool {
    match (left, right) {
        (Expression::IntrinsicType(a, _), Expression::IntrinsicType(b, _)) => a == b,
        (Expression::Identifier(a, _), Expression::Identifier(b, _)) => a.0 == b.0,
        (Expression::Struct(a_items, _), Expression::Struct(b_items, _)) => {
            if a_items.len() != b_items.len() {
                return false;
            }
            a_items
                .iter()
                .zip(b_items.iter())
                .all(|((aid, aexpr), (bid, bexpr))| {
                    aid.0 == bid.0 && types_equivalent(aexpr, bexpr)
                })
        }
        (
            Expression::FunctionType {
                parameter: a_param,
                return_type: a_ret,
                ..
            },
            Expression::FunctionType {
                parameter: b_param,
                return_type: b_ret,
                ..
            },
        ) => types_equivalent(a_param, b_param) && types_equivalent(a_ret, b_ret),
        (
            Expression::AttachImplementation {
                type_expr: a_type, ..
            },
            Expression::AttachImplementation {
                type_expr: b_type, ..
            },
        ) => types_equivalent(a_type, b_type),
        (Expression::EnumType(a_variants, _), Expression::EnumType(b_variants, _)) => {
            if a_variants.len() != b_variants.len() {
                return false;
            }
            a_variants
                .iter()
                .zip(b_variants.iter())
                .all(|((a_id, a_ty), (b_id, b_ty))| {
                    a_id.0 == b_id.0 && types_equivalent(a_ty, b_ty)
                })
        }
        _ => false,
    }
}

#[cfg(test)]
fn literal_number_expr(value: i32) -> Expression {
    Expression::Literal(ExpressionLiteral::Number(value), dummy_span())
}

fn is_type_expression(expr: &Expression) -> bool {
    match expr {
        Expression::IntrinsicType(_, _) => true,
        Expression::AttachImplementation { .. } => true,
        Expression::EnumType(_, _) => true,
        Expression::FunctionType { .. } => true,
        Expression::Struct(items, _) => items.iter().all(|(_, ty)| is_type_expression(ty)),
        Expression::Identifier(_, _) => true,
        _ => false,
    }
}

fn enum_variant_info(enum_type: &Expression, variant: &Identifier) -> Option<(usize, Expression)> {
    if let Expression::EnumType(variants, _) = enum_type {
        variants
            .iter()
            .enumerate()
            .find(|(_, (id, _))| id.0 == variant.0)
            .map(|(idx, (_, ty))| (idx, ty.clone()))
    } else {
        None
    }
}

pub fn resolve_enum_type_expression(
    enum_expr: &Expression,
    context: &mut Context,
) -> Option<Expression> {
    let interpreted = interpret_expression(enum_expr.clone(), context).ok()?;
    resolve_expression(interpreted, context).ok()
}

fn resolve_operations(expr: Expression, context: &mut Context) -> Result<Expression, Diagnostic> {
    match expr {
        Expression::Operation {
            operator,
            left,
            right,
            span,
        } => {
            let mut type_context = context.clone();
            let possible_binary_op = get_type_of_expression(&left, &mut type_context)
                .and_then(|left_type| resolve_expression(left_type, &mut type_context))
                .and_then(|resolved_left_type| {
                    get_trait_prop_of_type(&resolved_left_type, &operator, span)
                })
                .map(|op_impl| {
                    let Expression::Function {
                        body: outer_body, ..
                    } = op_impl
                    else {
                        return None;
                    };
                    let Expression::Function {
                        body: inner_body, ..
                    } = *outer_body
                    else {
                        return None;
                    };

                    if let Expression::IntrinsicOperation(
                        IntrinsicOperation::Binary(_, _, binary_op),
                        _,
                    ) = *inner_body
                    {
                        Some(binary_op)
                    } else {
                        None
                    }
                })
                .unwrap_or(None);

            if let Some(binary_op) = possible_binary_op {
                let resolved_left = resolve_operations(*left, context)?;
                let resolved_right = resolve_operations(*right, context)?;
                Ok(Expression::IntrinsicOperation(
                    IntrinsicOperation::Binary(
                        Box::new(resolved_left),
                        Box::new(resolved_right),
                        binary_op,
                    ),
                    span,
                ))
            } else {
                Ok(Expression::Operation {
                    operator,
                    left,
                    right,
                    span,
                })
            }
        }
        Expression::IntrinsicOperation(
            IntrinsicOperation::Binary(left, right, BinaryIntrinsicOperator::BooleanAnd),
            span,
        ) => {
            let resolved_left = resolve_operations(*left, context)?;
            let resolved_right = resolve_operations(*right, context)?;
            Ok(Expression::IntrinsicOperation(
                IntrinsicOperation::Binary(
                    Box::new(resolved_left),
                    Box::new(resolved_right),
                    BinaryIntrinsicOperator::BooleanAnd,
                ),
                span,
            ))
        }
        _ => Ok(expr),
    }
}

fn collect_bindings(expr: &Expression, context: &mut Context) -> Result<(), Diagnostic> {
    match expr {
        Expression::Binding(binding, _) => {
            let mut type_context = context.clone();
            let value_type = get_type_of_expression(&binding.expr, &mut type_context).ok();
            let resolved_type = if let Some(ty) = value_type {
                resolve_expression(ty, context).ok()
            } else {
                None
            };
            bind_pattern_blanks(binding.pattern.clone(), context, Vec::new(), resolved_type)?;
        }
        Expression::IntrinsicOperation(
            IntrinsicOperation::Binary(left, right, BinaryIntrinsicOperator::BooleanAnd),
            _,
        ) => {
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
    let expr = normalize_enum_application(expr);

    let result = match expr {
        Expression::EnumType(variants, span) => {
            let mut evaluated_variants = Vec::with_capacity(variants.len());
            for (id, ty_expr) in variants {
                let evaluated_ty = interpret_expression(ty_expr, context)?;
                evaluated_variants.push((id, evaluated_ty));
            }
            Ok(Expression::EnumType(evaluated_variants, span))
        }
        expr @ (Expression::Literal(_, _) | Expression::IntrinsicType(_, _)) => Ok(expr),
        Expression::If {
            condition,
            then_branch,
            else_branch,
            span,
        } => {
            let base_context = context.clone();
            let condition_expr = *condition;
            let condition_for_pattern = condition_expr.clone();
            let interpreted_condition = interpret_expression(condition_expr, context)?;

            let inferred_else_expr = else_branch
                .clone()
                .unwrap_or_else(|| Box::new(empty_struct_expr(SourceSpan::new(span.end(), 0))));

            let mut then_context = base_context.clone();
            let resolved_condition = resolve_operations(condition_for_pattern, context)?;
            collect_bindings(&resolved_condition, &mut then_context)?;

            let (interpreted_then, then_type, then_returns) =
                branch_type(&then_branch, &then_context)?;
            let (interpreted_else, else_type, else_returns) =
                branch_type(&inferred_else_expr, &base_context)?;

            if !types_equivalent(&then_type, &else_type) {
                if then_returns && !else_returns {
                    // The then branch exits, so the resulting type is determined by the else branch.
                } else if else_returns && !then_returns {
                    // The else branch exits, so the resulting type is determined by the then branch.
                } else {
                    return Err(diagnostic("Type mismatch between if branches", span));
                }
            }

            let resolved_condition = resolve_expression(interpreted_condition.clone(), context);
            if let Ok(Expression::Literal(ExpressionLiteral::Boolean(condition_value), _)) =
                resolved_condition
            {
                if condition_value {
                    interpret_expression(*then_branch, context)
                } else {
                    interpret_expression(*inferred_else_expr, context)
                }
            } else {
                Ok(Expression::If {
                    condition: Box::new(interpreted_condition),
                    then_branch: Box::new(interpreted_then),
                    else_branch: Some(Box::new(interpreted_else)),
                    span,
                })
            }
        }
        Expression::Identifier(identifier, span) => {
            if let Some((binding_ctx, _)) = context.bindings.get(&identifier.0) {
                match binding_ctx.clone() {
                    BindingContext::Bound(expr, preserve_behavior) => {
                        let resolved = resolve_value(expr, context)?;
                        if matches!(&resolved, Expression::Identifier(id, _) if id.0 == identifier.0)
                            && matches!(preserve_behavior, PreserveBehavior::PreserveUsage)
                        {
                            return Ok(Expression::Identifier(identifier, span));
                        }
                        return Ok(resolved);
                    }
                    BindingContext::UnboundWithType(_) | BindingContext::UnboundWithoutType => {
                        Ok(Expression::Identifier(identifier, span))
                    }
                }
            } else {
                Err(diagnostic(
                    format!("Unbound identifier: {}", identifier.0),
                    span,
                ))
            }
        }
        Expression::Operation {
            operator,
            left,
            right,
            span,
        } => interpret_expression(
            Expression::FunctionCall {
                function: Box::new(Expression::PropertyAccess {
                    object: left,
                    property: operator.clone(),
                    span,
                }),
                argument: right,
                span,
            },
            context,
        ),
        Expression::Binding(binding, _) => {
            interpret_binding(*binding, context, None).map(|(binding_result, _)| binding_result)
        }
        Expression::Return { value, span } => {
            let evaluated_value = match value {
                Some(expr) => interpret_expression(*expr, context)?,
                None => empty_struct_expr(span),
            };

            let resolved_value = resolve_value(evaluated_value, context)?;

            Ok(Expression::Return {
                value: Some(Box::new(resolved_value)),
                span,
            })
        }
        Expression::Loop { body, span } => {
            let mut iteration_count = 0usize;
            loop {
                if iteration_count > 10_000 {
                    return Err(diagnostic("Loop did not produce a return value", span));
                }

                iteration_count += 1;
                let iteration_value = match *body.clone() {
                    Expression::Block(exprs, block_span) => {
                        interpret_loop_block(exprs, block_span, context)?
                    }
                    other => interpret_expression(other, context)?,
                };
                if matches!(iteration_value, Expression::Return { .. }) {
                    return Ok(iteration_value);
                }
            }
        }
        Expression::Assignment { target, expr, span } => {
            let value = interpret_expression(*expr, context)?;
            apply_assignment(target, value, span, context)
        }
        Expression::Block(expressions, span) => {
            let (value, _) = interpret_block(expressions, span, context)?;
            Ok(value)
        }
        Expression::FunctionCall {
            function,
            argument,
            span,
        } => {
            let function_value = interpret_expression(*function, context)?;
            let argument_value = interpret_expression(*argument, context)?;

            let effective_function = if let Expression::Identifier(ident, _) = &function_value {
                context.bindings.get(&ident.0).and_then(|b| match &b.0 {
                    BindingContext::Bound(v, _) => Some(v.clone()),
                    _ => None,
                })
            } else {
                Some(function_value.clone())
            };

            if let Some(Expression::IntrinsicOperation(IntrinsicOperation::EnumFromStruct, _)) =
                &effective_function
            {
                return interpret_enum_from_struct(argument_value, span, context);
            }

            if let Some(Expression::Function {
                parameter,
                return_type,
                body,
                span: _,
            }) = effective_function
            {
                let is_direct = matches!(function_value, Expression::Function { .. });
                let returns_type = is_type_expression(&return_type);

                if is_direct || returns_type {
                    let mut call_context = context.clone();
                    let parameter_clone = parameter.clone();
                    bind_pattern_from_value(
                        parameter,
                        &argument_value,
                        &mut call_context,
                        Vec::new(),
                        PreserveBehavior::Inline,
                        None,
                    )?;
                    force_bindings(&parameter_clone, &argument_value, &mut call_context);
                    if let Some(identifier) = parameter_identifier(&parameter_clone) {
                        call_context.bindings.insert(
                            identifier.0.clone(),
                            (
                                BindingContext::Bound(
                                    argument_value.clone(),
                                    PreserveBehavior::Inline,
                                ),
                                Vec::new(),
                            ),
                        );
                    }
                    let body_value = interpret_expression(*body, &mut call_context)?;
                    if let Expression::Return { value, .. } = body_value {
                        return Ok(*value.expect("Return value should be populated"));
                    }
                    return Ok(body_value);
                }
            }

            if let Expression::EnumConstructor {
                enum_type,
                variant,
                variant_index,
                payload_type,
                span: constructor_span,
            } = function_value.clone()
            {
                let payload_type_resolved = resolve_expression(*payload_type, context)?;
                let argument_type = get_type_of_expression(&argument_value, context)?;
                if !types_equivalent(&payload_type_resolved, &argument_type) {
                    return Err(diagnostic("Enum variant payload type mismatch", span));
                }
                return Ok(Expression::EnumValue {
                    enum_type,
                    variant,
                    variant_index,
                    payload: Some(Box::new(argument_value)),
                    span: span.merge(&constructor_span),
                });
            }

            match function_value {
                Expression::Function { .. } => unreachable!(), // Handled above
                other => {
                    if is_resolved_constant(&other) {
                        Err(diagnostic("Attempted to call a non-function value", span))
                    } else {
                        Ok(Expression::FunctionCall {
                            function: Box::new(other),
                            argument: Box::new(argument_value),
                            span,
                        })
                    }
                }
            }
        }
        Expression::AttachImplementation {
            type_expr,
            implementation,
            span,
        } => Ok(Expression::AttachImplementation {
            type_expr: Box::new(interpret_expression(*type_expr, context)?),
            implementation: Box::new(interpret_expression(*implementation, context)?),
            span,
        }),
        Expression::Function {
            parameter,
            return_type,
            body,
            span,
        } => {
            let mut body_context = context.clone();

            bind_pattern_blanks(parameter.clone(), &mut body_context, Vec::new(), None)?;

            Ok(Expression::Function {
                parameter: interpret_binding_pattern(parameter, context)?,
                return_type: Box::new(interpret_expression(*return_type, &mut body_context)?),
                body: Box::new(interpret_expression(*body, &mut body_context)?),
                span,
            })
        }
        Expression::Struct(items, span) => {
            let mut evaluated_items = Vec::with_capacity(items.len());
            for (identifier, value_expr) in items {
                let evaluated_value = interpret_expression(value_expr, context)?;
                evaluated_items.push((identifier, evaluated_value));
            }
            Ok(Expression::Struct(evaluated_items, span))
        }
        Expression::FunctionType {
            parameter,
            return_type,
            span,
        } => Ok(Expression::FunctionType {
            parameter,
            return_type,
            span,
        }),
        Expression::EnumAccess {
            enum_expr,
            variant,
            span,
        } => {
            let interpreted_enum = interpret_expression(*enum_expr, context)?;
            let resolved_enum = resolve_expression(interpreted_enum.clone(), context)?;
            if let Some((variant_index, payload_type)) = enum_variant_info(&resolved_enum, &variant)
            {
                if let Expression::Struct(fields, _payload_span) = &payload_type {
                    if fields.is_empty() {
                        return Ok(Expression::EnumValue {
                            enum_type: Box::new(resolved_enum),
                            variant,
                            variant_index,
                            payload: None,
                            span,
                        });
                    }
                }

                Ok(Expression::EnumConstructor {
                    enum_type: Box::new(resolved_enum),
                    variant,
                    variant_index,
                    payload_type: Box::new(payload_type),
                    span,
                })
            } else {
                Ok(Expression::EnumAccess {
                    enum_expr: Box::new(interpreted_enum),
                    variant,
                    span,
                })
            }
        }
        Expression::EnumValue {
            enum_type,
            variant,
            variant_index,
            payload,
            span,
        } => Ok(Expression::EnumValue {
            enum_type: Box::new(interpret_expression(*enum_type, context)?),
            variant,
            variant_index,
            payload: match payload {
                Some(value) => Some(Box::new(interpret_expression(*value, context)?)),
                None => None,
            },
            span,
        }),
        Expression::EnumConstructor {
            enum_type,
            variant,
            variant_index,
            payload_type,
            span,
        } => Ok(Expression::EnumConstructor {
            enum_type: Box::new(interpret_expression(*enum_type, context)?),
            variant,
            variant_index,
            payload_type: Box::new(interpret_expression(*payload_type, context)?),
            span,
        }),
        Expression::IntrinsicOperation(intrinsic_operation, span) => match intrinsic_operation {
            IntrinsicOperation::Binary(left, right, operator) => {
                let evaluated_left = resolve_value(interpret_expression(*left, context)?, context)?;
                let evaluated_right =
                    resolve_value(interpret_expression(*right, context)?, context)?;
                if !is_resolved_constant(&evaluated_left)
                    || !is_resolved_constant(&evaluated_right)
                {
                    return Ok(Expression::IntrinsicOperation(
                        IntrinsicOperation::Binary(
                            Box::new(evaluated_left),
                            Box::new(evaluated_right),
                            operator,
                        ),
                        span,
                    ));
                }
                match operator {
                    BinaryIntrinsicOperator::I32Add
                    | BinaryIntrinsicOperator::I32Subtract
                    | BinaryIntrinsicOperator::I32Multiply => interpret_numeric_intrinsic(
                        evaluated_left,
                        evaluated_right,
                        context,
                        span,
                        match operator {
                            BinaryIntrinsicOperator::I32Add => |l, r| l + r,
                            BinaryIntrinsicOperator::I32Subtract => |l, r| l - r,
                            BinaryIntrinsicOperator::I32Multiply => |l, r| l * r,
                            _ => unreachable!(),
                        },
                    ),
                    BinaryIntrinsicOperator::I32Divide => {
                        interpret_divide_intrinsic(evaluated_left, evaluated_right, context, span)
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
                            context,
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
                        )
                    }
                    BinaryIntrinsicOperator::BooleanAnd
                    | BinaryIntrinsicOperator::BooleanOr
                    | BinaryIntrinsicOperator::BooleanXor => interpret_boolean_intrinsic(
                        evaluated_left,
                        evaluated_right,
                        context,
                        span,
                        match operator {
                            BinaryIntrinsicOperator::BooleanAnd => |l, r| l && r,
                            BinaryIntrinsicOperator::BooleanOr => |l, r| l || r,
                            BinaryIntrinsicOperator::BooleanXor => |l, r| l ^ r,
                            _ => unreachable!(),
                        },
                    ),
                }
            }
            IntrinsicOperation::EnumFromStruct => Ok(Expression::IntrinsicOperation(
                IntrinsicOperation::EnumFromStruct,
                span,
            )),
        },
        Expression::PropertyAccess {
            object,
            property,
            span,
        } => {
            let evaluated_object = interpret_expression(*object, context)?;
            let resolved_object = resolve_value(evaluated_object, context)?;
            if let Expression::Struct(items, _) = &resolved_object {
                for (item_id, item_expr) in items {
                    if item_id.0 == property {
                        return Ok(item_expr.clone());
                    }
                }
            }

            let object_type = get_type_of_expression(&resolved_object, context)?;
            let resolved_object_type = resolve_value(object_type, context)?;
            let trait_prop = get_trait_prop_of_type(&resolved_object_type, &property, span)?;
            match trait_prop {
                Expression::Function { .. } => interpret_expression(
                    Expression::FunctionCall {
                        function: Box::new(trait_prop),
                        argument: Box::new(resolved_object),
                        span,
                    },
                    context,
                ),
                _other => Ok(Expression::PropertyAccess {
                    object: Box::new(resolved_object),
                    property,
                    span,
                }),
            }
        }
    };
    result
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

fn get_type_of_expression(
    expr: &Expression,
    context: &mut Context,
) -> Result<Expression, Diagnostic> {
    match expr {
        Expression::Literal(lit, _) => match lit {
            ExpressionLiteral::Number(_) => interpret_expression(identifier_expr("i32"), context),
            ExpressionLiteral::Boolean(_) => interpret_expression(identifier_expr("bool"), context),
            ExpressionLiteral::Target(_) => {
                interpret_expression(identifier_expr("target"), context)
            }
        },
        Expression::Identifier(identifier, span) => {
            let bound_value = context
                .bindings
                .get(&identifier.0)
                .ok_or_else(|| diagnostic(format!("Unbound identifier: {}", identifier.0), *span))?
                .clone();

            match bound_value.0 {
                BindingContext::Bound(value, _) => get_type_of_expression(&value, context),
                BindingContext::UnboundWithType(type_expr) => {
                    interpret_expression(type_expr.clone(), context)
                }
                BindingContext::UnboundWithoutType => Err(diagnostic(
                    format!(
                        "Cannot determine type of unbound identifier: {}",
                        identifier.0
                    ),
                    *span,
                )),
            }
        }
        Expression::Assignment { target, expr, span } => {
            let mut type_context = context.clone();
            let value_type = get_type_of_expression(expr, &mut type_context)?;
            let target_type = get_lvalue_type(target, context, *span)?;

            if !types_equivalent(&target_type, &value_type) {
                return Err(diagnostic(
                    format!(
                        "Cannot assign value of mismatched type to {}",
                        lvalue_display_name(target)
                    ),
                    *span,
                ));
            }

            Ok(target_type)
        }
        Expression::Return { value, span } => {
            let inner_value = value
                .as_ref()
                .map(|expr| expr.as_ref().clone())
                .unwrap_or_else(|| empty_struct_expr(*span));
            get_type_of_expression(&inner_value, context)
        }
        Expression::If {
            then_branch,
            else_branch,
            span,
            ..
        } => {
            let inferred_else = else_branch
                .as_ref()
                .map(|expr| expr.as_ref().clone())
                .unwrap_or_else(|| empty_struct_expr(SourceSpan::new(span.end(), 0)));
            let mut then_context = context.clone();
            let then_type = get_type_of_expression(then_branch, &mut then_context)?;
            let mut else_context = context.clone();
            let else_type = get_type_of_expression(&inferred_else, &mut else_context)?;
            if !types_equivalent(&then_type, &else_type) {
                let then_returns = expression_contains_return(then_branch);
                let else_returns = expression_contains_return(&inferred_else);

                if then_returns && !else_returns {
                    return Ok(else_type);
                } else if else_returns && !then_returns {
                    return Ok(then_type);
                } else {
                    return Err(diagnostic("Type mismatch between if branches", *span));
                }
            }
            Ok(then_type)
        }
        Expression::Binding(binding, _) => {
            let mut binding_context = context.clone();
            let value_type = get_type_of_expression(&binding.expr, &mut binding_context)?;
            bind_pattern_blanks(
                binding.pattern.clone(),
                &mut binding_context,
                Vec::new(),
                Some(value_type),
            )?;
            interpret_expression(identifier_expr("bool"), context)
        }
        Expression::EnumAccess {
            enum_expr,
            variant,
            span,
        } => {
            let enum_type = get_type_of_expression(enum_expr, context)?;
            if let Some((_, payload_type)) = enum_variant_info(&enum_type, variant) {
                if let Expression::Struct(fields, _) = &payload_type {
                    if fields.is_empty() {
                        return Ok(enum_type);
                    }
                }
                Ok(Expression::FunctionType {
                    parameter: Box::new(payload_type),
                    return_type: Box::new(enum_type),
                    span: *span,
                })
            } else {
                Err(diagnostic("Unknown enum variant", *span))
            }
        }
        Expression::EnumConstructor {
            enum_type,
            payload_type,
            span,
            ..
        } => Ok(Expression::FunctionType {
            parameter: payload_type.clone(),
            return_type: enum_type.clone(),
            span: *span,
        }),
        Expression::EnumValue { enum_type, .. } => Ok(*enum_type.clone()),
        Expression::EnumType(_, span) => Ok(Expression::IntrinsicType(IntrinsicType::Type, *span)),
        Expression::Operation {
            operator,
            left,
            right,
            span,
        } => get_type_of_expression(
            &Expression::FunctionCall {
                function: Box::new(Expression::PropertyAccess {
                    object: left.clone(),
                    property: operator.clone(),
                    span: *span,
                }),
                argument: right.clone(),
                span: *span,
            },
            context,
        ),
        Expression::Block(exprs, span) => {
            let mut block_context = context.clone();
            let (value, _) = interpret_block(exprs.clone(), *span, &mut block_context)?;
            get_type_of_expression(&value, &mut block_context)
        }
        Expression::Loop { body, .. } => {
            let mut loop_context = context.clone();
            get_type_of_expression(body, &mut loop_context)
        }
        Expression::FunctionCall {
            function,
            argument,
            span,
        } => {
            if let Expression::EnumAccess {
                enum_expr,
                variant,
                span,
            } = function.as_ref()
            {
                let enum_type = interpret_expression(enum_expr.as_ref().clone(), context)?;
                if let Some((_, payload_type)) = enum_variant_info(&enum_type, variant) {
                    let argument_type = get_type_of_expression(argument, context)?;
                    if !types_equivalent(&payload_type, &argument_type) {
                        return Err(diagnostic("Enum variant payload type mismatch", *span));
                    }
                    return Ok(enum_type);
                } else {
                    return Err(diagnostic("Unknown enum variant", *span));
                }
            }
            let mut call_context = context.clone();
            let evaluated_call = interpret_expression(
                Expression::FunctionCall {
                    function: function.clone(),
                    argument: argument.clone(),
                    span: *span,
                },
                &mut call_context,
            )?;
            get_type_of_expression(&evaluated_call, &mut call_context)
        }
        Expression::PropertyAccess {
            object,
            property,
            span,
        } => {
            let object_type = get_type_of_expression(object, context)?;
            let resolved_object_type = resolve_expression(object_type, context)?;
            get_trait_prop_of_type(&resolved_object_type, property, *span)
        }
        Expression::IntrinsicType(intrinsic_type, span) => match intrinsic_type {
            IntrinsicType::I32
            | IntrinsicType::Boolean
            | IntrinsicType::Target
            | IntrinsicType::Type => Ok(Expression::IntrinsicType(IntrinsicType::Type, *span)),
        },
        Expression::AttachImplementation { type_expr, .. } => {
            get_type_of_expression(type_expr, context)
        }
        Expression::Function {
            parameter,
            return_type,
            span,
            ..
        } => Ok(Expression::FunctionType {
            parameter: Box::new(get_type_of_binding_pattern(&parameter, context)?),
            return_type: return_type.clone(),
            span: *span,
        }),
        Expression::Struct(items, span) => {
            let mut struct_items = Vec::with_capacity(items.len());
            for (field_id, field_expr) in items {
                let field_type = get_type_of_expression(field_expr, context)?;
                struct_items.push((field_id.clone(), field_type));
            }
            Ok(Expression::Struct(struct_items, *span))
        }
        Expression::FunctionType { span, .. } => {
            Ok(Expression::IntrinsicType(IntrinsicType::Type, *span))
        }
        Expression::IntrinsicOperation(
            IntrinsicOperation::Binary(
                _,
                _,
                BinaryIntrinsicOperator::I32Add
                | BinaryIntrinsicOperator::I32Subtract
                | BinaryIntrinsicOperator::I32Multiply
                | BinaryIntrinsicOperator::I32Divide,
            ),
            _,
        ) => interpret_expression(identifier_expr("i32"), context),
        Expression::IntrinsicOperation(
            IntrinsicOperation::Binary(
                _,
                _,
                BinaryIntrinsicOperator::I32Equal
                | BinaryIntrinsicOperator::I32NotEqual
                | BinaryIntrinsicOperator::I32LessThan
                | BinaryIntrinsicOperator::I32GreaterThan
                | BinaryIntrinsicOperator::I32LessThanOrEqual
                | BinaryIntrinsicOperator::I32GreaterThanOrEqual,
            ),
            _,
        ) => interpret_expression(identifier_expr("bool"), context),
        Expression::IntrinsicOperation(
            IntrinsicOperation::Binary(
                _,
                _,
                BinaryIntrinsicOperator::BooleanAnd
                | BinaryIntrinsicOperator::BooleanOr
                | BinaryIntrinsicOperator::BooleanXor,
            ),
            _,
        ) => interpret_expression(identifier_expr("bool"), context),
        Expression::IntrinsicOperation(IntrinsicOperation::EnumFromStruct, span) => {
            Ok(Expression::FunctionType {
                parameter: Box::new(intrinsic_type_expr(IntrinsicType::Type)),
                return_type: Box::new(intrinsic_type_expr(IntrinsicType::Type)),
                span: *span,
            })
        }
    }
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
        BindingPattern::Literal(lit, span) => {
            get_type_of_expression(&Expression::Literal(lit.clone(), *span), context)
        }
        BindingPattern::Struct(pattern_items, span) => {
            let mut struct_items = Vec::with_capacity(pattern_items.len());
            for (field_identifier, field_pattern) in pattern_items {
                let field_type = get_type_of_binding_pattern(field_pattern, context)?;
                struct_items.push((field_identifier.clone(), field_type));
            }
            Ok(Expression::Struct(struct_items, *span))
        }
        BindingPattern::EnumVariant { enum_type, .. } => {
            interpret_expression(*enum_type.clone(), context)
        }
        BindingPattern::TypeHint(_, type_expr, _) => Ok(*type_expr.clone()),
        BindingPattern::Annotated { pattern, .. } => get_type_of_binding_pattern(pattern, context),
    }
}

fn expression_contains_return(expr: &Expression) -> bool {
    match expr {
        Expression::Return { .. } => true,
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
        Expression::FunctionCall { function, argument, .. } => {
            expression_contains_return(function) || expression_contains_return(argument)
        }
        Expression::Function { body, .. } => expression_contains_return(body),
        Expression::Loop { body, .. } => expression_contains_return(body),
        Expression::PropertyAccess { object, .. } => expression_contains_return(object),
        Expression::Operation { left, right, .. } => {
            expression_contains_return(left) || expression_contains_return(right)
        }
        Expression::AttachImplementation { type_expr, implementation, .. } => {
            expression_contains_return(type_expr) || expression_contains_return(implementation)
        }
        Expression::EnumAccess { enum_expr, .. } => expression_contains_return(enum_expr),
        Expression::EnumValue { enum_type, payload, .. } => {
            expression_contains_return(enum_type)
                || payload
                    .as_ref()
                    .map(|payload| expression_contains_return(payload))
                    .unwrap_or(false)
        }
        Expression::EnumConstructor {
            enum_type,
            payload_type,
            ..
        } => {
            expression_contains_return(enum_type)
                || expression_contains_return(payload_type)
        }
        Expression::IntrinsicOperation(IntrinsicOperation::Binary(left, right, _), _span) => {
            expression_contains_return(left) || expression_contains_return(right)
        }
        Expression::IntrinsicOperation(IntrinsicOperation::EnumFromStruct, _) => false,
        Expression::Literal(_, _)
        | Expression::Identifier(_, _)
        | Expression::IntrinsicType(_, _)
        | Expression::EnumType(_, _)
        | Expression::FunctionType { .. }
        | Expression::Struct(_, _) => false,
    }
}

fn branch_type(
    branch: &Expression,
    context: &Context,
) -> Result<(Expression, Expression, bool), Diagnostic> {
    let mut branch_context = context.clone();
    let evaluated_branch = interpret_expression(branch.clone(), &mut branch_context)?;
    let branch_type = get_type_of_expression(&evaluated_branch, &mut branch_context)?;
    Ok((
        evaluated_branch.clone(),
        branch_type,
        matches!(evaluated_branch, Expression::Return { .. }),
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
            .find(|(field_id, _)| field_id.0 == trait_prop)
            .map(|(_, expr)| expr.clone())
    }

    match value_type {
        Expression::Struct(items, _) => get_struct_field(items, trait_prop)
            .ok_or_else(|| diagnostic(format!("Missing field {} on type", trait_prop), span)),
        Expression::AttachImplementation {
            type_expr,
            implementation,
            ..
        } => {
            if let Expression::Struct(ref items, _) = **implementation {
                if let Some(field) = get_struct_field(items, trait_prop) {
                    return Ok(field);
                }
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

fn interpret_block(
    expressions: Vec<Expression>,
    span: SourceSpan,
    context: &mut Context,
) -> Result<(Expression, Context), Diagnostic> {
    let mut block_context = context.clone();
    let mut last_value: Option<Expression> = None;
    let mut preserved_expressions = Vec::new();

    let mut usage_counter = UsageCounter::new();
    for expression in &expressions {
        usage_counter.count(expression);
    }

    for expression in expressions {
        let value = match expression {
            Expression::Binding(binding, span) => {
                let (binding_expr, preserve_behavior) =
                    interpret_binding(*binding, &mut block_context, Some(&usage_counter))?;
                let should_preserve_binding = preserve_behavior != PreserveBehavior::Inline
                    || matches!(
                        &binding_expr,
                        Expression::Binding(binding, _)
                            if pattern_has_mutable_annotation(&binding.pattern)
                    );
                if should_preserve_binding {
                    if let Expression::Binding(binding, _) = binding_expr.clone() {
                        preserved_expressions.push(Expression::Binding(binding, span));
                    }
                }
                Expression::Literal(ExpressionLiteral::Boolean(true), dummy_span())
            }
            Expression::Assignment { target, expr, span } => {
                let interpreted_expr = interpret_expression((*expr).clone(), &mut block_context)?;
                let assignment_expr = Expression::Assignment {
                    target: target.clone(),
                    expr: Box::new(interpreted_expr.clone()),
                    span,
                };
                let val = apply_assignment(target, interpreted_expr, span, &mut block_context)?;
                preserved_expressions.push(assignment_expr);
                val
            }
            other => {
                let val = interpret_expression(other, &mut block_context)?;
                if let Expression::Binding(..) = val {
                    // Should not happen from interpret_expression but just in case
                } else {
                    // If it's not a binding, we don't add it to preserved_expressions
                    // unless it's the last value?
                    // Wait, interpret_block returns (Expression, Context).
                    // If we have statements like `do_something();`, we want to preserve them?
                    // The current interpreter only returns the *last* value.
                    // Intermediate expressions that are not bindings are effectively dropped if they are not used?
                    // But side effects? The interpreter assumes pure expressions mostly, except for bindings?
                    // If we have `print("hello")`, it's an expression.
                    // If we want to preserve it in the output AST, we should add it.
                    // But `interpret_block` signature returns `Expression`.
                    // If we preserve bindings, we are building a Block.
                }
                val
            }
        };
        if matches!(value, Expression::Return { .. }) {
            return Ok((value, block_context));
        }

        last_value = Some(value);
    }

    let final_value = last_value.ok_or_else(|| diagnostic("Cannot evaluate empty block", span))?;

    if preserved_expressions.is_empty() {
        Ok((final_value, block_context))
    } else {
        // If we have preserved bindings, we need to return a Block that contains them
        // AND the final value.
        preserved_expressions.push(final_value);
        Ok((
            Expression::Block(preserved_expressions, span),
            block_context,
        ))
    }
}

fn interpret_loop_block(
    expressions: Vec<Expression>,
    span: SourceSpan,
    context: &mut Context,
) -> Result<Expression, Diagnostic> {
    let mut last_value: Option<Expression> = None;

    for expression in expressions {
        let value = match expression {
            Expression::Binding(binding, _) => {
                interpret_binding(*binding, context, None)?;
                Expression::Literal(ExpressionLiteral::Boolean(true), dummy_span())
            }
            Expression::Assignment { target, expr, span } => {
                let interpreted_expr = interpret_expression(*expr, context)?;
                let resolved_expr = resolve_expression(interpreted_expr, context)?;
                apply_assignment(target, resolved_expr.clone(), span, context)?;
                resolved_expr
            }
            other => resolve_expression(other, context)?,
        };

        if matches!(value, Expression::Return { .. }) {
            return Ok(value);
        }

        last_value = Some(value);
    }

    last_value.ok_or_else(|| diagnostic("Cannot evaluate empty block", span))
}

pub fn interpret_program(
    expr: Expression,
    context: &mut Context,
) -> Result<(Expression, Context), Diagnostic> {
    match expr {
        Expression::Block(expressions, span) => interpret_block(expressions, span, context),
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
            let (_, annotations) = context.bindings.get(&identifier.0).ok_or_else(|| {
                diagnostic(
                    format!("Cannot assign to unbound identifier: {}", identifier.0),
                    *target_span,
                )
            })?;

            if !annotations
                .iter()
                .any(|ann| matches!(ann, BindingAnnotation::Mutable(_)))
            {
                return Err(diagnostic(
                    format!("Cannot assign to immutable identifier: {}", identifier.0),
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
        LValue::Identifier(Identifier(name), _) => name.clone(),
        LValue::PropertyAccess {
            object, property, ..
        } => {
            format!("{}.{}", lvalue_display_name(object), property)
        }
    }
}

fn get_lvalue_type(
    target: &LValue,
    context: &mut Context,
    span: SourceSpan,
) -> Result<Expression, Diagnostic> {
    ensure_lvalue_mutable(target, context, span)?;

    match target {
        LValue::Identifier(identifier, target_span) => {
            let (binding_ctx, _) = context.bindings.get(&identifier.0).ok_or_else(|| {
                diagnostic(
                    format!("Cannot assign to unbound identifier: {}", identifier.0),
                    *target_span,
                )
            })?;

            match binding_ctx {
                BindingContext::Bound(value, _) => {
                    get_type_of_expression(value, &mut context.clone())
                }
                BindingContext::UnboundWithType(type_expr) => Ok(type_expr.clone()),
                BindingContext::UnboundWithoutType => Err(diagnostic(
                    format!("Cannot determine type of {}", identifier.0),
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
            let Expression::Struct(fields, _) = object_type else {
                return Err(diagnostic("Property access on non-struct type", *prop_span));
            };

            let field_type = fields
                .iter()
                .find(|(id, _)| id.0 == *property)
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

fn get_lvalue_value(target: &LValue, context: &mut Context) -> Result<Expression, Diagnostic> {
    match target {
        LValue::Identifier(identifier, target_span) => {
            let (binding_ctx, _) = context.bindings.get(&identifier.0).ok_or_else(|| {
                diagnostic(
                    format!("Cannot assign to unbound identifier: {}", identifier.0),
                    *target_span,
                )
            })?;

            match binding_ctx {
                BindingContext::Bound(value, _) => Ok(value.clone()),
                _ => Err(diagnostic(
                    format!(
                        "Cannot assign to uninitialized identifier: {}",
                        identifier.0
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
            let object_value = get_lvalue_value(object, context)?;
            let Expression::Struct(fields, struct_span) = object_value else {
                return Err(diagnostic(
                    "Property access on non-struct value",
                    *prop_span,
                ));
            };

            fields
                .into_iter()
                .find(|(id, _)| id.0 == *property)
                .map(|(_, expr)| expr)
                .ok_or_else(|| {
                    diagnostic(format!("Missing field {} in struct", property), struct_span)
                })
        }
    }
}

fn apply_lvalue_update(
    target: LValue,
    value: Expression,
    context: &mut Context,
    span: SourceSpan,
) -> Result<(), Diagnostic> {
    match target {
        LValue::Identifier(identifier, _) => {
            let mut type_context = context.clone();
            let value_type = get_type_of_expression(&value, &mut type_context).ok();

            let Some((binding_ctx, _annotations)) = context.bindings.get_mut(&identifier.0) else {
                return Err(diagnostic(
                    format!("Cannot assign to unbound identifier: {}", identifier.0),
                    span,
                ));
            };

            let expected_type = match binding_ctx {
                BindingContext::Bound(existing, _) => {
                    get_type_of_expression(existing, &mut type_context).ok()
                }
                BindingContext::UnboundWithType(expected_ty) => Some(expected_ty.clone()),
                BindingContext::UnboundWithoutType => None,
            };

            if let (Some(expected_ty), Some(actual_ty)) = (expected_type, value_type) {
                if !types_equivalent(&expected_ty, &actual_ty) {
                    return Err(diagnostic(
                        format!("Cannot assign value of mismatched type to {}", identifier.0),
                        span,
                    ));
                }
            }

            let preserve_behavior = match binding_ctx {
                BindingContext::Bound(_, preserve_behavior) => *preserve_behavior,
                BindingContext::UnboundWithType(_) | BindingContext::UnboundWithoutType => {
                    PreserveBehavior::PreserveBinding
                }
            };

            *binding_ctx = BindingContext::Bound(value, preserve_behavior);
            Ok(())
        }
        LValue::PropertyAccess {
            object,
            property,
            span: prop_span,
        } => {
            let current_object = get_lvalue_value(&object, context)?;
            let Expression::Struct(mut fields, struct_span) = current_object else {
                return Err(diagnostic("Property access on non-struct value", prop_span));
            };

            let mut found = false;
            for (field_id, field_expr) in fields.iter_mut() {
                if field_id.0 == property {
                    *field_expr = value.clone();
                    found = true;
                    break;
                }
            }

            if !found {
                return Err(diagnostic(
                    format!("Missing field {} in struct", property),
                    prop_span,
                ));
            }

            let updated_struct = Expression::Struct(fields, struct_span);
            apply_lvalue_update(*object, updated_struct, context, span)
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

    let mut type_context = context.clone();
    let value_type = get_type_of_expression(&value, &mut type_context).ok();
    let target_type = get_lvalue_type(&target, &mut type_context, span)?;

    if let Some(actual_type) = value_type {
        if !types_equivalent(&target_type, &actual_type) {
            return Err(diagnostic(
                format!(
                    "Cannot assign value of mismatched type to {}",
                    lvalue_display_name(&target)
                ),
                span,
            ));
        }
    }

    apply_lvalue_update(target, value.clone(), context, span)?;

    Ok(value)
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
    usage_counter: Option<&UsageCounter>,
) -> Result<(Expression, PreserveBehavior), Diagnostic> {
    if let Ok(value_type) = get_type_of_expression(&binding.expr, &mut context.clone()) {
        bind_pattern_blanks(
            binding.pattern.clone(),
            context,
            Vec::new(),
            Some(value_type),
        )?;
    }

    let value = interpret_expression(binding.expr.clone(), context)?;
    let value_is_constant = is_resolved_constant(&value);
    let (bound_success, preserve_behavior) = bind_pattern_from_value(
        binding.pattern.clone(),
        &value,
        context,
        Vec::new(),
        PreserveBehavior::Inline,
        usage_counter,
    )?;
    let force_preserve = pattern_has_mutable_annotation(&binding.pattern);
    let binding_expr = Expression::Binding(
        Box::new(Binding {
            pattern: binding.pattern,
            expr: value,
        }),
        dummy_span(),
    );

    Ok((
        if force_preserve
            || preserve_behavior != PreserveBehavior::Inline
            || (!value_is_constant && !bound_success)
        {
            binding_expr
        } else {
            Expression::Literal(ExpressionLiteral::Boolean(bound_success), dummy_span())
        },
        if force_preserve {
            preserve_behavior.max(PreserveBehavior::PreserveBinding)
        } else {
            preserve_behavior
        },
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
                context.bindings.insert(
                    identifier.0,
                    (
                        BindingContext::UnboundWithType(type_expr),
                        passed_annotations.clone(),
                    ),
                );
            } else {
                context.bindings.insert(
                    identifier.0,
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
            if let Some(Expression::Struct(type_fields, _)) = &type_hint {
                type_lookup = Some(type_fields.clone());
            }

            for (field_identifier, field_pattern) in pattern_items {
                let field_type_hint = type_lookup.as_ref().and_then(|fields| {
                    fields
                        .iter()
                        .find(|(name, _)| name.0 == field_identifier.0)
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
            variant, payload, ..
        } => {
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
            None,
        ),
    }
}

fn bind_pattern_from_value(
    pattern: BindingPattern,
    value: &Expression,
    context: &mut Context,
    passed_annotations: Vec<BindingAnnotation>,
    preserve_behavior: PreserveBehavior,
    usage_counter: Option<&UsageCounter>,
) -> Result<(bool, PreserveBehavior), Diagnostic> {
    match pattern {
        BindingPattern::Identifier(identifier, _) => {
            let mut identifier_preserve_behavior = preserve_behavior;
            if let Some(usage_counter) = usage_counter
                && let Some(usage_count) = usage_counter.counts.get(&identifier.0)
                && *usage_count > 1
                && !is_resolved_constant(value)
            {
                identifier_preserve_behavior =
                    identifier_preserve_behavior.max(PreserveBehavior::PreserveUsage);
            }
            context.bindings.insert(
                identifier.0,
                (
                    BindingContext::Bound(value.clone(), identifier_preserve_behavior),
                    passed_annotations.clone(),
                ),
            );
            Ok((true, identifier_preserve_behavior))
        }
        BindingPattern::Literal(literal, _) => match (literal, value) {
            (
                ExpressionLiteral::Number(pattern_value),
                Expression::Literal(ExpressionLiteral::Number(value), _),
            ) => Ok((pattern_value == *value, preserve_behavior)),
            (
                ExpressionLiteral::Boolean(pattern_value),
                Expression::Literal(ExpressionLiteral::Boolean(value), _),
            ) => Ok((pattern_value == *value, preserve_behavior)),
            _ => Ok((false, preserve_behavior)),
        },
        BindingPattern::Struct(pattern_items, span) => {
            let Expression::Struct(struct_items, _) = value else {
                // not sure if it is ok to fail here, it could be an identifier of the struct type
                return Err(diagnostic("Struct pattern requires struct value", span));
            };

            let mut preserve_behavior = preserve_behavior;
            let mut overall_matched = true;

            for (field_identifier, field_pattern) in pattern_items {
                let field_span = field_pattern.span();
                let field_value = struct_items
                    .iter()
                    .find(|(value_identifier, _)| value_identifier.0 == field_identifier.0)
                    .map(|(_, expr)| expr)
                    .ok_or_else(|| {
                        diagnostic(format!("Missing field {}", field_identifier.0), field_span)
                    })?;

                let (matched, new_preserve_behavior) = bind_pattern_from_value(
                    field_pattern,
                    field_value,
                    context,
                    passed_annotations.clone(),
                    preserve_behavior,
                    usage_counter,
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
            let Expression::EnumValue {
                enum_type: value_enum,
                variant: value_variant,
                payload: value_payload,
                ..
            } = value
            else {
                return Ok((false, preserve_behavior));
            };

            let enum_type_name = match enum_type.as_ref() {
                Expression::Identifier(id, _) => id.0.clone(),
                _ => "<unknown>".to_string(),
            };

            let expected_enum_type = resolve_enum_type_expression(enum_type.as_ref(), context)
                .ok_or_else(|| {
                    diagnostic(
                        format!("Enum pattern references unknown type: {}", enum_type_name),
                        span,
                    )
                })?;
            let actual_enum_type = resolve_expression(value_enum.as_ref().clone(), context)?;

            if !types_equivalent(&expected_enum_type, &actual_enum_type) {
                return Ok((false, preserve_behavior));
            }

            if value_variant.0 != variant.0 {
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
                    usage_counter,
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
            usage_counter,
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
            bind_pattern_from_value(
                *pattern,
                value,
                context,
                passed_annotations
                    .into_iter()
                    .chain(new_annotations.into_iter())
                    .collect(),
                new_preserve_behavior,
                usage_counter,
            )
        }
    }
}

fn force_bindings(pattern: &BindingPattern, value: &Expression, context: &mut Context) {
    match pattern {
        BindingPattern::Identifier(identifier, _) => {
            context.bindings.insert(
                identifier.0.clone(),
                (
                    BindingContext::Bound(value.clone(), PreserveBehavior::Inline),
                    Vec::new(),
                ),
            );
        }
        BindingPattern::TypeHint(inner, _, _) => force_bindings(inner, value, context),
        BindingPattern::Annotated { pattern, .. } => force_bindings(pattern, value, context),
        _ => {}
    }
}

fn parameter_identifier(pattern: &BindingPattern) -> Option<&Identifier> {
    match pattern {
        BindingPattern::Identifier(identifier, _) => Some(identifier),
        BindingPattern::TypeHint(inner, _, _) => parameter_identifier(inner),
        BindingPattern::Annotated { pattern, .. } => parameter_identifier(pattern),
        _ => None,
    }
}

fn interpret_numeric_intrinsic<F>(
    left: Expression,
    right: Expression,
    context: &mut Context,
    span: SourceSpan,
    op: F,
) -> Result<Expression, Diagnostic>
where
    F: Fn(i32, i32) -> i32,
{
    let left_value = evaluate_numeric_operand(left, context)?;
    let right_value = evaluate_numeric_operand(right, context)?;
    Ok(Expression::Literal(
        ExpressionLiteral::Number(op(left_value, right_value)),
        span,
    ))
}

fn interpret_divide_intrinsic(
    left: Expression,
    right: Expression,
    context: &mut Context,
    span: SourceSpan,
) -> Result<Expression, Diagnostic> {
    let left_value = evaluate_numeric_operand(left, context)?;
    let right_value = evaluate_numeric_operand(right, context)?;

    if right_value == 0 {
        return Err(diagnostic("Division by zero", span));
    }

    Ok(Expression::Literal(
        ExpressionLiteral::Number(left_value / right_value),
        span,
    ))
}

fn interpret_comparison_intrinsic<F>(
    left: Expression,
    right: Expression,
    context: &mut Context,
    span: SourceSpan,
    op: F,
) -> Result<Expression, Diagnostic>
where
    F: Fn(i32, i32) -> bool,
{
    let left_value = evaluate_numeric_operand(left, context)?;
    let right_value = evaluate_numeric_operand(right, context)?;
    Ok(Expression::Literal(
        ExpressionLiteral::Boolean(op(left_value, right_value)),
        span,
    ))
}

fn evaluate_numeric_operand(expr: Expression, context: &mut Context) -> Result<i32, Diagnostic> {
    let operand_span = expr.span();
    let evaluated = resolve_expression(expr, context)?;
    match evaluated {
        Expression::Literal(ExpressionLiteral::Number(value), _) => Ok(value),
        _ => Err(diagnostic(
            "Expected numeric literal during intrinsic operation",
            operand_span,
        )),
    }
}

fn interpret_boolean_intrinsic<F>(
    left: Expression,
    right: Expression,
    context: &mut Context,
    span: SourceSpan,
    op: F,
) -> Result<Expression, Diagnostic>
where
    F: Fn(bool, bool) -> bool,
{
    let left_value = evaluate_boolean_operand(left, context)?;
    let right_value = evaluate_boolean_operand(right, context)?;
    Ok(Expression::Literal(
        ExpressionLiteral::Boolean(op(left_value, right_value)),
        span,
    ))
}

fn evaluate_boolean_operand(expr: Expression, context: &mut Context) -> Result<bool, Diagnostic> {
    let operand_span = expr.span();
    let evaluated = resolve_expression(expr, context)?;
    match evaluated {
        Expression::Literal(ExpressionLiteral::Boolean(value), _) => Ok(value),
        _ => Err(diagnostic(
            "Expected boolean literal during intrinsic operation",
            operand_span,
        )),
    }
}

fn is_resolved_constant(expr: &Expression) -> bool {
    match expr {
        Expression::Literal(_, _) | Expression::IntrinsicType(_, _) => true,
        Expression::EnumType(variants, _) => {
            variants.iter().all(|(_, ty)| is_resolved_constant(ty))
        }
        Expression::EnumValue {
            enum_type, payload, ..
        } => {
            is_resolved_constant(enum_type)
                && payload
                    .as_ref()
                    .map(|p| is_resolved_constant(p))
                    .unwrap_or(true)
        }
        Expression::EnumConstructor {
            enum_type,
            payload_type,
            ..
        } => is_resolved_constant(enum_type) && is_resolved_constant(payload_type),
        Expression::Struct(items, _) => items
            .iter()
            .all(|(_, value_expr)| is_resolved_constant(value_expr)),
        Expression::AttachImplementation {
            type_expr,
            implementation,
            ..
        } => is_resolved_constant(type_expr) && is_resolved_constant(implementation),
        Expression::Function {
            parameter,
            return_type,
            body,
            ..
        } => {
            let new_function_context = {
                let mut ctx = Context::empty();
                bind_pattern_blanks(parameter.clone(), &mut ctx, Vec::new(), None).unwrap();
                ctx
            };
            is_resolved_constant(&return_type)
                && is_resolved_const_function_expression(&body, &new_function_context)
        }
        Expression::FunctionType {
            parameter,
            return_type,
            ..
        } => is_resolved_constant(parameter) && is_resolved_constant(return_type),
        Expression::Assignment { .. } => false,
        _ => false,
    }
}

fn is_resolved_const_function_expression(expr: &Expression, function_context: &Context) -> bool {
    match expr {
        Expression::Literal(_, _) | Expression::IntrinsicType(_, _) => true,
        Expression::Struct(items, _) => items.iter().all(|(_, value_expr)| {
            is_resolved_const_function_expression(value_expr, function_context)
        }),
        Expression::AttachImplementation {
            type_expr,
            implementation,
            ..
        } => {
            is_resolved_const_function_expression(type_expr, function_context)
                && is_resolved_const_function_expression(implementation, function_context)
        }
        Expression::Function {
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
            is_resolved_const_function_expression(&return_type, &new_function_context)
                && is_resolved_const_function_expression(&body, &new_function_context)
        }
        Expression::FunctionType {
            parameter,
            return_type,
            ..
        } => {
            is_resolved_const_function_expression(parameter, function_context)
                && is_resolved_const_function_expression(return_type, function_context)
        }
        Expression::Identifier(ident, _) => function_context.bindings.get(&ident.0).is_some(),
        Expression::IntrinsicOperation(intrinsic_operation, _) => match intrinsic_operation {
            IntrinsicOperation::Binary(left, right, _) => {
                is_resolved_const_function_expression(left, function_context)
                    && is_resolved_const_function_expression(right, function_context)
            }
            IntrinsicOperation::EnumFromStruct => true,
        },
        _ => false,
    }
}

pub fn intrinsic_context() -> Context {
    let mut context = Context {
        bindings: std::collections::HashMap::new(),
    };

    context.bindings.insert(
        "type".to_string(),
        (
            BindingContext::Bound(
                Expression::AttachImplementation {
                    type_expr: Box::new(intrinsic_type_expr(IntrinsicType::Type)),
                    implementation: Box::new(Expression::Struct(vec![], dummy_span())),
                    span: dummy_span(),
                },
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
                    Identifier(name.to_string()),
                    dummy_span(),
                )),
                Box::new(intrinsic_type_expr(IntrinsicType::I32)),
                dummy_span(),
            )
        };

        (
            Identifier(symbol.to_string()),
            Expression::Function {
                parameter: typed_pattern("self"),
                return_type: Box::new(Expression::FunctionType {
                    parameter: Box::new(intrinsic_type_expr(IntrinsicType::I32)),
                    return_type: Box::new(intrinsic_type_expr(IntrinsicType::I32)),
                    span: dummy_span(),
                }),
                body: Box::new(Expression::Function {
                    parameter: typed_pattern("other"),
                    return_type: Box::new(intrinsic_type_expr(IntrinsicType::I32)),
                    body: Box::new(Expression::IntrinsicOperation(
                        IntrinsicOperation::Binary(
                            Box::new(identifier_expr("self")),
                            Box::new(identifier_expr("other")),
                            operator,
                        ),
                        dummy_span(),
                    )),
                    span: dummy_span(),
                }),
                span: dummy_span(),
            },
        )
    }

    context.bindings.insert(
        "i32".to_string(),
        (
            BindingContext::Bound(
                Expression::AttachImplementation {
                    type_expr: Box::new(intrinsic_type_expr(IntrinsicType::I32)),
                    implementation: Box::new(Expression::Struct(
                        vec![
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
                        ],
                        dummy_span(),
                    )),
                    span: dummy_span(),
                },
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
                    Identifier(name.to_string()),
                    dummy_span(),
                )),
                Box::new(intrinsic_type_expr(IntrinsicType::Boolean)),
                dummy_span(),
            )
        };

        (
            Identifier(symbol.to_string()),
            Expression::Function {
                parameter: typed_pattern("self"),
                return_type: Box::new(Expression::FunctionType {
                    parameter: Box::new(intrinsic_type_expr(IntrinsicType::Boolean)),
                    return_type: Box::new(intrinsic_type_expr(IntrinsicType::Boolean)),
                    span: dummy_span(),
                }),
                body: Box::new(Expression::Function {
                    parameter: typed_pattern("other"),
                    return_type: Box::new(intrinsic_type_expr(IntrinsicType::Boolean)),
                    body: Box::new(Expression::IntrinsicOperation(
                        IntrinsicOperation::Binary(
                            Box::new(identifier_expr("self")),
                            Box::new(identifier_expr("other")),
                            operator,
                        ),
                        dummy_span(),
                    )),
                    span: dummy_span(),
                }),
                span: dummy_span(),
            },
        )
    }

    context.bindings.insert(
        "bool".to_string(),
        (
            BindingContext::Bound(
                Expression::AttachImplementation {
                    type_expr: Box::new(intrinsic_type_expr(IntrinsicType::Boolean)),
                    implementation: Box::new(Expression::Struct(
                        vec![
                            bool_binary_intrinsic("==", BinaryIntrinsicOperator::I32Equal),
                            bool_binary_intrinsic("!=", BinaryIntrinsicOperator::I32NotEqual),
                            bool_binary_intrinsic("&&", BinaryIntrinsicOperator::BooleanAnd),
                            bool_binary_intrinsic("||", BinaryIntrinsicOperator::BooleanOr),
                            bool_binary_intrinsic("^", BinaryIntrinsicOperator::BooleanXor),
                        ],
                        dummy_span(),
                    )),
                    span: dummy_span(),
                },
                PreserveBehavior::Inline,
            ),
            Vec::new(),
        ),
    );

    context.bindings.insert(
        "true".to_string(),
        (
            BindingContext::Bound(
                Expression::Literal(ExpressionLiteral::Boolean(true), dummy_span()),
                PreserveBehavior::Inline,
            ),
            Vec::new(),
        ),
    );

    context.bindings.insert(
        "false".to_string(),
        (
            BindingContext::Bound(
                Expression::Literal(ExpressionLiteral::Boolean(false), dummy_span()),
                PreserveBehavior::Inline,
            ),
            Vec::new(),
        ),
    );

    context.bindings.insert(
        "js".to_string(),
        (
            BindingContext::Bound(
                Expression::Literal(
                    ExpressionLiteral::Target(TargetLiteral::JSTarget),
                    dummy_span(),
                ),
                PreserveBehavior::Inline,
            ),
            Vec::new(),
        ),
    );

    context.bindings.insert(
        "wasm".to_string(),
        (
            BindingContext::Bound(
                Expression::Literal(
                    ExpressionLiteral::Target(TargetLiteral::WasmTarget),
                    dummy_span(),
                ),
                PreserveBehavior::Inline,
            ),
            Vec::new(),
        ),
    );

    context.bindings.insert(
        "enum".to_string(),
        (
            BindingContext::Bound(
                Expression::IntrinsicOperation(IntrinsicOperation::EnumFromStruct, dummy_span()),
                PreserveBehavior::Inline,
            ),
            Vec::new(),
        ),
    );
    context
}

pub fn evaluate_text_to_raw_expression(program: &str) -> Result<(Expression, Context), Diagnostic> {
    let (expression, remaining) =
        crate::parsing::parse_block(program).expect("Failed to parse program text");
    assert!(
        remaining.trim().is_empty(),
        "Parser did not consume entire input, remaining: {remaining:?}"
    );

    let mut context = intrinsic_context();
    interpret_program(expression, &mut context)
}

pub fn evaluate_text_to_expression(program: &str) -> Result<(Expression, Context), Diagnostic> {
    let (expression, remaining) =
        crate::parsing::parse_block(program).expect("Failed to parse program text");
    assert!(
        remaining.trim().is_empty(),
        "Parser did not consume entire input, remaining: {remaining:?}"
    );

    let mut context = intrinsic_context();
    let (value, context) = interpret_program(expression, &mut context)?;

    let final_value = match &value {
        Expression::Block(exprs, _) => exprs.last().cloned().unwrap_or(value),
        other => other.clone(),
    };

    Ok((final_value, context))
}

#[cfg(test)]
fn evaluate_text_to_number(program: &str) -> i32 {
    match evaluate_text_to_expression(program)
        .expect("Failed to interpret parsed expression")
        .0
    {
        Expression::Literal(ExpressionLiteral::Number(value), _) => value,
        _ => panic!("Expected numeric result"),
    }
}

fn resolve_value(expr: Expression, context: &mut Context) -> Result<Expression, Diagnostic> {
    match expr {
        Expression::Identifier(ident, span) => {
            if let Some(binding) = context.bindings.get(&ident.0) {
                match &binding.0 {
                    BindingContext::Bound(val, _) => {
                        if matches!(val, Expression::Identifier(id, _) if id.0 == ident.0) {
                            Ok(Expression::Identifier(ident, span))
                        } else {
                            resolve_value(val.clone(), context)
                        }
                    }
                    _ => Ok(Expression::Identifier(ident, span)),
                }
            } else {
                Ok(Expression::Identifier(ident, span))
            }
        }
        other => Ok(other),
    }
}

fn resolve_expression(expr: Expression, context: &mut Context) -> Result<Expression, Diagnostic> {
    match expr {
        Expression::Identifier(..) => resolve_value(expr, context),
        other => {
            let evaluated = interpret_expression(other, context)?;
            resolve_value(evaluated, context)
        }
    }
}

struct UsageCounter {
    counts: std::collections::HashMap<String, usize>,
}

impl UsageCounter {
    fn new() -> Self {
        Self {
            counts: std::collections::HashMap::new(),
        }
    }

    fn count(&mut self, expr: &Expression) {
        match expr {
            Expression::Identifier(ident, _) => {
                *self.counts.entry(ident.0.clone()).or_default() += 1;
            }
            Expression::Assignment { target, expr, .. } => {
                self.count_lvalue(target);
                self.count(expr);
            }
            Expression::Block(exprs, _) => {
                for e in exprs {
                    self.count(e);
                }
            }
            Expression::Function {
                body,
                parameter,
                return_type,
                ..
            } => {
                self.count(body);
                self.count(return_type);
                self.count_pattern(parameter);
            }
            Expression::FunctionCall {
                function, argument, ..
            } => {
                self.count(function);
                self.count(argument);
            }
            Expression::Loop { body, .. } => {
                self.count(body);
            }
            Expression::Return { value, .. } => {
                if let Some(expr) = value {
                    self.count(expr);
                }
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.count(condition);
                self.count(then_branch);
                if let Some(else_branch) = else_branch {
                    self.count(else_branch);
                }
            }
            Expression::Operation { left, right, .. } => {
                self.count(left);
                self.count(right);
            }
            Expression::Binding(binding, _) => {
                self.count(&binding.expr);
                self.count_pattern(&binding.pattern);
            }
            Expression::IntrinsicOperation(op, _) => match op {
                IntrinsicOperation::Binary(left, right, _) => {
                    self.count(left);
                    self.count(right);
                }
                IntrinsicOperation::EnumFromStruct => {}
            },
            Expression::Struct(items, _) => {
                for (_, expr) in items {
                    self.count(expr);
                }
            }
            Expression::EnumValue {
                enum_type, payload, ..
            } => {
                self.count(enum_type);
                if let Some(payload) = payload {
                    self.count(payload);
                }
            }
            Expression::EnumConstructor {
                enum_type,
                payload_type,
                ..
            } => {
                self.count(enum_type);
                self.count(payload_type);
            }
            Expression::EnumAccess { enum_expr, .. } => {
                self.count(enum_expr);
            }
            Expression::EnumType(variants, _) => {
                for (_, ty) in variants {
                    self.count(ty);
                }
            }
            Expression::PropertyAccess { object, .. } => {
                self.count(object);
            }
            Expression::AttachImplementation {
                type_expr,
                implementation,
                ..
            } => {
                self.count(type_expr);
                self.count(implementation);
            }
            Expression::FunctionType {
                parameter,
                return_type,
                ..
            } => {
                self.count(parameter);
                self.count(return_type);
            }
            Expression::Literal(..) | Expression::IntrinsicType(..) => {}
        }
    }

    fn count_lvalue(&mut self, target: &LValue) {
        match target {
            LValue::Identifier(identifier, _) => {
                *self.counts.entry(identifier.0.clone()).or_default() += 1;
            }
            LValue::PropertyAccess { object, .. } => self.count_lvalue(object),
        }
    }

    fn count_pattern(&mut self, pattern: &BindingPattern) {
        match pattern {
            BindingPattern::TypeHint(inner, type_expr, _) => {
                self.count_pattern(inner);
                self.count(type_expr);
            }
            BindingPattern::Annotated {
                pattern,
                annotations,
                ..
            } => {
                self.count_pattern(pattern);
                for ann in annotations {
                    match ann {
                        BindingAnnotation::Export(expr, _) => self.count(expr),
                        BindingAnnotation::Mutable(_) => {}
                    }
                }
            }
            BindingPattern::Struct(items, _) => {
                for (_, pat) in items {
                    self.count_pattern(pat);
                }
            }
            BindingPattern::EnumVariant { payload, .. } => {
                if let Some(payload) = payload {
                    self.count_pattern(payload);
                }
            }
            BindingPattern::Identifier(..) | BindingPattern::Literal(..) => {}
        }
    }

    fn get_preserve_behavior(&self, pattern: &BindingPattern) -> PreserveBehavior {
        match pattern {
            BindingPattern::Identifier(ident, _) => {
                (self.counts.get(&ident.0).copied().unwrap_or(0) > 1)
                    .then(|| PreserveBehavior::PreserveUsage)
                    .unwrap_or(PreserveBehavior::Inline)
            }
            BindingPattern::Literal(..) => PreserveBehavior::Inline,
            BindingPattern::TypeHint(inner, _, _) => self.get_preserve_behavior(inner),
            BindingPattern::Annotated {
                pattern,
                annotations,
                ..
            } => Ord::max(
                self.get_preserve_behavior(pattern),
                annotations
                    .iter()
                    .map(|ann| match ann {
                        BindingAnnotation::Export(..) => PreserveBehavior::PreserveBinding,
                        BindingAnnotation::Mutable(..) => PreserveBehavior::PreserveBinding,
                    })
                    .max()
                    .unwrap_or(PreserveBehavior::Inline),
            ),
            BindingPattern::Struct(items, _) => items
                .iter()
                .map(|(_, pat)| self.get_preserve_behavior(pat))
                .max()
                .unwrap_or(PreserveBehavior::Inline),
            BindingPattern::EnumVariant { payload, .. } => payload
                .as_ref()
                .map(|p| self.get_preserve_behavior(p))
                .unwrap_or(PreserveBehavior::Inline),
        }
    }
}

#[test]
fn test_basic_binding_interpretation() {
    let mut context = intrinsic_context();

    let expr = Expression::Block(
        vec![
            Expression::Binding(
                Box::new(Binding {
                    pattern: BindingPattern::TypeHint(
                        Box::new(BindingPattern::Identifier(
                            Identifier("x".to_string()),
                            dummy_span(),
                        )),
                        Box::new(intrinsic_type_expr(IntrinsicType::I32)),
                        dummy_span(),
                    ),
                    expr: literal_number_expr(5),
                }),
                dummy_span(),
            ),
            identifier_expr("x"),
        ],
        dummy_span(),
    );

    let result = interpret_expression(expr, &mut context).unwrap();

    if let Expression::Literal(ExpressionLiteral::Number(value), _) = result {
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

    let expr = Expression::Operation {
        operator: "+".to_string(),
        left: Box::new(literal_number_expr(5)),
        right: Box::new(literal_number_expr(10)),
        span: dummy_span(),
    };

    let result = interpret_expression(expr, &mut context).unwrap();

    if let Expression::Literal(ExpressionLiteral::Number(value), _) = result {
        assert_eq!(value, 15);
    } else {
        panic!("Expected a number literal as result");
    }
}

#[test]
fn i32_is_constant() {
    let binding = intrinsic_context();
    let Some((BindingContext::Bound(expr, _), _)) = binding.bindings.get("i32") else {
        panic!("i32 binding not found");
    };
    assert!(is_resolved_constant(expr));
}

#[test]
fn interpret_text_user_defined_function2() {
    let program = "
fn(bar: i32) -> i32 (
    bar + 1
)
    ";
    let (expr, _) = evaluate_text_to_expression(program).unwrap();
    assert!(matches!(expr, Expression::Function { .. }));
}
#[test]
fn interpret_let_binding_function() {
    let program = "
let Level2 = enum { Some = i32, None = {} };
let Level1 = enum { Some = Level2, None = {} };

let export(wasm) check = fn(x: i32) -> i32 (
    let foo = if x > 0 (Level1::Some(Level2::Some(x))) else (Level1::None);

    if let Level1::Some(Level2::Some(b)) = foo (
        b
    ) else (
        0
    )
);
{}
    ";
    let (expr, _) = evaluate_text_to_raw_expression(program).unwrap();
    println!("{expr:?}");
}

#[test]
fn interpret_text_user_defined_function() {
    let program = "
let foo = fn(bar: i32) -> i32 (
    bar + 1
);
foo(123)
    ";
    assert_eq!(evaluate_text_to_number(program), 124);
}

#[test]
fn interpret_text_user_defined_tuple_arguments() {
    let program = "
let foo2 = fn{bar1: i32, bar2: i32} -> i32 (
    bar1 + bar2
);
foo2{100, 24}
    ";
    assert_eq!(evaluate_text_to_number(program), 124);
}

#[test]
fn interpret_text_struct_property_access() {
    let program = "
let point = { x = 5, y = 10 };
point.x
    ";
    assert_eq!(evaluate_text_to_number(program), 5);
}

#[test]
fn interpret_text_struct_property_call() {
    let program = "
let container = {
    inc = fn(value: i32) -> i32 (
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
let export(js) answer: i32 = 42;
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
fn interpret_preserves_bindings_in_function() {
    let program = "
    let export(wasm) double_add = fn(x: i32) -> i32 (
        let y = x * 2;
        y + y
    );
    {}
    ";
    let (_, context) = evaluate_text_to_expression(program).unwrap();
    let bindings = context.annotated_bindings();
    let double_add = bindings
        .iter()
        .find(|b| b.name == "double_add")
        .expect("double_add not found");

    if let Expression::Function { body, .. } = &double_add.value {
        // The body should be a Block containing `let y = ...` and `y + y`.
        if let Expression::Block(exprs, _) = &**body {
            assert_eq!(exprs.len(), 2);
            match &exprs[0] {
                Expression::Binding(binding, _) => {
                    if let BindingPattern::Identifier(ident, _) = &binding.pattern {
                        assert_eq!(ident.0, "y");
                    } else {
                        panic!("Expected identifier pattern for y");
                    }
                }
                _ => panic!("Expected binding as first expression"),
            }
        } else {
            panic!("Expected function body to be a block, got {:?}", body);
        }
    } else {
        panic!("Expected function value");
    }
}

#[test]
fn interpret_inlines_single_use() {
    let program = "
    let export(wasm) add_one = fn(x: i32) -> i32 (
        let y = x + 1;
        y
    );
    {}
    ";
    let (_, context) = evaluate_text_to_expression(program).unwrap();
    let bindings = context.annotated_bindings();
    let add_one = bindings
        .iter()
        .find(|b| b.name == "add_one")
        .expect("add_one not found");

    if let Expression::Function { body, .. } = &add_one.value {
        match &**body {
            Expression::IntrinsicOperation(..) => {} // Good
            Expression::Block(..) => panic!("Expected inlined expression, got Block"),
            _ => panic!("Expected IntrinsicOperation, got {:?}", body),
        }
    } else {
        panic!("Expected function value");
    }
}

#[test]
fn interpret_basic_enum_flow() {
    let program = "
    let IntOption = enum { Some = i32, None = {} };
    let export(wasm) pick_positive = fn(x: i32) -> i32 (
        let opt = if x > 0 (
            IntOption::Some(x)
        ) else (
            IntOption::None
        );

        if let IntOption::Some(value) = opt (
            value
        ) else (
            0
        )
    );
    pick_positive(3)
    ";

    let (ast, remaining) = crate::parsing::parse_block(program).unwrap();
    assert!(remaining.trim().is_empty());
    let mut context = intrinsic_context();
    if let Expression::Block(exprs, _) = ast {
        let mut last = None;
        for expr in exprs {
            last = Some(interpret_expression(expr, &mut context).unwrap());
        }
        match last {
            Some(Expression::Literal(ExpressionLiteral::Number(v), _)) => assert_eq!(v, 3),
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
        .bindings
        .get("enum")
        .expect("enum intrinsic should be present")
        .clone();

    match &enum_binding.0 {
        BindingContext::Bound(expr, _) => match get_type_of_expression(expr, &mut context) {
            Ok(Expression::FunctionType {
                parameter,
                return_type,
                ..
            }) => {
                assert!(matches!(
                    parameter.as_ref(),
                    Expression::IntrinsicType(IntrinsicType::Type, _)
                ));
                assert!(matches!(
                    return_type.as_ref(),
                    Expression::IntrinsicType(IntrinsicType::Type, _)
                ));
            }
            other => panic!("unexpected enum intrinsic type {other:?}"),
        },
        other => panic!("unexpected enum intrinsic binding {other:?}"),
    }
}

#[test]
fn enum_intrinsic_can_be_aliased() {
    let program = "
    let Alias = enum;
    let IntOption = Alias { Some = i32, None = {} };
    IntOption::None
    ";

    let (value, _context) = evaluate_text_to_expression(program).unwrap();
    match value {
        Expression::EnumValue { variant, .. } => assert_eq!(variant.0, "None"),
        other => panic!("unexpected alias result {other:?}"),
    }
}

#[test]
fn enum_pattern_rejects_unknown_type() {
    let program = "
    let Opt = enum { Some = i32, None = {} };
    let value = Opt::Some(1);
    if let Missing::Some(v) = value ( v ) else ( 0 )
    ";

    let error = match evaluate_text_to_expression(program) {
        Ok(_) => panic!("Expected error for unknown enum"),
        Err(err) => err,
    };
    assert!(
        error
            .message
            .contains("Enum pattern references unknown type: Missing")
    );
}

#[test]
fn enum_pattern_requires_matching_type() {
    let program = "
    let First = enum { Some = i32, None = {} };
    let Second = enum { Some = {}, None = {} };
    let check = fn{} -> i32 (
        let value = First::Some(5);
        if let Second::Some = value ( 1 ) else ( 0 )
    );
    check{}
    ";

    let (value, _context) = evaluate_text_to_expression(program).unwrap();
    match value {
        Expression::Literal(ExpressionLiteral::Number(result), _) => assert_eq!(result, 0),
        other => panic!("unexpected result {other:?}"),
    }
}

#[test]
fn enum_rejects_value_payloads() {
    let program = "
    let Bad = enum { Value = 1 };
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
    let Expression::Struct(variants, _) = &argument_value else {
        return Err(diagnostic("enum expects a struct of variants", span));
    };

    let mut evaluated_variants = Vec::with_capacity(variants.len());
    for (variant_name, variant_type) in variants {
        let evaluated_type = interpret_expression(variant_type.clone(), context)?;
        if !is_type_expression(&evaluated_type) {
            return Err(diagnostic(
                "Enum variant payload must be a type",
                variant_type.span(),
            ));
        }
        evaluated_variants.push((variant_name.clone(), evaluated_type));
    }

    Ok(Expression::EnumType(evaluated_variants, span))
}
