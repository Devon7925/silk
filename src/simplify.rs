use crate::interpret::Context;
use crate::parsing::{ExpressionKind, Identifier};
use crate::{
    Diagnostic,
    interpret::BindingContext,
    parsing::{BinaryIntrinsicOperator, Binding, BindingPattern, Expression, IntrinsicOperation},
};
use std::collections::HashMap;

#[cfg(test)]
use crate::parsing::{BindingAnnotation, ExpressionLiteral, TargetLiteral};

pub fn simplify_expression(expr: Expression) -> Result<Expression, Diagnostic> {
    let span = expr.span;
    let result = match expr.kind {
        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(left, right, op)) => {
            ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(
                Box::new(simplify_expression(*left)?),
                Box::new(simplify_expression(*right)?),
                op,
            ))
            .with_span(span)
        }
        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(operand, op)) => {
            ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(
                Box::new(simplify_expression(*operand)?),
                op,
            ))
            .with_span(span)
        }
        ExpressionKind::AttachImplementation { type_expr, .. } => simplify_expression(*type_expr)?,
        ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => ExpressionKind::If {
            condition: Box::new(simplify_expression(*condition)?),
            then_branch: Box::new(simplify_expression(*then_branch)?),
            else_branch: Box::new(simplify_expression(*else_branch)?),
        }
        .with_span(span),
        ExpressionKind::Match { value, branches } => ExpressionKind::Match {
            value: Box::new(simplify_expression(*value)?),
            branches: branches
                .into_iter()
                .map(|(pattern, expr)| {
                    Ok((
                        simplify_binding_pattern(pattern)?,
                        simplify_expression(expr)?,
                    ))
                })
                .collect::<Result<Vec<_>, Diagnostic>>()?,
        }
        .with_span(span),
        ExpressionKind::Function {
            parameter,
            return_type,
            body,
        } => ExpressionKind::Function {
            parameter: simplify_binding_pattern(parameter)?,
            return_type: Some(Box::new(simplify_expression(
                *return_type.expect("Function return type should be Some"),
            )?)),
            body: Box::new(simplify_expression(*body)?),
        }
        .with_span(span),
        ExpressionKind::FunctionType {
            parameter,
            return_type,
        } => ExpressionKind::FunctionType {
            parameter: Box::new(simplify_expression(*parameter)?),
            return_type: Box::new(simplify_expression(*return_type)?),
        }
        .with_span(span),
        ExpressionKind::Loop { body } => ExpressionKind::Loop {
            body: Box::new(simplify_expression(*body)?),
        }
        .with_span(span),
        ExpressionKind::Struct(items) => {
            let simplified_items = items
                .into_iter()
                .map(|(id, expr)| Ok((id, simplify_expression(expr)?)))
                .collect::<Result<_, Diagnostic>>()?;

            ExpressionKind::Struct(simplified_items).with_span(span)
        }
        ExpressionKind::Operation {
            operator,
            left,
            right,
        } => {
            if let Some(intrinsic) = intrinsic_operator(&operator) {
                ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(
                    Box::new(simplify_expression(*left)?),
                    Box::new(simplify_expression(*right)?),
                    intrinsic,
                ))
                .with_span(span)
            } else {
                ExpressionKind::FunctionCall {
                    function: Box::new(Expression::new(
                        ExpressionKind::PropertyAccess {
                            object: Box::new(simplify_expression(*left)?),
                            property: operator,
                        },
                        span,
                    )),
                    argument: Box::new(simplify_expression(*right)?),
                }
                .with_span(span)
            }
        }
        ExpressionKind::FunctionCall { function, argument } => ExpressionKind::FunctionCall {
            function: Box::new(simplify_expression(*function)?),
            argument: Box::new(simplify_expression(*argument)?),
        }
        .with_span(span),
        ExpressionKind::PropertyAccess { object, property } => ExpressionKind::PropertyAccess {
            object: Box::new(simplify_expression(*object)?),
            property,
        }
        .with_span(span),
        ExpressionKind::Diverge {
            value,
            divergance_type,
        } => ExpressionKind::Diverge {
            value: Box::new(simplify_expression(*value)?),
            divergance_type,
        }
        .with_span(span),
        ExpressionKind::EnumType(variants) => ExpressionKind::EnumType(
            variants
                .into_iter()
                .map(|(id, ty)| Ok((id, simplify_expression(ty)?)))
                .collect::<Result<_, Diagnostic>>()?,
        )
        .with_span(span),
        ExpressionKind::EnumAccess { enum_expr, variant } => {
            let simplified_enum = simplify_expression(*enum_expr)?;
            if let ExpressionKind::EnumType(variants) = &simplified_enum.kind
                && let Some((variant_index, (_id, payload_type))) = variants
                    .iter()
                    .enumerate()
                    .find(|(_, (id, _))| id.name == variant.name)
            {
                if let ExpressionKind::Struct(fields) = &payload_type.kind
                    && fields.is_empty()
                {
                    ExpressionKind::EnumValue {
                        enum_type: Box::new(simplified_enum.clone()),
                        variant,
                        variant_index,
                        payload: Box::new(ExpressionKind::Struct(vec![]).with_span(span)),
                    }
                    .with_span(span)
                } else {
                    ExpressionKind::EnumConstructor {
                        enum_type: Box::new(simplified_enum.clone()),
                        variant,
                        variant_index,
                        payload_type: Box::new(payload_type.clone()),
                    }
                    .with_span(span)
                }
            } else {
                ExpressionKind::EnumAccess {
                    enum_expr: Box::new(simplified_enum),
                    variant,
                }
                .with_span(span)
            }
        }
        ExpressionKind::EnumConstructor {
            enum_type,
            variant,
            variant_index,
            payload_type,
        } => ExpressionKind::EnumConstructor {
            enum_type: Box::new(simplify_expression(*enum_type)?),
            variant,
            variant_index,
            payload_type: Box::new(simplify_expression(*payload_type)?),
        }
        .with_span(span),
        ExpressionKind::EnumValue {
            enum_type,
            variant,
            variant_index,
            payload,
        } => ExpressionKind::EnumValue {
            enum_type: Box::new(simplify_expression(*enum_type)?),
            variant,
            variant_index,
            payload: Box::new(simplify_expression(*payload)?),
        }
        .with_span(span),
        ExpressionKind::Assignment { target, expr } => ExpressionKind::Assignment {
            target,
            expr: Box::new(simplify_expression(*expr)?),
        }
        .with_span(span),
        ExpressionKind::Binding(binding) => {
            let binding = Binding {
                pattern: simplify_binding_pattern(binding.pattern)?,
                expr: simplify_expression(binding.expr)?,
            };

            ExpressionKind::Binding(Box::new(binding)).with_span(span)
        }
        ExpressionKind::Block(expressions) => {
            let simplified_exprs = expressions
                .into_iter()
                .map(simplify_expression)
                .collect::<Result<Vec<_>, Diagnostic>>()?;
            ExpressionKind::Block(simplified_exprs).with_span(span)
        }
        ExpressionKind::Identifier(id) => Expression::new(ExpressionKind::Identifier(id), span),
        ExpressionKind::IntrinsicType(intrinsic) => {
            ExpressionKind::IntrinsicType(intrinsic).with_span(span)
        }
        ExpressionKind::Literal(lit) => Expression::new(ExpressionKind::Literal(lit), span),
    };
    Ok(result)
}

fn intrinsic_operator(operator: &str) -> Option<BinaryIntrinsicOperator> {
    match operator {
        "+" => Some(BinaryIntrinsicOperator::I32Add),
        "-" => Some(BinaryIntrinsicOperator::I32Subtract),
        "*" => Some(BinaryIntrinsicOperator::I32Multiply),
        "/" => Some(BinaryIntrinsicOperator::I32Divide),
        "==" => Some(BinaryIntrinsicOperator::I32Equal),
        "!=" => Some(BinaryIntrinsicOperator::I32NotEqual),
        "<" => Some(BinaryIntrinsicOperator::I32LessThan),
        ">" => Some(BinaryIntrinsicOperator::I32GreaterThan),
        "<=" => Some(BinaryIntrinsicOperator::I32LessThanOrEqual),
        ">=" => Some(BinaryIntrinsicOperator::I32GreaterThanOrEqual),
        "&&" => Some(BinaryIntrinsicOperator::BooleanAnd),
        "||" => Some(BinaryIntrinsicOperator::BooleanOr),
        "^" => Some(BinaryIntrinsicOperator::BooleanXor),
        _ => None,
    }
}

fn simplify_binding_pattern(pattern: BindingPattern) -> Result<BindingPattern, Diagnostic> {
    match pattern {
        pat @ BindingPattern::Identifier(..) => Ok(pat),
        pat @ BindingPattern::Literal(..) => Ok(pat),
        BindingPattern::Struct(items, source_span) => {
            let simplified_items = items
                .into_iter()
                .map(|(id, pat)| Ok((id, simplify_binding_pattern(pat)?)))
                .collect::<Result<_, Diagnostic>>()?;
            Ok(BindingPattern::Struct(simplified_items, source_span))
        }
        BindingPattern::TypeHint(binding_pattern, expression, source_span) => {
            Ok(BindingPattern::TypeHint(
                Box::new(simplify_binding_pattern(*binding_pattern)?),
                Box::new(simplify_expression(*expression)?),
                source_span,
            ))
        }
        BindingPattern::EnumVariant {
            enum_type,
            variant,
            payload,
            span,
        } => Ok(BindingPattern::EnumVariant {
            enum_type: Box::new(simplify_expression(*enum_type)?),
            variant,
            payload: match payload {
                Some(payload) => Some(Box::new(simplify_binding_pattern(*payload)?)),
                None => None,
            },
            span,
        }),
        BindingPattern::Annotated {
            annotations,
            pattern,
            span,
        } => Ok(BindingPattern::Annotated {
            annotations,
            pattern: Box::new(simplify_binding_pattern(*pattern)?),
            span,
        }),
    }
}

fn simplify_binding_context(binding_context: BindingContext) -> Result<BindingContext, Diagnostic> {
    match binding_context {
        BindingContext::Bound(expression, preserve_behavior) => Ok(BindingContext::Bound(
            simplify_expression(expression)?,
            preserve_behavior,
        )),
        BindingContext::UnboundWithType(expression) => Ok(BindingContext::UnboundWithType(
            simplify_expression(expression)?,
        )),
        BindingContext::UnboundWithoutType => Ok(BindingContext::UnboundWithoutType),
    }
}

pub fn simplify_context(context: Context) -> Result<Context, Diagnostic> {
    use crate::parsing::BindingAnnotation;
    let simplified_bindings = context
        .bindings
        .into_iter()
        .map(|binding_context| {
            binding_context
                    .into_iter()
                    .map(|(bind_name, (binding, annotations))| {
                        Ok((bind_name, (simplify_binding_context(binding)?, annotations)))
                    })
                    .collect::<Result<
                        HashMap<Identifier, (BindingContext, Vec<BindingAnnotation>)>,
                        Diagnostic,
                    >>()
        })
        .collect::<Result<_, Diagnostic>>()?;
    Ok(Context {
        bindings: simplified_bindings,
        in_loop: false,
    })
}

pub fn evaluate_text_to_simplified_expression(
    program: &str,
) -> Result<(Expression, Context), Diagnostic> {
    use crate::interpret::{interpret_program, intrinsic_context};

    let (expression, remaining) =
        crate::parsing::parse_block(program).expect("Failed to parse program text");
    assert!(
        remaining.trim().is_empty(),
        "Parser did not consume entire input, remaining: {remaining:?}"
    );

    let mut context = intrinsic_context();
    let (result, context) = interpret_program(expression, &mut context)?;
    let simplified_expression = simplify_expression(result)?;
    let simplified_context = simplify_context(context)?;
    Ok((simplified_expression, simplified_context))
}

#[test]
fn interpret_exported_function() {
    let program = "
(export js) add_one := (x: i32) => (
    x + 1
);
{}
    ";
    let (_result, context) =
        evaluate_text_to_simplified_expression(&program).expect("interpretation should succeed");
    let annotated_bindings = context.annotated_bindings();
    assert_eq!(annotated_bindings.len(), 1);
    let exported_binding = &annotated_bindings[0];
    assert_eq!(exported_binding.identifier.name, "add_one");
    assert!(exported_binding.annotations.len() == 1);
    let target_expr = exported_binding
        .annotations
        .iter()
        .find_map(|ann| match ann {
            BindingAnnotation::Export(expr, _) => Some(expr),
            _ => None,
        })
        .expect("expected export annotation");
    let ExpressionKind::Literal(ExpressionLiteral::Target(TargetLiteral::JSTarget)) =
        target_expr.kind
    else {
        panic!("expected js target in export annotation");
    };
    println!("Exported binding value: {:?}", exported_binding.value);
}

#[test]
fn interpret_exported_function_w_binding() {
    let program = include_str!("../fixtures/binding_in_function.silk");
    let (_result, context) =
        evaluate_text_to_simplified_expression(&program).expect("interpretation should succeed");
    let annotated_bindings = context.annotated_bindings();
    assert_eq!(annotated_bindings.len(), 1);
    let exported_binding = &annotated_bindings[0];
    assert_eq!(exported_binding.identifier.name, "add_one_squared");
    assert!(exported_binding.annotations.len() == 1);
    let target_expr = exported_binding
        .annotations
        .iter()
        .find_map(|ann| match ann {
            BindingAnnotation::Export(expr, _) => Some(expr),
            _ => None,
        })
        .expect("expected export annotation");
    let ExpressionKind::Literal(ExpressionLiteral::Target(TargetLiteral::WasmTarget)) =
        target_expr.kind
    else {
        panic!("expected wasm target in export annotation");
    };
    println!("Exported binding value: {:?}", exported_binding.value);
}
