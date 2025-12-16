use crate::interpret::Context;
use crate::parsing::Identifier;
use crate::{
    Diagnostic,
    interpret::BindingContext,
    parsing::{BinaryIntrinsicOperator, Binding, BindingPattern, Expression, IntrinsicOperation},
};
use std::collections::HashMap;

#[cfg(test)]
use crate::parsing::{BindingAnnotation, ExpressionLiteral, TargetLiteral};

pub fn simplify_expression(expr: Expression) -> Result<Expression, Diagnostic> {
    match expr {
        Expression::IntrinsicOperation(
            IntrinsicOperation::Binary(left, right, op),
            source_span,
        ) => Ok(Expression::IntrinsicOperation(
            IntrinsicOperation::Binary(
                Box::new(simplify_expression(*left)?),
                Box::new(simplify_expression(*right)?),
                op,
            ),
            source_span,
        )),
        Expression::IntrinsicOperation(IntrinsicOperation::Unary(operand, op), source_span) => {
            Ok(Expression::IntrinsicOperation(
                IntrinsicOperation::Unary(Box::new(simplify_expression(*operand)?), op),
                source_span,
            ))
        }
        Expression::AttachImplementation { type_expr, .. } => simplify_expression(*type_expr),
        Expression::If {
            condition,
            then_branch,
            else_branch,
            span,
        } => Ok(Expression::If {
            condition: Box::new(simplify_expression(*condition)?),
            then_branch: Box::new(simplify_expression(*then_branch)?),
            else_branch: match else_branch {
                Some(branch) => Some(Box::new(simplify_expression(*branch)?)),
                None => None,
            },
            span,
        }),
        Expression::Match {
            value,
            branches,
            span,
        } => Ok(Expression::Match {
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
            span,
        }),
        Expression::Function {
            parameter,
            return_type,
            body,
            span,
        } => Ok(Expression::Function {
            parameter: simplify_binding_pattern(parameter)?,
            return_type: Some(Box::new(simplify_expression(*return_type.unwrap())?)),
            body: Box::new(simplify_expression(*body)?),
            span,
        }),
        Expression::FunctionType {
            parameter,
            return_type,
            span,
        } => Ok(Expression::FunctionType {
            parameter: Box::new(simplify_expression(*parameter)?),
            return_type: Box::new(simplify_expression(*return_type)?),
            span,
        }),
        Expression::Loop { body, span } => Ok(Expression::Loop {
            body: Box::new(simplify_expression(*body)?),
            span,
        }),
        Expression::Struct(items, source_span) => {
            let simplified_items = items
                .into_iter()
                .map(|(id, expr)| Ok((id, simplify_expression(expr)?)))
                .collect::<Result<_, Diagnostic>>()?;
            Ok(Expression::Struct(simplified_items, source_span))
        }
        Expression::Operation {
            operator,
            left,
            right,
            span,
        } => {
            if let Some(intrinsic) = intrinsic_operator(&operator) {
                Ok(Expression::IntrinsicOperation(
                    IntrinsicOperation::Binary(
                        Box::new(simplify_expression(*left)?),
                        Box::new(simplify_expression(*right)?),
                        intrinsic,
                    ),
                    span,
                ))
            } else {
                Ok(Expression::FunctionCall {
                    function: Box::new(Expression::PropertyAccess {
                        object: Box::new(simplify_expression(*left)?),
                        property: operator,
                        span,
                    }),
                    argument: Box::new(simplify_expression(*right)?),
                    span,
                })
            }
        }
        Expression::FunctionCall {
            function,
            argument,
            span,
        } => Ok(Expression::FunctionCall {
            function: Box::new(simplify_expression(*function)?),
            argument: Box::new(simplify_expression(*argument)?),
            span,
        }),
        Expression::PropertyAccess {
            object,
            property,
            span,
        } => Ok(Expression::PropertyAccess {
            object: Box::new(simplify_expression(*object)?),
            property,
            span,
        }),
        Expression::Diverge {
            value,
            divergance_type,
            span,
        } => Ok(Expression::Diverge {
            value: match value {
                Some(expr) => Some(Box::new(simplify_expression(*expr)?)),
                None => None,
            },
            divergance_type,
            span,
        }),
        Expression::EnumType(variants, span) => Ok(Expression::EnumType(
            variants
                .into_iter()
                .map(|(id, ty)| Ok((id, simplify_expression(ty)?)))
                .collect::<Result<_, Diagnostic>>()?,
            span,
        )),
        Expression::EnumAccess {
            enum_expr,
            variant,
            span,
        } => {
            let simplified_enum = simplify_expression(*enum_expr)?;
            if let Expression::EnumType(variants, _) = &simplified_enum
                && let Some((variant_index, (_id, payload_type))) = variants
                    .iter()
                    .enumerate()
                    .find(|(_, (id, _))| id.name == variant.name)
            {
                if let Expression::Struct(fields, _) = payload_type
                    && fields.is_empty()
                {
                    return Ok(Expression::EnumValue {
                        enum_type: Box::new(simplified_enum.clone()),
                        variant,
                        variant_index,
                        payload: None,
                        span,
                    });
                }

                return Ok(Expression::EnumConstructor {
                    enum_type: Box::new(simplified_enum.clone()),
                    variant,
                    variant_index,
                    payload_type: Box::new(payload_type.clone()),
                    span,
                });
            }

            Ok(Expression::EnumAccess {
                enum_expr: Box::new(simplified_enum),
                variant,
                span,
            })
        }
        Expression::EnumConstructor {
            enum_type,
            variant,
            variant_index,
            payload_type,
            span,
        } => Ok(Expression::EnumConstructor {
            enum_type: Box::new(simplify_expression(*enum_type)?),
            variant,
            variant_index,
            payload_type: Box::new(simplify_expression(*payload_type)?),
            span,
        }),
        Expression::EnumValue {
            enum_type,
            variant,
            variant_index,
            payload,
            span,
        } => Ok(Expression::EnumValue {
            enum_type: Box::new(simplify_expression(*enum_type)?),
            variant,
            variant_index,
            payload: match payload {
                Some(payload) => Some(Box::new(simplify_expression(*payload)?)),
                None => None,
            },
            span,
        }),
        Expression::Assignment { target, expr, span } => Ok(Expression::Assignment {
            target,
            expr: Box::new(simplify_expression(*expr)?),
            span,
        }),
        Expression::Binding(binding, source_span) => {
            let binding = Binding {
                pattern: simplify_binding_pattern(binding.pattern)?,
                ..*binding
            };
            Ok(Expression::Binding(Box::new(binding), source_span))
        }
        Expression::Block(expressions, source_span) => {
            let simplified_exprs: Vec<Expression> = expressions
                .into_iter()
                .map(simplify_expression)
                .collect::<Result<_, Diagnostic>>()?;

            Ok(Expression::Block(simplified_exprs, source_span))
        }
        expr @ (Expression::Identifier(..)
        | Expression::IntrinsicType(..)
        | Expression::Literal(..)) => Ok(expr),
    }
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
    if let Expression::Literal(ExpressionLiteral::Target(TargetLiteral::JSTarget), _) = target_expr
    {
    } else {
        panic!("expected js target in export annotation");
    }
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
    if let Expression::Literal(ExpressionLiteral::Target(TargetLiteral::WasmTarget), _) =
        target_expr
    {
    } else {
        panic!("expected wasm target in export annotation");
    }
    println!("Exported binding value: {:?}", exported_binding.value);
}
