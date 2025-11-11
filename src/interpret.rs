use crate::parsing::{
    BinaryIntrinsicOperator, Binding, BindingPattern, Expression, ExpressionLiteral, Identifier,
    IntrinsicOperation, IntrinsicType,
};

#[derive(Clone)]
struct Context {
    bindings: std::collections::HashMap<String, Option<Expression>>,
}

fn interpret_expression(expr: Expression, context: &mut Context) -> Result<Expression, String> {
    match expr {
        expr @ (Expression::Literal(_) | Expression::IntrinsicType(_)) => Ok(expr),
        Expression::Identifier(identifier) => {
            if let Some(binding) = context.bindings.get(&identifier.0) {
                if let Some(expr) = binding {
                    Ok(expr.clone())
                } else {
                    Ok(Expression::Identifier(identifier))
                }
            } else {
                Err(format!("Unbound identifier: {}", identifier.0))
            }
        }
        Expression::Operation {
            operator,
            left,
            right,
        } => {
            let left_evaluated = interpret_expression(*left, context)?;
            let right_evaluated = interpret_expression(*right, context)?;
            let left_type = get_type_of_expression(&left_evaluated, context)?;

            let operator_type = get_operator_of_type(&left_type, &operator)?;
            interpret_expression(
                Expression::FunctionCall {
                    function: Box::new(Expression::FunctionCall {
                        function: Box::new(operator_type),
                        argument: Box::new(left_evaluated),
                    }),
                    argument: Box::new(right_evaluated),
                },
                context,
            )
        }
        Expression::Binding(binding) => interpret_binding(*binding, context),
        Expression::Block(expressions) => interpret_block(expressions, context),
        Expression::FunctionCall { function, argument } => {
            let function_value = interpret_expression(*function, context)?;
            let argument_value = interpret_expression(*argument, context)?;
            match function_value {
                Expression::Function {
                    parameter,
                    return_type: _,
                    body,
                } => {
                    let mut call_context = context.clone();
                    bind_pattern_from_value(parameter, &argument_value, &mut call_context)?;
                    interpret_expression(*body, &mut call_context)
                }
                _ => Err("Attempted to call a non-function value".to_string()),
            }
        }
        Expression::AttachImplementation {
            type_expr,
            implementation,
        } => Ok(Expression::AttachImplementation {
            type_expr: Box::new(interpret_expression(*type_expr, context)?),
            implementation: Box::new(interpret_expression(*implementation, context)?),
        }),
        Expression::Function {
            parameter,
            return_type,
            body,
        } => {
            let mut body_context = context.clone();

            bind_pattern_blanks(parameter.clone(), &mut body_context)?;

            Ok(Expression::Function {
                parameter,
                return_type,
                body: Box::new(interpret_expression(*body, &mut body_context)?),
            })
        }
        Expression::Struct(items) => {
            let mut evaluated_items = Vec::with_capacity(items.len());
            for (identifier, value_expr) in items {
                let evaluated_value = interpret_expression(value_expr, context)?;
                evaluated_items.push((identifier, evaluated_value));
            }
            Ok(Expression::Struct(evaluated_items))
        }
        Expression::FunctionType {
            parameter,
            return_type,
        } => Ok(Expression::FunctionType {
            parameter,
            return_type,
        }),
        Expression::IntrinsicOperation(intrinsic_operation) => match intrinsic_operation {
            IntrinsicOperation::Binary(left, right, operator) => {
                let evaluated_left = interpret_expression(*left, context)?;
                let evaluated_right = interpret_expression(*right, context)?;
                if !is_resolved_constant(&evaluated_left) || !is_resolved_constant(&evaluated_right)
                {
                    return Ok(Expression::IntrinsicOperation(IntrinsicOperation::Binary(
                        Box::new(evaluated_left),
                        Box::new(evaluated_right),
                        operator,
                    )));
                }
                interpret_numeric_intrinsic(
                    evaluated_left,
                    evaluated_right,
                    context,
                    match operator {
                        BinaryIntrinsicOperator::Add => |l, r| l + r,
                        BinaryIntrinsicOperator::Subtract => |l, r| l - r,
                        BinaryIntrinsicOperator::Multiply => |l, r| l * r,
                        BinaryIntrinsicOperator::Divide => |l, r| l / r,
                    },
                )
            }
        },
    }
}

fn get_type_of_expression(expr: &Expression, context: &mut Context) -> Result<Expression, String> {
    match expr {
        Expression::Literal(lit) => match lit {
            ExpressionLiteral::Number(_) => Ok(interpret_expression(
                Expression::Identifier(Identifier("i32".to_string())),
                context,
            )?),
        },
        Expression::Identifier(identifier) => {
            interpret_expression(Expression::Identifier(identifier.clone()), context)
        }
        Expression::Operation {
            operator,
            left,
            right,
        } => todo!(),
        Expression::Binding(binding) => todo!(),
        Expression::Block(expressions) => todo!(),
        Expression::FunctionCall { function, argument } => todo!(),
        Expression::IntrinsicType(intrinsic_type) => match intrinsic_type {
            IntrinsicType::I32 | IntrinsicType::Type => {
                Ok(Expression::IntrinsicType(IntrinsicType::Type))
            }
        },
        Expression::AttachImplementation {
            type_expr,
            implementation,
        } => todo!(),
        Expression::Function {
            parameter,
            return_type,
            body,
        } => Ok(Expression::FunctionType {
            parameter: parameter.clone(),
            return_type: return_type.clone(),
        }),
        Expression::Struct(items) => todo!(),
        Expression::FunctionType {
            parameter,
            return_type,
        } => Ok(Expression::IntrinsicType(IntrinsicType::Type)),
        Expression::IntrinsicOperation(intrinsic_operation) => todo!(),
    }
}

fn get_operator_of_type(value_type: &Expression, operator: &str) -> Result<Expression, String> {
    match value_type {
        Expression::AttachImplementation {
            type_expr,
            implementation,
        } => {
            if let Expression::Struct(ref items) = **implementation {
                for (item_id, item_expr) in items {
                    if item_id.0 == operator {
                        return Ok(item_expr.clone());
                    }
                }
            }

            get_operator_of_type(type_expr, operator)
        }
        _ => Err("Unsupported value type for operator lookup".to_string()),
    }
}

fn interpret_block(
    expressions: Vec<Expression>,
    context: &mut Context,
) -> Result<Expression, String> {
    let mut block_context = context.clone();
    let mut last_value: Option<Expression> = None;

    for expression in expressions {
        let value = match expression {
            Expression::Binding(binding) => interpret_binding(*binding, &mut block_context)?,
            other => interpret_expression(other, &mut block_context)?,
        };
        last_value = Some(value);
    }

    last_value.ok_or_else(|| "Cannot evaluate empty block".to_string())
}

fn interpret_binding(binding: Binding, context: &mut Context) -> Result<Expression, String> {
    let value = interpret_expression(binding.expr, context)?;
    bind_pattern_from_value(binding.pattern, &value, context)?;
    Ok(value)
}

fn bind_pattern_blanks(pattern: BindingPattern, context: &mut Context) -> Result<(), String> {
    match pattern {
        BindingPattern::Identifier(identifier) => {
            context.bindings.insert(identifier.0, None);
            Ok(())
        }
        BindingPattern::Struct(pattern_items) => {
            for (field_identifier, field_pattern) in pattern_items {
                bind_pattern_blanks(field_pattern, context)?;
            }

            Ok(())
        }
        BindingPattern::TypeHint(inner, _) => bind_pattern_blanks(*inner, context),
    }
}

fn bind_pattern_from_value(
    pattern: BindingPattern,
    value: &Expression,
    context: &mut Context,
) -> Result<(), String> {
    match pattern {
        BindingPattern::Identifier(identifier) => {
            context.bindings.insert(identifier.0, Some(value.clone()));
            Ok(())
        }
        BindingPattern::Struct(pattern_items) => {
            let Expression::Struct(struct_items) = value else {
                return Err("Struct pattern requires struct value".to_string());
            };

            for (field_identifier, field_pattern) in pattern_items {
                let field_value = struct_items
                    .iter()
                    .find(|(value_identifier, _)| value_identifier.0 == field_identifier.0)
                    .map(|(_, expr)| expr.clone())
                    .ok_or_else(|| format!("Missing field {}", field_identifier.0))?;

                bind_pattern_from_value(field_pattern, &field_value, context)?;
            }

            Ok(())
        }
        BindingPattern::TypeHint(inner, _) => bind_pattern_from_value(*inner, value, context),
    }
}

fn interpret_numeric_intrinsic<F>(
    left: Expression,
    right: Expression,
    context: &mut Context,
    op: F,
) -> Result<Expression, String>
where
    F: Fn(i32, i32) -> i32,
{
    let left_value = evaluate_numeric_operand(left, context)?;
    let right_value = evaluate_numeric_operand(right, context)?;
    Ok(Expression::Literal(ExpressionLiteral::Number(op(
        left_value,
        right_value,
    ))))
}

fn evaluate_numeric_operand(expr: Expression, context: &mut Context) -> Result<i32, String> {
    let evaluated = interpret_expression(expr, context)?;
    match evaluated {
        Expression::Literal(ExpressionLiteral::Number(value)) => Ok(value),
        expr => Err(format!(
            "Expected numeric literal during intrinsic operation, got {:?}",
            expr
        )),
    }
}

fn is_resolved_constant(expr: &Expression) -> bool {
    match expr {
        Expression::Literal(_) | Expression::IntrinsicType(_) => true,
        Expression::Struct(items) => items
            .iter()
            .all(|(_, value_expr)| is_resolved_constant(value_expr)),
        _ => false,
    }
}

fn intrinsic_context() -> Context {
    let mut context = Context {
        bindings: std::collections::HashMap::new(),
    };

    context.bindings.insert(
        "i32".to_string(),
        Some(Expression::AttachImplementation {
            type_expr: Box::new(Expression::IntrinsicType(IntrinsicType::I32)),
            implementation: Box::new(Expression::Struct(vec![(
                Identifier("+".to_string()),
                Expression::Function {
                    parameter: BindingPattern::TypeHint(
                        Box::new(BindingPattern::Identifier(Identifier("self".to_string()))),
                        Box::new(Expression::IntrinsicType(IntrinsicType::I32)),
                    ),
                    return_type: Box::new(Expression::FunctionType {
                        parameter: BindingPattern::TypeHint(
                            Box::new(BindingPattern::Identifier(Identifier("other".to_string()))),
                            Box::new(Expression::IntrinsicType(IntrinsicType::I32)),
                        ),
                        return_type: Box::new(Expression::IntrinsicType(IntrinsicType::I32)),
                    }),
                    body: Box::new(Expression::Function {
                        parameter: BindingPattern::TypeHint(
                            Box::new(BindingPattern::Identifier(Identifier("other".to_string()))),
                            Box::new(Expression::IntrinsicType(IntrinsicType::I32)),
                        ),
                        return_type: Box::new(Expression::IntrinsicType(IntrinsicType::I32)),
                        body: Box::new(Expression::IntrinsicOperation(IntrinsicOperation::Binary(
                            Box::new(Expression::Identifier(Identifier("self".to_string()))),
                            Box::new(Expression::Identifier(Identifier("other".to_string()))),
                            BinaryIntrinsicOperator::Add,
                        ))),
                    }),
                },
            )])),
        }),
    );

    context
}

#[test]
fn test_basic_binding_interpretation() {
    let mut context = intrinsic_context();

    let expr = Expression::Block(vec![
        Expression::Binding(Box::new(Binding {
            pattern: BindingPattern::TypeHint(
                Box::new(BindingPattern::Identifier(Identifier("x".to_string()))),
                Box::new(Expression::IntrinsicType(IntrinsicType::I32)),
            ),
            expr: Expression::Literal(ExpressionLiteral::Number(5)),
        })),
        Expression::Identifier(Identifier("x".to_string())),
    ]);

    let result = interpret_expression(expr, &mut context).unwrap();

    if let Expression::Literal(ExpressionLiteral::Number(value)) = result {
        assert_eq!(value, 5);
    } else {
        panic!("Expected a number literal as result");
    }
}

#[test]
fn test_basic_addition_interpretation() {
    let mut context = intrinsic_context();

    let expr = Expression::Operation {
        operator: "+".to_string(),
        left: Box::new(Expression::Literal(ExpressionLiteral::Number(5))),
        right: Box::new(Expression::Literal(ExpressionLiteral::Number(10))),
    };

    let result = interpret_expression(expr, &mut context).unwrap();

    if let Expression::Literal(ExpressionLiteral::Number(value)) = result {
        assert_eq!(value, 15);
    } else {
        panic!("Expected a number literal as result");
    }
}
