use crate::parsing::{
    Binding, BindingPattern, Expression, ExpressionLiteral, Identifier, IntrinsicOperation, IntrinsicType
};

struct Context {
    bindings: std::collections::HashMap<String, Expression>,
}

fn interpret_expression(expr: Expression, context: &Context) -> Result<Expression, String> {
    match expr {
        expr @ (Expression::Literal(_) | Expression::IntrinsicType(_)) => Ok(expr),
        Expression::Identifier(identifier) => {
            if let Some(binding) = context.bindings.get(&identifier.0) {
                Ok(binding.clone())
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

            let operator_type = get_operator_of_type(&left_type, context, &operator)?;
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
        Expression::Binding(binding) => todo!(),
        Expression::Block(expressions) => todo!(),
        Expression::FunctionCall { function, argument } => todo!(),
        Expression::IntrinsicType(intrinsic_type) => todo!(),
        Expression::AttachImplementation {
            type_expr,
            implementation,
        } => todo!(),
        Expression::Function {
            parameter,
            return_type,
            body,
        } => todo!(),
        Expression::Struct(items) => todo!(),
        Expression::Literal(expression_literal) => todo!(),
        Expression::FunctionType {
            parameter,
            return_type,
        } => todo!(),
        Expression::IntrinsicOperation(intrinsic_operation) => todo!(),
    }
}

fn get_type_of_expression(expr: &Expression, context: &Context) -> Result<Expression, String> {
    match expr {
        Expression::Literal(lit) => match lit {
            ExpressionLiteral::Number(_) => Ok(interpret_expression(
                Expression::Identifier(Identifier("i32".to_string())),
                context,
            )?),
        },
        Expression::Identifier(identifier) => todo!(),
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

fn get_operator_of_type(
    value_type: &Expression,
    context: &Context,
    operator: &str,
) -> Result<Expression, String> {
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

            get_operator_of_type(type_expr, context, operator)
        }
        _ => Err("Unsupported value type for operator lookup".to_string()),
    }
}

fn intrinsic_context() -> Context {
    let mut context = Context {
        bindings: std::collections::HashMap::new(),
    };

    context.bindings.insert(
        "i32".to_string(),
        Expression::AttachImplementation {
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
                    body: Box::new(Expression::IntrinsicOperation(IntrinsicOperation::Add(
                        Box::new(Expression::Identifier(Identifier("self".to_string()))),
                        Box::new(Expression::Identifier(Identifier("other".to_string()))),
                    ))),
                },
            )])),
        },
    );

    context
}


#[test]
fn test_basic_binding_interpretation() {
    let context = intrinsic_context();

    let expr = Expression::Block(vec![
        Expression::Binding(Box::new(Binding {
            pattern: BindingPattern::TypeHint(
                Box::new(BindingPattern::Identifier(Identifier("x".to_string()))),
                Box::new(Expression::IntrinsicType(IntrinsicType::I32)),
            ),
            expr: Expression::Literal(ExpressionLiteral::Number(5)),
        })),
        Expression::Identifier(Identifier("x".to_string()))
    ]);

    let result = interpret_expression(expr, &context).unwrap();

    if let Expression::Literal(ExpressionLiteral::Number(value)) = result {
        assert_eq!(value, 5);
    } else {
        panic!("Expected a number literal as result");
    }
}

#[test]
fn test_basic_addition_interpretation() {
    let context = intrinsic_context();

    let expr = Expression::Operation {
        operator: "+".to_string(),
        left: Box::new(Expression::Literal(ExpressionLiteral::Number(5))),
        right: Box::new(Expression::Literal(ExpressionLiteral::Number(10))),
    };

    let result = interpret_expression(expr, &context).unwrap();

    if let Expression::Literal(ExpressionLiteral::Number(value)) = result {
        assert_eq!(value, 15);
    } else {
        panic!("Expected a number literal as result");
    }
}
