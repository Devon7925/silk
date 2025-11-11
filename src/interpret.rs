use crate::{interpret, parsing::{
    BinaryIntrinsicOperator, Binding, BindingPattern, Expression, ExpressionLiteral, Identifier,
    IntrinsicOperation, IntrinsicType,
}};

#[derive(Clone, Debug)]
pub enum BindingContext {
    Bound(Expression),
    UnboundWithType(Expression),
    UnboundWithoutType,
}

#[derive(Clone)]
pub struct Context {
    bindings: std::collections::HashMap<String, BindingContext>,
}

pub fn interpret_expression(expr: Expression, context: &mut Context) -> Result<Expression, String> {
    match expr {
        expr @ (Expression::Literal(_) | Expression::IntrinsicType(_)) => Ok(expr),
        Expression::Identifier(identifier) => {
            if let Some(binding) = context.bindings.get(&identifier.0) {
                if let BindingContext::Bound(expr) = binding {
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
        } => interpret_expression(
            Expression::FunctionCall {
                function: Box::new(Expression::PropertyAccess {
                    object: left,
                    property: operator.clone(),
                }),
                argument: right,
            },
            context,
        ),
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

            bind_pattern_blanks(parameter.clone(), &mut body_context, None)?;

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
        Expression::IntrinsicType(intrinsic_type) => todo!(),
        Expression::Literal(expression_literal) => todo!(),
        Expression::PropertyAccess { object, property } => {
            let evaluated_object = interpret_expression(*object, context)?;
            if let Expression::Struct(items) = &evaluated_object {
                for (item_id, item_expr) in items {
                    if item_id.0 == property {
                        return Ok(item_expr.clone());
                    }
                }
            }

            let object_type = get_type_of_expression(&evaluated_object, context)?;
            let trait_prop = get_trait_prop_of_type(&object_type, &property)?;
            interpret_expression(
                Expression::FunctionCall {
                    function: Box::new(trait_prop),
                    argument: Box::new(evaluated_object),
                },
                context,
            )
        }
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
            let bound_value = context.bindings
                .get(&identifier.0)
                .ok_or_else(|| format!("Unbound identifier: {}", identifier.0))?
                .clone();
            
            match bound_value {
                BindingContext::Bound(value) => get_type_of_expression(&value, context),
                BindingContext::UnboundWithType(type_expr) => Ok(interpret_expression(type_expr.clone(), context)?),
                BindingContext::UnboundWithoutType => Err(format!("Cannot determine type of unbound identifier: {}", identifier.0)),
            }
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
            parameter: Box::new(get_type_of_binding_pattern(&parameter, context)?),
            return_type: return_type.clone(),
        }),
        Expression::Struct(items) => todo!(),
        Expression::FunctionType {
            parameter,
            return_type,
        } => Ok(Expression::IntrinsicType(IntrinsicType::Type)),
        Expression::IntrinsicOperation(intrinsic_operation) => todo!(),
        Expression::PropertyAccess { object, property } => todo!(),
    }
}

fn get_type_of_binding_pattern(
    pattern: &BindingPattern,
    context: &mut Context,
) -> Result<Expression, String> {
    match pattern {
        BindingPattern::Identifier(_) => {
            Err("Cannot determine type of untyped identifier".to_string())
        }
        BindingPattern::Struct(pattern_items) => {
            let mut struct_items = Vec::with_capacity(pattern_items.len());
            for (field_identifier, field_pattern) in pattern_items {
                let field_type = get_type_of_binding_pattern(field_pattern, context)?;
                struct_items.push((field_identifier.clone(), field_type));
            }
            Ok(Expression::Struct(struct_items))
        }
        BindingPattern::TypeHint(_, type_expr) => Ok(*type_expr.clone()),
    }
}

fn get_trait_prop_of_type(value_type: &Expression, trait_prop: &str) -> Result<Expression, String> {
    match value_type {
        Expression::AttachImplementation {
            type_expr,
            implementation,
        } => {
            if let Expression::Struct(ref items) = **implementation {
                for (item_id, item_expr) in items {
                    if item_id.0 == trait_prop {
                        return Ok(item_expr.clone());
                    }
                }
            }

            get_trait_prop_of_type(type_expr, trait_prop)
        }
        unrecognized => Err(format!("Unsupported value type {:?} for operator lookup", unrecognized)),
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

fn bind_pattern_blanks(pattern: BindingPattern, context: &mut Context, type_hint: Option<Expression>) -> Result<(), String> {
    match pattern {
        BindingPattern::Identifier(identifier) => {
            if let Some(type_expr) = type_hint {
                context.bindings.insert(identifier.0, BindingContext::UnboundWithType(type_expr));
            } else {
                context.bindings.insert(identifier.0, BindingContext::UnboundWithoutType);
            }
            Ok(())
        }
        BindingPattern::Struct(pattern_items) => {
            for (_, field_pattern) in pattern_items {
                bind_pattern_blanks(field_pattern, context, None)?;
            }

            Ok(())
        }
        BindingPattern::TypeHint(inner, type_hint) => bind_pattern_blanks(*inner, context, Some(*type_hint)),
    }
}

fn bind_pattern_from_value(
    pattern: BindingPattern,
    value: &Expression,
    context: &mut Context,
) -> Result<(), String> {
    match pattern {
        BindingPattern::Identifier(identifier) => {
            context.bindings.insert(identifier.0, BindingContext::Bound(value.clone()));
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
                    .map(|(_, expr)| expr)
                    .ok_or_else(|| format!("Missing field {}", field_identifier.0))?;

                bind_pattern_from_value(field_pattern, field_value, context)?;
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

pub fn intrinsic_context() -> Context {
    let mut context = Context {
        bindings: std::collections::HashMap::new(),
    };

    fn i32_binary_intrinsic(
        symbol: &str,
        operator: BinaryIntrinsicOperator,
    ) -> (Identifier, Expression) {
        let typed_pattern = |name: &str| {
            BindingPattern::TypeHint(
                Box::new(BindingPattern::Identifier(Identifier(name.to_string()))),
                Box::new(Expression::IntrinsicType(IntrinsicType::I32)),
            )
        };
        let identifier_expr = |name: &str| Expression::Identifier(Identifier(name.to_string()));

        (
            Identifier(symbol.to_string()),
            Expression::Function {
                parameter: typed_pattern("self"),
                return_type: Box::new(Expression::FunctionType {
                    parameter: Box::new(Expression::IntrinsicType(IntrinsicType::I32)),
                    return_type: Box::new(Expression::IntrinsicType(IntrinsicType::I32)),
                }),
                body: Box::new(Expression::Function {
                    parameter: typed_pattern("other"),
                    return_type: Box::new(Expression::IntrinsicType(IntrinsicType::I32)),
                    body: Box::new(Expression::IntrinsicOperation(IntrinsicOperation::Binary(
                        Box::new(identifier_expr("self")),
                        Box::new(identifier_expr("other")),
                        operator,
                    ))),
                }),
            },
        )
    }

    context.bindings.insert(
        "i32".to_string(),
        BindingContext::Bound(Expression::AttachImplementation {
            type_expr: Box::new(Expression::IntrinsicType(IntrinsicType::I32)),
            implementation: Box::new(Expression::Struct(vec![
                i32_binary_intrinsic("+", BinaryIntrinsicOperator::Add),
                i32_binary_intrinsic("-", BinaryIntrinsicOperator::Subtract),
                i32_binary_intrinsic("*", BinaryIntrinsicOperator::Multiply),
                i32_binary_intrinsic("/", BinaryIntrinsicOperator::Divide),
            ])),
        }),
    );

    context
}

#[cfg(test)]
fn evaluate_text_to_number(program: &str) -> i32 {
    let (expression, remaining) =
        crate::parsing::parse_block(program).expect("Failed to parse program text");
    assert!(
        remaining.trim().is_empty(),
        "Parser did not consume entire input, remaining: {remaining:?}"
    );

    let mut context = intrinsic_context();
    match interpret_expression(expression, &mut context)
        .expect("Failed to interpret parsed expression")
    {
        Expression::Literal(ExpressionLiteral::Number(value)) => value,
        other => panic!(
            "Expected numeric literal after interpretation, got {:?}",
            other
        ),
    }
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
