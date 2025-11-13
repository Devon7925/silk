use crate::{
    diagnostics::{Diagnostic, SourceSpan},
    parsing::{
        BinaryIntrinsicOperator, Binding, BindingAnnotation, BindingPattern, Expression,
        ExpressionLiteral, Identifier, IntrinsicOperation, IntrinsicType, TargetLiteral,
    },
};

#[derive(Clone, Debug)]
pub enum BindingContext {
    Bound(Expression),
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
                BindingContext::Bound(value) => Some(AnnotatedBinding {
                    name: name.clone(),
                    annotations: annotations.clone(),
                    value: value.clone(),
                }),
                _ => None,
            })
            .collect()
    }
}

fn diagnostic(message: impl Into<String>, span: SourceSpan) -> Diagnostic {
    Diagnostic::new(message).with_span(span)
}

fn dummy_span() -> SourceSpan {
    SourceSpan::default()
}

fn identifier_expr(name: &str) -> Expression {
    Expression::Identifier(Identifier(name.to_string()), dummy_span())
}

fn intrinsic_type_expr(ty: IntrinsicType) -> Expression {
    Expression::IntrinsicType(ty, dummy_span())
}

#[cfg(test)]
fn literal_number_expr(value: i32) -> Expression {
    Expression::Literal(ExpressionLiteral::Number(value), dummy_span())
}

pub fn interpret_expression(
    expr: Expression,
    context: &mut Context,
) -> Result<Expression, Diagnostic> {
    match expr {
        expr @ (Expression::Literal(_, _) | Expression::IntrinsicType(_, _)) => Ok(expr),
        Expression::Identifier(identifier, span) => {
            if let Some(binding) = context.bindings.get(&identifier.0) {
                if let BindingContext::Bound(expr) = &binding.0 {
                    Ok(expr.clone())
                } else {
                    Ok(Expression::Identifier(identifier, span))
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
        Expression::Binding(binding, _) => interpret_binding(*binding, context),
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
            match function_value {
                Expression::Function {
                    parameter,
                    return_type: _,
                    body,
                    span: _,
                } => {
                    let mut call_context = context.clone();
                    bind_pattern_from_value(
                        parameter,
                        &argument_value,
                        &mut call_context,
                        Vec::new(),
                    )?;
                    interpret_expression(*body, &mut call_context)
                }
                _ => Err(diagnostic("Attempted to call a non-function value", span)),
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
        Expression::IntrinsicOperation(intrinsic_operation, span) => match intrinsic_operation {
            IntrinsicOperation::Binary(left, right, operator) => {
                let evaluated_left = interpret_expression(*left, context)?;
                let evaluated_right = interpret_expression(*right, context)?;
                if !is_resolved_constant(&evaluated_left) || !is_resolved_constant(&evaluated_right)
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
                interpret_numeric_intrinsic(
                    evaluated_left,
                    evaluated_right,
                    context,
                    span,
                    match operator {
                        BinaryIntrinsicOperator::Add => |l, r| l + r,
                        BinaryIntrinsicOperator::Subtract => |l, r| l - r,
                        BinaryIntrinsicOperator::Multiply => |l, r| l * r,
                        BinaryIntrinsicOperator::Divide => |l, r| l / r,
                    },
                )
            }
        },
        Expression::PropertyAccess {
            object,
            property,
            span,
        } => {
            let evaluated_object = interpret_expression(*object, context)?;
            if let Expression::Struct(items, _) = &evaluated_object {
                for (item_id, item_expr) in items {
                    if item_id.0 == property {
                        return Ok(item_expr.clone());
                    }
                }
            }

            let object_type = get_type_of_expression(&evaluated_object, context)?;
            let trait_prop = get_trait_prop_of_type(&object_type, &property, span)?;
            interpret_expression(
                Expression::FunctionCall {
                    function: Box::new(trait_prop),
                    argument: Box::new(evaluated_object),
                    span,
                },
                context,
            )
        }
    }
}

fn interpret_binding_pattern(parameter: BindingPattern, context: &mut Context) -> Result<BindingPattern, Diagnostic> {
    match parameter {
        pat @ BindingPattern::Identifier(..) => Ok(pat),
        BindingPattern::Struct(items, source_span) => {
            let mut interpreted_items = Vec::with_capacity(items.len());
            for (field_id, field_pattern) in items {
                let interpreted_field_pattern = interpret_binding_pattern(field_pattern, context)?;
                interpreted_items.push((field_id, interpreted_field_pattern));
            }
            Ok(BindingPattern::Struct(interpreted_items, source_span))
        }
        BindingPattern::TypeHint(binding_pattern, expression, source_span) => {
            let interpreted_pattern = interpret_binding_pattern(*binding_pattern, context)?;
            let interpreted_type = interpret_expression(*expression, context)?;
            Ok(BindingPattern::TypeHint(
                Box::new(interpreted_pattern),
                Box::new(interpreted_type),
                source_span,
            ))
        }
        BindingPattern::Annotated { annotations, pattern, span } => {
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
                BindingContext::Bound(value) => get_type_of_expression(&value, context),
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
        Expression::Operation { .. } => todo!(),
        Expression::Binding(..) => todo!(),
        Expression::Block(..) => todo!(),
        Expression::FunctionCall { .. } => todo!(),
        Expression::IntrinsicType(intrinsic_type, span) => match intrinsic_type {
            IntrinsicType::I32
            | IntrinsicType::Boolean
            | IntrinsicType::Target
            | IntrinsicType::Type => Ok(Expression::IntrinsicType(IntrinsicType::Type, *span)),
        },
        Expression::AttachImplementation { .. } => todo!(),
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
        Expression::Struct(..) => todo!(),
        Expression::FunctionType { span, .. } => {
            Ok(Expression::IntrinsicType(IntrinsicType::Type, *span))
        }
        Expression::IntrinsicOperation(..) => todo!(),
        Expression::PropertyAccess { .. } => todo!(),
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
        BindingPattern::Struct(pattern_items, span) => {
            let mut struct_items = Vec::with_capacity(pattern_items.len());
            for (field_identifier, field_pattern) in pattern_items {
                let field_type = get_type_of_binding_pattern(field_pattern, context)?;
                struct_items.push((field_identifier.clone(), field_type));
            }
            Ok(Expression::Struct(struct_items, *span))
        }
        BindingPattern::TypeHint(_, type_expr, _) => Ok(*type_expr.clone()),
        BindingPattern::Annotated { pattern, .. } => get_type_of_binding_pattern(pattern, context),
    }
}

fn get_trait_prop_of_type(
    value_type: &Expression,
    trait_prop: &str,
    span: SourceSpan,
) -> Result<Expression, Diagnostic> {
    match value_type {
        Expression::AttachImplementation {
            type_expr,
            implementation,
            ..
        } => {
            if let Expression::Struct(ref items, _) = **implementation {
                for (item_id, item_expr) in items {
                    if item_id.0 == trait_prop {
                        return Ok(item_expr.clone());
                    }
                }
            }

            get_trait_prop_of_type(type_expr, trait_prop, span)
        }
        _ => Err(diagnostic(
            format!(
                "Unsupported value type for `{}` operator lookup",
                trait_prop
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

    for expression in expressions {
        let value = match expression {
            Expression::Binding(binding, _) => interpret_binding(*binding, &mut block_context)?,
            other => interpret_expression(other, &mut block_context)?,
        };
        last_value = Some(value);
    }

    let value = last_value.ok_or_else(|| diagnostic("Cannot evaluate empty block", span))?;
    Ok((value, block_context))
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

fn interpret_binding(binding: Binding, context: &mut Context) -> Result<Expression, Diagnostic> {
    let value = interpret_expression(binding.expr, context)?;
    let bound_success = bind_pattern_from_value(binding.pattern, &value, context, Vec::new())?;
    Ok(Expression::Literal(
        ExpressionLiteral::Boolean(bound_success),
        dummy_span(),
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
        BindingPattern::Struct(pattern_items, _) => {
            for (_, field_pattern) in pattern_items {
                bind_pattern_blanks(field_pattern, context, Vec::new(), None)?;
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
) -> Result<bool, Diagnostic> {
    match pattern {
        BindingPattern::Identifier(identifier, _) => {
            context.bindings.insert(
                identifier.0,
                (
                    BindingContext::Bound(value.clone()),
                    passed_annotations.clone(),
                ),
            );
            Ok(true)
        }
        BindingPattern::Struct(pattern_items, span) => {
            let Expression::Struct(struct_items, _) = value else {
                return Err(diagnostic("Struct pattern requires struct value", span));
            };

            for (field_identifier, field_pattern) in pattern_items {
                let field_span = field_pattern.span();
                let field_value = struct_items
                    .iter()
                    .find(|(value_identifier, _)| value_identifier.0 == field_identifier.0)
                    .map(|(_, expr)| expr)
                    .ok_or_else(|| {
                        diagnostic(format!("Missing field {}", field_identifier.0), field_span)
                    })?;

                bind_pattern_from_value(field_pattern, field_value, context, Vec::new())?;
            }

            Ok(true)
        }
        BindingPattern::TypeHint(inner, _, _) => {
            bind_pattern_from_value(*inner, value, context, passed_annotations)
        }
        BindingPattern::Annotated {
            pattern,
            annotations,
            ..
        } => bind_pattern_from_value(
            *pattern,
            value,
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
        ),
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

fn evaluate_numeric_operand(expr: Expression, context: &mut Context) -> Result<i32, Diagnostic> {
    let operand_span = expr.span();
    let evaluated = interpret_expression(expr, context)?;
    match evaluated {
        Expression::Literal(ExpressionLiteral::Number(value), _) => Ok(value),
        _ => Err(diagnostic(
            "Expected numeric literal during intrinsic operation",
            operand_span,
        )),
    }
}

fn is_resolved_constant(expr: &Expression) -> bool {
    match expr {
        Expression::Literal(_, _) | Expression::IntrinsicType(_, _) => true,
        Expression::Struct(items, _) => items
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
            BindingContext::Bound(Expression::AttachImplementation {
                type_expr: Box::new(intrinsic_type_expr(IntrinsicType::I32)),
                implementation: Box::new(Expression::Struct(
                    vec![
                        i32_binary_intrinsic("+", BinaryIntrinsicOperator::Add),
                        i32_binary_intrinsic("-", BinaryIntrinsicOperator::Subtract),
                        i32_binary_intrinsic("*", BinaryIntrinsicOperator::Multiply),
                        i32_binary_intrinsic("/", BinaryIntrinsicOperator::Divide),
                    ],
                    dummy_span(),
                )),
                span: dummy_span(),
            }),
            Vec::new(),
        ),
    );

    context.bindings.insert(
        "js".to_string(),
        (
            BindingContext::Bound(Expression::Literal(
                ExpressionLiteral::Target(TargetLiteral::JSTarget),
                dummy_span(),
            )),
            Vec::new(),
        ),
    );

    context.bindings.insert(
        "wasm".to_string(),
        (
            BindingContext::Bound(Expression::Literal(
                ExpressionLiteral::Target(TargetLiteral::WasmTarget),
                dummy_span(),
            )),
            Vec::new(),
        ),
    );

    context
}

#[cfg(test)]
fn evaluate_text_to_expression(program: &str) -> Result<(Expression, Context), Diagnostic> {
    let (expression, remaining) =
        crate::parsing::parse_block(program).expect("Failed to parse program text");
    assert!(
        remaining.trim().is_empty(),
        "Parser did not consume entire input, remaining: {remaining:?}"
    );

    let mut context = intrinsic_context();
    interpret_program(expression, &mut context)
}

#[cfg(test)]
fn evaluate_text_to_number(program: &str) -> i32 {
    match evaluate_text_to_expression(program)
        .expect("Failed to interpret parsed expression")
        .0
    {
        Expression::Literal(ExpressionLiteral::Number(value), _) => value,
        other => panic!(
            "Expected numeric literal after interpretation, got {:?}",
            other
        ),
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
