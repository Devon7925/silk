use crate::interpret::Context;
use crate::{
    Diagnostic,
    interpret::BindingContext,
    parsing::{
        Binding, BindingAnnotation, BindingPattern, Expression, ExpressionLiteral,
        IntrinsicOperation, TargetLiteral,
    },
};

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
        Expression::AttachImplementation { type_expr, .. } => simplify_expression(*type_expr),
        Expression::Function {
            parameter,
            return_type,
            body,
            span,
        } => Ok(Expression::Function {
            parameter: simplify_binding_pattern(parameter)?,
            return_type: Box::new(simplify_expression(*return_type)?),
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
        Expression::Struct(items, source_span) => {
            let simplified_items = items
                .into_iter()
                .map(|(id, expr)| Ok((id, simplify_expression(expr)?)))
                .collect::<Result<_, Diagnostic>>()?;
            Ok(Expression::Struct(simplified_items, source_span))
        }
        Expression::Operation { span, .. } => Err(Diagnostic::new(format!(
            "Invalid state: uninterpreted operator expression",
        ))
        .with_span(span)),
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
        Expression::Binding(binding, source_span) => {
            let binding = Binding {
                pattern: simplify_binding_pattern(binding.pattern)?,
                ..*binding
            };
            Ok(Expression::Binding(Box::new(binding), source_span))
        }
        Expression::Block(expressions, source_span) => {
            let simplified_exprs = expressions
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

fn simplify_binding_pattern(pattern: BindingPattern) -> Result<BindingPattern, Diagnostic> {
    match pattern {
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
        BindingPattern::Annotated {
            annotations,
            pattern,
            span,
        } => Ok(BindingPattern::Annotated {
            annotations,
            pattern: Box::new(simplify_binding_pattern(*pattern)?),
            span,
        }),
        pat @ BindingPattern::Identifier(..) => Ok(pat),
    }
}

fn simplify_binding_context(binding_context: BindingContext) -> Result<BindingContext, Diagnostic> {
    match binding_context {
        BindingContext::Bound(expression) => {
            Ok(BindingContext::Bound(simplify_expression(expression)?))
        }
        BindingContext::BoundPreserved(expression) => Ok(BindingContext::BoundPreserved(
            simplify_expression(expression)?,
        )),
        BindingContext::UnboundWithType(expression) => Ok(BindingContext::UnboundWithType(
            simplify_expression(expression)?,
        )),
        BindingContext::UnboundWithoutType => Ok(BindingContext::UnboundWithoutType),
    }
}

pub fn simplify_context(context: Context) -> Result<Context, Diagnostic> {
    let simplified_bindings = context
        .bindings
        .into_iter()
        .map(|(bind_name, (binding, annotations))| {
            Ok((bind_name, (simplify_binding_context(binding)?, annotations)))
        })
        .collect::<Result<_, Diagnostic>>()?;
    Ok(Context {
        bindings: simplified_bindings,
    })
}

#[cfg(test)]
fn evaluate_text_to_simplified_expression(
    program: &str,
) -> Result<(Expression, Context), Diagnostic> {
    use crate::interpret::{interpret_program, intrinsic_context};
    use crate::parsing::{BindingAnnotation, ExpressionLiteral, TargetLiteral};

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
let export(js) add_one = fn(x: i32) -> i32 (
    x + 1
);
{}
    ";
    let (_result, context) =
        evaluate_text_to_simplified_expression(&program).expect("interpretation should succeed");
    let annotated_bindings = context.annotated_bindings();
    assert_eq!(annotated_bindings.len(), 1);
    let exported_binding = &annotated_bindings[0];
    assert_eq!(exported_binding.name, "add_one");
    assert!(exported_binding.annotations.len() == 1);
    let BindingAnnotation::Export(target_expr, _) = &exported_binding.annotations[0];
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
    assert_eq!(exported_binding.name, "add_one_squared");
    assert!(exported_binding.annotations.len() == 1);
    let BindingAnnotation::Export(target_expr, _) = &exported_binding.annotations[0];
    if let Expression::Literal(ExpressionLiteral::Target(TargetLiteral::WasmTarget), _) =
        target_expr
    {
    } else {
        panic!("expected wasm target in export annotation");
    }
    println!("Exported binding value: {:?}", exported_binding.value);
}
