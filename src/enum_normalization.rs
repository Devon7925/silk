use crate::parsing::Expression;

/// Normalize enum constructor applications so that accessing a variant after
/// applying a type argument is represented uniformly as `EnumAccess(FunctionCall)`.
///
/// For example, calls shaped like `Some<Option>(value)` are rewritten so the enum
/// access happens after the type application: `Option(value)::Some`.
pub fn normalize_enum_application(expr: Expression) -> Expression {
    match expr {
        Expression::FunctionCall {
            function,
            argument,
            span,
        } => {
            let normalized_function = Box::new(normalize_enum_application(*function));
            let normalized_argument = Box::new(normalize_enum_application(*argument));

            match *normalized_argument {
                Expression::EnumAccess {
                    enum_expr,
                    variant,
                    span: access_span,
                } => {
                    let enum_expr_span = enum_expr.span();
                    normalize_enum_application(Expression::EnumAccess {
                        enum_expr: Box::new(Expression::FunctionCall {
                            function: normalized_function,
                            argument: enum_expr,
                            span: span.merge(&enum_expr_span),
                        }),
                        variant,
                        span: access_span,
                    })
                }
                _ => Expression::FunctionCall {
                    function: normalized_function,
                    argument: normalized_argument,
                    span,
                },
            }
        }
        other => other,
    }
}
