use silk::syntax::{ExpressionKind, ExpressionLiteral};
use silk::test_support::evaluate_text_to_expression;

#[test]
fn generic_enum_option_t() {
    let program = "
unwrap_or_zero := (x: i32) => (
    val: Option(i32) := if x > 0 then Option(i32)::Some(x) else Option(i32)::None;

    if Option(i32)::Some(v) := val then v else 0
);
unwrap_or_zero(5) + unwrap_or_zero(-3)
    ";

    let (expr, _) = evaluate_text_to_expression(program).unwrap_or_else(|err| {
        panic!(
            "Evaluation failed with error: {}",
            err.render_with_source(program)
        );
    });

    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => assert_eq!(value, 5),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn generic_enum_nested_generics() {
    let program = "
Container := (T: type) => (
    enum { Wrapped = Option(T), Empty = {} }
);

check := (x: i32) => (
    c := Container(i32)::Wrapped(Option(i32)::Some(x));
    
    if Container(i32)::Wrapped(opt) := c then (
        if Option(i32)::Some(val) := opt then val else 0
    ) else 0
);
check(10) + check(-5)
    ";

    let (expr, _) = evaluate_text_to_expression(program).unwrap_or_else(|err| {
        panic!(
            "Evaluation failed with error: {}",
            err.render_with_source(program)
        );
    });

    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => assert_eq!(value, 5),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

