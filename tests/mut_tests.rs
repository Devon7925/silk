use silk::parsing::{ExpressionKind, ExpressionLiteral};
use silk::test_support::evaluate_text_to_expression;

#[test]
fn nested_mutation() {
    let program = "
    (export wasm) destructure_mut := (bar: i32) => (
        mut foo := { first = {bar, 3}, second = 4 };
        foo.first.0 = foo.first.0 + foo.second;
        foo.first.0
    );
    destructure_mut(5)
    ";

    let (expr, _) = evaluate_text_to_expression(program).unwrap_or_else(|err| {
        panic!(
            "Evaluation failed with error: {}",
            err.render_with_source(program)
        );
    });

    match &expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => assert_eq!(*value, 9),
        other => panic!("Expected numeric literal, got {:?}", other),
    };
}
