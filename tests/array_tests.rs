use silk::syntax::{ExpressionKind, ExpressionLiteral};
use silk::test_support::evaluate_text_to_expression;

#[test]
fn array_indexing_with_mutable_update() {
    let program = "
        mut nums := {1, 2, 3};
        idx := 1;
        nums(idx) = nums(idx) + 4;
        nums(idx)
    ";

    let (expr, _) = evaluate_text_to_expression(program).unwrap_or_else(|err| {
        panic!(
            "Evaluation failed with error: {}",
            err.render_with_source(program)
        )
    });

    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => assert_eq!(value, 6),
        other => panic!("Expected array index expression, got {:?}", other),
    }
}

