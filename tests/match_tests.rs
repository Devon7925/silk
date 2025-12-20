use silk::parsing::{ExpressionKind, ExpressionLiteral};
use silk::test_support::evaluate_text_to_expression;

#[test]
fn match_selects_correct_branch() {
    let program = "
        Option := enum { Some = i32, None = {} };
        choose := (option: Option) => (
            option |> match {
                Option::Some(value) => value,
                Option::None => 0
            }
        );

        choose(Option::Some(5))
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
fn match_allows_literal_branch() {
    let program = "
        Option := enum { Some = i32, None = {} };
        choose := (option: Option) => (
            option |> match {
                Option::Some(value) => value,
                else => 0
            }
        );

        choose(Option::Some(5))
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
fn match_requires_exhaustive_or_else() {
    let program = "
        Option := enum { Some = i32, None = {} };
        Option::None |> match {
            Option::Some(_) => 1
        }
    ";

    let error = evaluate_text_to_expression(program).expect_err("expected missing match branch");
    assert!(
        error.message.contains("No match branches matched"),
        "Unexpected error: {}",
        error.message
    );
}
