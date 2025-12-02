use silk::parsing::{Expression, ExpressionLiteral};
use silk::test_support::evaluate_text_to_expression;

#[test]
fn match_selects_correct_branch() {
    let program = "
        let Option = enum { Some = i32, None = {} };
        let choose = (option: Option) => i32 (
            match option with (
                Option::Some(value) => value,
                Option::None => 0
            )
        );

        choose(Option::Some(5))
    ";

    let (expr, _) = evaluate_text_to_expression(program).unwrap_or_else(|err| {
        panic!(
            "Evaluation failed with error: {}",
            err.render_with_source(program)
        );
    });

    match expr {
        Expression::Literal(ExpressionLiteral::Number(value), _) => assert_eq!(value, 5),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn match_requires_exhaustive_or_else() {
    let program = "
        let Option = enum { Some = i32, None = {} };
        match Option::None with (
            Option::Some(_) => 1
        )
    ";

    let error = evaluate_text_to_expression(program).expect_err("expected missing match branch");
    assert!(
        error.message.contains("No match branches matched"),
        "Unexpected error: {}",
        error.message
    );
}
