use silk::parsing::{Expression, ExpressionLiteral};
use silk::test_support::evaluate_text_to_expression;

#[test]
fn match_selects_correct_branch() {
    let program = "
        let Option = enum { Some = i32, None = {} };
        let choose = fn(option: Option) -> i32 (
            match option {
                Option::Some(value) => ( value );
                Option::None => ( 0 );
            }
        );

        choose Option::Some(5)
    ";

    let (expr, _) = evaluate_text_to_expression(program).unwrap();

    match expr {
        Expression::Literal(ExpressionLiteral::Number(value), _) => assert_eq!(value, 5),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn match_requires_exhaustive_or_else() {
    let program = "
        let Option = enum { Some = i32, None = {} };
        match Option::None {
            Option::Some(_) => ( 1 );
        }
    ";

    let error = evaluate_text_to_expression(program).expect_err("expected missing match branch");
    assert!(
        error.message.contains("No match branches matched"),
        "Unexpected error: {}",
        error.message
    );
}
