use silk::parsing::{Expression, ExpressionLiteral};
use silk::test_support::evaluate_text_to_expression;

fn evaluate_text_to_bool(program: &str) -> bool {
    match evaluate_text_to_expression(program)
        .expect("Failed to interpret parsed expression")
        .0
    {
        Expression::Literal(ExpressionLiteral::Boolean(value), _) => value,
        val => panic!("Expected boolean result, got {:?}", val),
    }
}

#[test]
fn test_true_literal() {
    assert_eq!(evaluate_text_to_bool("true"), true);
}

#[test]
fn test_false_literal() {
    assert_eq!(evaluate_text_to_bool("false"), false);
}

#[test]
fn test_equality() {
    assert_eq!(evaluate_text_to_bool("5 == 5"), true);
    assert_eq!(evaluate_text_to_bool("5 == 6"), false);
}

#[test]
fn test_inequality() {
    assert_eq!(evaluate_text_to_bool("5 != 6"), true);
    assert_eq!(evaluate_text_to_bool("5 != 5"), false);
}

#[test]
fn test_less_than() {
    assert_eq!(evaluate_text_to_bool("5 < 6"), true);
    assert_eq!(evaluate_text_to_bool("6 < 5"), false);
    assert_eq!(evaluate_text_to_bool("5 < 5"), false);
}

#[test]
fn test_greater_than() {
    assert_eq!(evaluate_text_to_bool("6 > 5"), true);
    assert_eq!(evaluate_text_to_bool("5 > 6"), false);
    assert_eq!(evaluate_text_to_bool("5 > 5"), false);
}

#[test]
fn test_less_than_or_equal() {
    assert_eq!(evaluate_text_to_bool("5 <= 6"), true);
    assert_eq!(evaluate_text_to_bool("5 <= 5"), true);
    assert_eq!(evaluate_text_to_bool("6 <= 5"), false);
}

#[test]
fn test_greater_than_or_equal() {
    assert_eq!(evaluate_text_to_bool("6 >= 5"), true);
    assert_eq!(evaluate_text_to_bool("5 >= 5"), true);
    assert_eq!(evaluate_text_to_bool("5 >= 6"), false);
}

#[test]
fn test_operator_precedence() {
    // (5 + 5) == 10 -> true
    assert_eq!(evaluate_text_to_bool("5 + 5 == 10"), true);
    // 5 + (5 == 10) -> 5 + false -> type error (ideally, but currently might fail or do something else)
    // Since we don't have mixed type arithmetic yet, we just test that precedence parses correctly
    // 10 == 5 + 5 -> 10 == (5 + 5) -> true
    assert_eq!(evaluate_text_to_bool("10 == 5 + 5"), true);
}

#[test]
fn test_boolean_operators() {
    assert_eq!(evaluate_text_to_bool("true && true"), true);
    assert_eq!(evaluate_text_to_bool("true && false"), false);
    assert_eq!(evaluate_text_to_bool("false || true"), true);
    assert_eq!(evaluate_text_to_bool("false || false"), false);
    assert_eq!(evaluate_text_to_bool("true ^ false"), true);
    assert_eq!(evaluate_text_to_bool("true ^ true"), false);
}

#[test]
fn test_boolean_operator_chaining() {
    assert_eq!(evaluate_text_to_bool("true && true && false"), false);
    assert_eq!(evaluate_text_to_bool("true && true && true"), true);
    assert_eq!(evaluate_text_to_bool("false || false || true"), true);
    assert_eq!(evaluate_text_to_bool("false || false || false"), false);
    assert_eq!(evaluate_text_to_bool("true ^ true ^ true"), true);
    assert_eq!(evaluate_text_to_bool("true ^ false ^ false"), true);
}
