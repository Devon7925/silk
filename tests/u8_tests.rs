use silk::parsing::{ExpressionKind, ExpressionLiteral};
use silk::test_support::evaluate_text_to_expression;

fn evaluate_text_to_number(program: &str) -> i32 {
    match evaluate_text_to_expression(program)
        .expect("Failed to interpret parsed expression")
        .0
        .kind
    {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => value,
        val => panic!("Expected numeric result, got {:?}", val),
    }
}

fn evaluate_text_to_bool(program: &str) -> bool {
    match evaluate_text_to_expression(program)
        .expect("Failed to interpret parsed expression")
        .0
        .kind
    {
        ExpressionKind::Literal(ExpressionLiteral::Boolean(value)) => value,
        val => panic!("Expected boolean result, got {:?}", val),
    }
}

#[test]
fn test_u8_addition() {
    let program = ""
        .to_string()
        + "add_one := (value: u8) => ( value + 1 );\n"
        + "add_one(41)";
    assert_eq!(evaluate_text_to_number(&program), 42);
}

#[test]
fn test_u8_comparisons() {
    let program = ""
        .to_string()
        + "is_even := (value: u8) => ( value == 2 );\n"
        + "is_even(2)";
    assert_eq!(evaluate_text_to_bool(&program), true);
}
