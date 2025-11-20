use silk::parsing::{Expression, ExpressionLiteral};
use silk::test_support::evaluate_text_to_expression;

#[test]
fn functions_can_be_passed_as_arguments() {
    let program = "
        let apply = fn{ func = func, value = value } -> i32 (
            func value
        );

        let increment = fn(x: i32) -> i32 (
            x + 1
        );

        apply { func = increment, value = 41 }
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr {
        Expression::Literal(ExpressionLiteral::Number(value), _) => assert_eq!(value, 42),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn functions_can_be_returned() {
    let program = "
        let make_adder = fn(base: i32) -> i32 (
            fn(offset: i32) -> i32 (
                base + offset
            )
        );

        let add_three = make_adder 3;
        add_three 7
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr {
        Expression::Literal(ExpressionLiteral::Number(value), _) => assert_eq!(value, 10),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}
