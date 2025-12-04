use silk::parsing::{Expression, ExpressionLiteral};
use silk::test_support::evaluate_text_to_expression;

#[test]
fn functions_can_be_passed_as_arguments() {
    let program = "
    (export wasm) apply_increment := (x: i32) => (
        apply := { func = func: (i32 -> i32), value = value: i32 } => (
            func value
        );

        increment := (y: i32) => (
            y + 1
        );

        apply { func = increment, value = x }
    );
    apply_increment 41
    ";

    let (expr, _) = evaluate_text_to_expression(program)
        .unwrap_or_else(|err| panic!("{}", err.render_with_source(program)));

    match expr {
        Expression::Literal(ExpressionLiteral::Number(value), _) => assert_eq!(value, 42),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn functions_can_be_returned() {
    let program = "
        make_adder := (base: i32) => (
            (offset: i32) => (
                base + offset
            )
        );

        add_three := make_adder 3;
        add_three 7
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr {
        Expression::Literal(ExpressionLiteral::Number(value), _) => assert_eq!(value, 10),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn return_exits_function_early() {
    let program = "
        early := (x: i32) => (
            return x + 1;
            x + 100
        );

        early 41
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr {
        Expression::Literal(ExpressionLiteral::Number(value), _) => assert_eq!(value, 42),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}
