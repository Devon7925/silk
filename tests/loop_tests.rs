use silk::syntax::{ExpressionKind, ExpressionLiteral};
use silk::test_support::evaluate_text_to_expression;

#[test]
fn loop_can_compute_factorial_using_return() {
    let program = "
        factorial := (limit: i32) => (
            mut acc := 1;
            mut iter := limit;
            loop (
                acc = acc * iter;
                iter = iter - 1;
                if iter <= 0 then (
                    return acc;
                )
            );
            1
        );

        factorial 5
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => assert_eq!(value, 120),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn loop_can_compute_factorial_using_early_break() {
    let program = "
        factorial := (limit: i32) => (
            mut acc := 1;
            mut iter := limit;
            loop (
                if iter <= 0 then (
                    break acc;
                );
                acc = acc * iter;
                iter = iter - 1;
            )
        );

        factorial 5
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => assert_eq!(value, 120),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn loop_can_compute_factorial_using_late_break() {
    let program = "
        factorial := (limit: i32) => (
            mut acc := 1;
            mut iter := limit;
            loop (
                acc = acc * iter;
                iter = iter - 1;
                if iter <= 0 then (
                    break acc;
                );
            )
        );

        factorial 5
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => assert_eq!(value, 120),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn while_can_compute_factorial() {
    let program = "
        factorial := (limit: i32) => (
            mut acc := 1;
            mut iter := limit;
            while iter > 0 do (
                acc = acc * iter;
                iter = iter - 1;
            );
            acc
        );

        factorial 5
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => assert_eq!(value, 120),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

