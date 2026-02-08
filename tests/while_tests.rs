use silk::{
    parse_block,
    parsing::{ExpressionKind, ExpressionLiteral},
    test_support::{evaluate_text_to_expression, interpret_program, intrinsic_context},
};

#[test]
fn while_accumulates_until_limit() {
    let program = "
        sum_until := (limit: i32) => (
            mut acc := 0;
            mut iter := 0;
            while iter < limit do (
                acc = acc + iter;
                iter = iter + 1;
            );
            acc
        );

        sum_until 5
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => assert_eq!(value, 10),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn while_with_initially_false_condition_skips_body() {
    let program = "
        mut value := 3;
        while value < 0 do (
            value = value - 1;
        );
        value
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => assert_eq!(value, 3),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn while_condition_must_be_boolean() {
    let program = "
        mut value := 1;
        while value do (
            value = value - 1;
        );
    ";

    let expression = parse_block(program).expect("Failed to parse program text");

    let mut context = intrinsic_context();
    match interpret_program(expression, &mut context) {
        Err(diag) => assert_eq!(
            diag.message,
            "If condition did not resolve to a boolean value"
        ),
        Ok(_) => panic!("non-boolean while condition should fail"),
    }
}

#[test]
fn while_supports_literal_pattern_conditions() {
    let program = "
        mut value := 3;
        mut trips := 0;
        while 3 := value do (
            trips = trips + 1;
            value = 4;
        );
        trips
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => assert_eq!(value, 1),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}
