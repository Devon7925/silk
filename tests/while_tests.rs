use silk::{
    parsing::parse_block,
    test_support::{evaluate_text_to_expression, interpret_program, intrinsic_context},
};

#[test]
fn while_accumulates_until_limit() {
    let program = "
        let sum_until = (limit: i32) => (
            let mut acc = 0;
            let mut iter = 0;
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

    match expr {
        silk::parsing::Expression::Literal(silk::parsing::ExpressionLiteral::Number(value), _) => {
            assert_eq!(value, 10)
        }
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn while_with_initially_false_condition_skips_body() {
    let program = "
        let mut value = 3;
        while value < 0 do (
            value = value - 1;
        );
        value
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr {
        silk::parsing::Expression::Literal(silk::parsing::ExpressionLiteral::Number(value), _) => {
            assert_eq!(value, 3)
        }
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn while_condition_must_be_boolean() {
    let program = "
        let mut value = 1;
        while value do (
            value = value - 1;
        );
    ";

    let (expression, remaining) = parse_block(program).expect("Failed to parse program text");
    assert!(remaining.trim().is_empty());

    let mut context = intrinsic_context();
    match interpret_program(expression, &mut context) {
        Err(diag) => assert_eq!(
            diag.message,
            "While condition did not resolve to a boolean value"
        ),
        Ok(_) => panic!("non-boolean while condition should fail"),
    }
}
