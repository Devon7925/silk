use silk::test_support::evaluate_text_to_expression;

#[test]
fn loop_can_compute_factorial() {
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

    match expr {
        silk::parsing::Expression::Literal(silk::parsing::ExpressionLiteral::Number(value), _) => {
            assert_eq!(value, 120)
        }
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

    match expr {
        silk::parsing::Expression::Literal(silk::parsing::ExpressionLiteral::Number(value), _) => {
            assert_eq!(value, 120)
        }
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}
