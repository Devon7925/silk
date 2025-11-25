use silk::test_support::evaluate_text_to_expression;

#[test]
fn loop_can_compute_factorial() {
    let program = "
        let factorial = fn(limit: i32) -> i32 (
            let mut acc = 1;
            let mut iter = limit;
            loop (
                acc = acc * iter;
                iter = iter - 1;
                if iter <= 0 (
                    return acc;
                )
            )
        );

        factorial 5
    ";

    let (expr, _) = evaluate_text_to_expression(program)
        .unwrap_or_else(|err| panic!("{}", err.message));

    match expr {
        silk::parsing::Expression::Literal(silk::parsing::ExpressionLiteral::Number(value), _) => {
            assert_eq!(value, 120)
        }
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}
