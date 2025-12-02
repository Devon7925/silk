use silk::parsing::{Expression, ExpressionLiteral};
use silk::test_support::evaluate_text_to_expression;

#[test]
fn functions_accept_struct_parameters() {
    let program = "
        let rotate_pair = {first = first: i32, second = second: i32} => (
            {
                first = 0 - second,
                second = first,
            }
        );

        let rotated = rotate_pair { first = 3, second = 4 };
        rotated.first + rotated.second
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr {
        Expression::Literal(ExpressionLiteral::Number(value), _) => assert_eq!(value, -1),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn struct_patterns_require_all_fields() {
    let program = "
        let sum_pair = {first = first: i32, second = second: i32} => (
            first + second
        );

        sum_pair { first = 1 }
    ";

    match evaluate_text_to_expression(program) {
        Ok(_) => panic!("expected missing field error"),
        Err(err) => assert!(
            err.message.contains("Missing field second"),
            "Unexpected error: {}",
            err.message
        ),
    }
}

#[test]
fn struct_variables_support_property_access() {
    let program = "
        let pair = { first = 2, second = 5 };
        let mirrored = { first = pair.second, second = pair.first };
        mirrored.second
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr {
        Expression::Literal(ExpressionLiteral::Number(value), _) => assert_eq!(value, 2),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn functions_can_return_structs() {
    let program = "
        let make_pair = (x: i32) => (
            {
                first = x,
                second = x + 1,
            }
        );

        let pair = make_pair 5;
        pair.second
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr {
        Expression::Literal(ExpressionLiteral::Number(value), _) => assert_eq!(value, 6),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}
