use silk::parsing::{Expression, ExpressionLiteral};
use silk::test_support::evaluate_text_to_expression;

#[test]
fn rust_style_enums_support_if_let_patterns() {
    let program = "
let IntOption = enum {
    Some = { i32 },
    None = {},
};
let calculate_positive_sum = fn{first: i32, second: i32} -> i32 (
    if first + second <= 0 (
        IntOption::None
    ) else (
        IntOption::Some{first + second}
    )
);
let export(wasm) main = fn{} -> i32 (
    let a = calculate_positive_sum{0 - 1, 2};
    let b = calculate_positive_sum{0 - 3, 2};
    if let IntOption::Some{b_result} = b ( 1 )
    else if let IntOption::Some{a_result} = a ( 0 ) else ( 1 )
);
main{}
    ";

    let (expr, _) = evaluate_text_to_expression(program)
        .unwrap_or_else(|err| panic!("{}", err.message));

    match expr {
        Expression::Literal(ExpressionLiteral::Number(value), _) => assert_eq!(value, 0),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn rust_style_enums_allow_line_comments() {
    let program = "
let IntOption = enum {
    Some = { i32 },
    None = {},
};
let export(wasm) main = fn{} -> i32 (
    let maybe_positive = IntOption::Some{2};
    if let IntOption::Some{value} = maybe_positive (
        value // comment after expression
    ) else (
        99
    )
);
main{}";

    let (expr, _) = evaluate_text_to_expression(program)
        .unwrap_or_else(|err| panic!("{}", err.message));

    match expr {
        Expression::Literal(ExpressionLiteral::Number(value), _) => assert_eq!(value, 2),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

