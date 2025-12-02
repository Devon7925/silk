use silk::parsing::{Expression, ExpressionLiteral};
use silk::test_support::evaluate_text_to_expression;

#[test]
fn generic_enum_option_t() {
    let program = "
let Option = (T: type) => (
    enum { Some = T, None = {} }
);

let unwrap_or_zero = (x: i32) => (
    let val: Option(i32) = if x > 0 then Option(i32)::Some(x) else Option(i32)::None;

    if let Option(i32)::Some(v) = val then v else 0
);
unwrap_or_zero(5) + unwrap_or_zero(-3)
    ";

    let (expr, _) = evaluate_text_to_expression(program).unwrap_or_else(|err| {
        panic!(
            "Evaluation failed with error: {}",
            err.render_with_source(program)
        );
    });

    match expr {
        Expression::Literal(ExpressionLiteral::Number(value), _) => assert_eq!(value, 5),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn generic_enum_nested_generics() {
    let program = "
let Option = (T: type) => (
    enum { Some = T, None = {} }
);
let Container = (T: type) => (
    enum { Wrapped = Option(T), Empty = {} }
);

let check = (x: i32) => (
    let c = Container(i32)::Wrapped(Option(i32)::Some(x));
    
    if let Container(i32)::Wrapped(opt) = c then (
        if let Option(i32)::Some(val) = opt then val else 0
    ) else 0
);
check(10) + check(-5)
    ";

    let (expr, _) = evaluate_text_to_expression(program).unwrap_or_else(|err| {
        panic!(
            "Evaluation failed with error: {}",
            err.render_with_source(program)
        );
    });

    match expr {
        Expression::Literal(ExpressionLiteral::Number(value), _) => assert_eq!(value, 5),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}
