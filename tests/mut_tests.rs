use silk::parsing::{Expression, ExpressionLiteral};
use silk::test_support::evaluate_text_to_simplified_expression;

#[test]
fn nested_mutation() {
    let program = "
    (export wasm) destructure_mut := (bar: i32) => (
        mut foo := { first = {bar, 3}, second = 4 };
        foo.first.0 = foo.first.0 + foo.second;
        foo.first.0
    );
    destructure_mut(5)
    ";

    let (expr, _) = evaluate_text_to_simplified_expression(program).unwrap_or_else(|err| {
        panic!(
            "Evaluation failed with error: {}",
            err.render_with_source(program)
        );
    });

    
    let Expression::Block(statements, _) = expr else {
        panic!("Expected block expression, got {:?}", expr);
    };
    let expr = statements.iter().last().unwrap();
    let Expression::Block(statements, _) = expr else {
        panic!("Expected block expression, got {:?}", expr);
    };

    match statements.iter().last().unwrap() {
        Expression::Literal(ExpressionLiteral::Number(value), _) => assert_eq!(value, &9),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}
