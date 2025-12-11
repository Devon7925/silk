use silk::test_support::{evaluate_text_to_simplified_expression, compile_exports};

#[test]
fn interpret_range_sum() {
    let program = include_str!("../fixtures/range_sum.silk");
    let (_result, _context) =
        evaluate_text_to_simplified_expression(&program).unwrap_or_else(|err| {
            panic!("{}", err.render_with_source(&program));
        });
}

#[test]
fn interpret_generic_option_while() {
    let program = include_str!("../fixtures/generic_option_while.silk");
    let (result, context) =
        evaluate_text_to_simplified_expression(&program).unwrap_or_else(|err| {
            panic!("{}", err.render_with_source(&program));
        });
    println!("Result: {:?}", result.pretty_print());
    compile_exports(&context).unwrap_or_else(|err| {
        panic!("{}", err.render_with_source(&program));
    });
}
