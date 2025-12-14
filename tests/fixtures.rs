use silk::test_support::{compile_exports, evaluate_text_to_simplified_expression};

#[test]
fn interpret_pass_pair() {
    let program = include_str!("../fixtures/pass_pair.silk");
    let (result, context) =
        evaluate_text_to_simplified_expression(&program).unwrap_or_else(|err| {
            panic!("{}", err.render_with_source(&program));
        });
    println!("Result: {:?}", result.pretty_print());
    compile_exports(&context).unwrap_or_else(|err| {
        panic!("{}", err.render_with_source(&program));
    });
}

#[test]
fn interpret_range_sum() {
    let program = include_str!("../fixtures/range_sum.silk");
    let (result, context) =
        evaluate_text_to_simplified_expression(&program).unwrap_or_else(|err| {
            panic!("{}", err.render_with_source(&program));
        });
    println!("Result: {:?}", result.pretty_print());
    compile_exports(&context).unwrap_or_else(|err| {
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
