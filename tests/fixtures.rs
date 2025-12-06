use silk::test_support::evaluate_text_to_simplified_expression;

#[test]
fn interpret_range_sum() {
    let program = include_str!("../fixtures/range_sum.silk");
    let (_result, _context) =
        evaluate_text_to_simplified_expression(&program).unwrap_or_else(|err| {
            panic!("{}", err.render_with_source(&program));
        });
}
