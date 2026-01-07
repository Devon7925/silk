use silk::parsing::{ExpressionKind, ExpressionLiteral};
use silk::test_support::evaluate_files_to_expression;

#[test]
fn use_imports_block_expression() {
    let root = r#"
    lib := use "lib.silk";
    lib.answer
    "#;
    let lib = r#"
    {
        answer = 40 + 2
    }
    "#;

    let (value, _context) =
        evaluate_files_to_expression(vec![("main.silk", root), ("lib.silk", lib)], "main.silk")
            .expect("use import should evaluate");

    match value.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(result)) => assert_eq!(result, 42),
        other => panic!("Unexpected value: {:?}", other),
    }
}
