use std::fs;

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

#[test]
fn use_imports_file_with_intrinsic_types() {
    let root = r#"
    types := use "types.silk";
    types.answer
    "#;
    let types = r#"
    MyInt := i32;
    {
        answer = 42
    }
    "#;

    let (value, _context) = evaluate_files_to_expression(
        vec![("main.silk", root), ("types.silk", types)],
        "main.silk",
    )
    .expect("use import with intrinsic type should evaluate");

    match value.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(result)) => assert_eq!(result, 42),
        other => panic!("Unexpected value: {:?}", other),
    }
}

#[test]
fn use_types_node_can_be_boxed_as_type() {
    let types = fs::read_to_string("silk_src/types.silk")
        .unwrap_or_else(|err| panic!("failed to read silk_src/types.silk: {err}"));
    let root = r#"
    types := use "types.silk";
    Node := types.Node;
    value: Box(Node) := {};
    {}
    "#;

    let (value, _context) = evaluate_files_to_expression(
        vec![("main.silk", root), ("types.silk", types.as_str())],
        "main.silk",
    )
    .unwrap_or_else(|err| {
        panic!(
            "boxing imported types.Node failed:\nmessage: {}\nrender main:\n{}\nrender types:\n{}",
            err.message,
            err.render_with_source(root),
            err.render_with_source(types.as_str())
        )
    });

    match value.kind {
        ExpressionKind::Struct(_) => {}
        other => panic!("Unexpected value: {:?}", other),
    }
}

#[test]
fn debug_box_zero_error_snapshot() {
    let root = r#"
    Box(0)
    "#;
    let err = evaluate_files_to_expression(vec![("main.silk", root)], "main.silk")
        .expect_err("Box(0) should fail type checking");
    println!("{}", err.message);
    assert!(!err.message.is_empty());
}

#[test]
fn use_minimal_imported_node_can_be_boxed_as_type() {
    let types = r#"
    Node := { value = i32 };
    { Node = Node }
    "#;
    let root = r#"
    types := use "types.silk";
    Node := types.Node;
    value: Box(Node) := {};
    {}
    "#;

    let (value, _context) = evaluate_files_to_expression(
        vec![("main.silk", root), ("types.silk", types)],
        "main.silk",
    )
    .unwrap_or_else(|err| {
        panic!(
            "boxing minimal imported Node failed:\nmessage: {}\nrender main:\n{}\nrender types:\n{}",
            err.message,
            err.render_with_source(root),
            err.render_with_source(types)
        )
    });

    match value.kind {
        ExpressionKind::Struct(_) => {}
        other => panic!("Unexpected value: {:?}", other),
    }
}

#[test]
fn use_imported_root_type_can_be_boxed() {
    let types = r#"
    Node := { value = i32 };
    Node
    "#;
    let root = r#"
    types := use "types.silk";
    value: Box(types) := {};
    {}
    "#;

    let (value, _context) = evaluate_files_to_expression(
        vec![("main.silk", root), ("types.silk", types)],
        "main.silk",
    )
    .unwrap_or_else(|err| {
        panic!(
            "boxing imported root type failed:\nmessage: {}\nrender main:\n{}\nrender types:\n{}",
            err.message,
            err.render_with_source(root),
            err.render_with_source(types)
        )
    });

    match value.kind {
        ExpressionKind::Struct(_) => {}
        other => panic!("Unexpected value: {:?}", other),
    }
}
