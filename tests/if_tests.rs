use silk::parsing::{ExpressionKind, ExpressionLiteral};
use silk::test_support::evaluate_text_to_expression;

#[test]
fn if_expression_returns_then_branch() {
    let (expr, _) = evaluate_text_to_expression("if true then 1 else 2")
        .expect("if expression should evaluate");
    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => {
            assert_eq!(value, 1);
        }
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn else_branch_is_used_when_condition_is_false() {
    let (expr, _) = evaluate_text_to_expression("if false then 1 else 2")
        .expect("if expression should evaluate");
    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => {
            assert_eq!(value, 2);
        }
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn else_if_chains_resolve_first_true_branch() {
    let (expr, _) = evaluate_text_to_expression("if false then 1 else if true then 2 else 3")
        .expect("else if chain should evaluate");
    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => {
            assert_eq!(value, 2);
        }
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn block_with_trailing_semicolon_returns_empty_struct() {
    let (expr, _) =
        evaluate_text_to_expression("( 1; )").expect("block expression should evaluate");
    match expr.kind {
        ExpressionKind::Struct(items) => assert!(items.is_empty(), "Expected empty struct"),
        other => panic!("Expected empty struct, got {:?}", other),
    }
}

#[test]
fn mismatched_branch_types_report_diagnostic() {
    let err = match evaluate_text_to_expression("if true then 1 else true") {
        Ok(_) => panic!("expected diagnostic"),
        Err(err) => err,
    };
    assert!(
        err.message.contains("Type mismatch between if branches"),
        "Unexpected error: {}",
        err.message
    );
}

#[test]
fn empty_struct_branches_are_type_checked() {
    let err = match evaluate_text_to_expression("if true then ( 1; ) else ( 2 )") {
        Ok(_) => panic!("expected diagnostic"),
        Err(err) => err,
    };
    assert!(
        err.message.contains("Type mismatch between if branches"),
        "Unexpected error: {}",
        err.message
    );
}

#[test]
fn return_allows_unbalanced_if_in_statement_position() {
    let program = "
        (export wasm) early_exit_loopish := (flag: bool) => (
            if flag then (
                return 1;
            ) else (
                false
            );
            2
        );

        early_exit_loopish false
        ";

    let (expr, _) = evaluate_text_to_expression(program)
        .expect("if expression with return in one branch should evaluate");

    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => assert_eq!(value, 2),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}
