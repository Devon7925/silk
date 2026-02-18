use silk::syntax::{ExpressionKind, ExpressionLiteral};
use silk::test_support::evaluate_text_to_expression;

#[test]
fn test_char_literal_value() {
    let (expr, _) = evaluate_text_to_expression("'A'").expect("Failed to interpret char literal");
    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Char(value)) => {
            assert_eq!(value, b'A');
        }
        other => panic!("Expected char literal, got {:?}", other),
    }
}

#[test]
fn test_string_literal_value() {
    let (expr, _) =
        evaluate_text_to_expression("\"hi\"").expect("Failed to interpret string literal");
    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::String(bytes)) => {
            assert_eq!(bytes, b"hi");
        }
        other => panic!("Expected string literal, got {:?}", other),
    }
}

#[test]
fn test_string_literal_index() {
    let (expr, _) =
        evaluate_text_to_expression("\"hi\"(1)").expect("Failed to interpret string literal index");
    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Char(value)) => {
            assert_eq!(value, b'i');
        }
        other => panic!("Expected char literal, got {:?}", other),
    }
}

#[test]
fn test_string_binding_index() {
    let (expr, _) = evaluate_text_to_expression("value := \"hi\"; value(0)")
        .expect("Failed to index string binding");
    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Char(value)) => {
            assert_eq!(value, b'h');
        }
        other => panic!("Expected char literal, got {:?}", other),
    }
}

#[test]
fn test_string_literal_index_out_of_range() {
    let err = evaluate_text_to_expression("\"hi\"(2)")
        .expect_err("Expected out-of-range string index to fail");
    assert!(
        err.message.contains("Array index out of range"),
        "unexpected error: {}",
        err.message
    );
}

