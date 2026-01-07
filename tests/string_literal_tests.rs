use silk::parsing::{ExpressionKind, ExpressionLiteral};
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
    let (expr, _) = evaluate_text_to_expression("\"hi\"(1)")
        .expect("Failed to interpret string literal index");
    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Char(value)) => {
            assert_eq!(value, b'i');
        }
        other => panic!("Expected char literal, got {:?}", other),
    }
}
