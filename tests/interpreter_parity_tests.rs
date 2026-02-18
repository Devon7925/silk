use silk::syntax::{ExpressionKind, ExpressionLiteral};
use silk::test_support::evaluate_text_to_expression;

fn evaluate_text_to_number(program: &str) -> i32 {
    match evaluate_text_to_expression(program)
        .unwrap_or_else(|err| panic!("{}", err.render_with_source(program)))
        .0
        .kind
    {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => value,
        other => panic!("expected numeric literal, got {:?}", other),
    }
}

#[test]
fn option_builtin_requires_type_argument() {
    assert!(
        evaluate_text_to_expression("Option(1); 0").is_err(),
        "Option should reject non-type arguments"
    );
}

#[test]
fn iterator_builtin_accepts_non_type_argument() {
    assert!(
        evaluate_text_to_expression("Iterator(1); 0").is_ok(),
        "Iterator should match current Rust interpreter behavior for non-type arguments"
    );
}

#[test]
fn enum_patterns_compare_option_payload_types() {
    let result = evaluate_text_to_number(
        "
        value := Option(i32)::Some(1);
        if Option(bool)::Some(flag) := value then 1 else 0
        ",
    );
    assert_eq!(result, 0);
}

#[test]
fn range_type_binding_resolves_builtin_range() {
    let result = evaluate_text_to_number(
        "
        range: Range := 0..3;
        range.end
        ",
    );
    assert_eq!(result, 3);
}

#[test]
fn empty_struct_parameter_matches_unit_argument() {
    let result = evaluate_text_to_number(
        "
        check := {} => (1);
        check{}
        ",
    );
    assert_eq!(result, 1);
}

#[test]
fn enum_builtin_rejects_value_payloads() {
    assert!(
        evaluate_text_to_expression(
            "
            Bad := enum { Value = 1 };
            {};
            ",
        )
        .is_err(),
        "enum variants should reject non-type payload expressions",
    );
}

#[test]
fn if_requires_branch_type_compatibility_for_scalars() {
    assert!(
        evaluate_text_to_expression("if true then 1 else true").is_err(),
        "if branches with mismatched scalar types should be rejected",
    );
}

#[test]
fn if_requires_branch_type_compatibility_for_unit_and_scalar() {
    assert!(
        evaluate_text_to_expression("if true then (1;) else (2)").is_err(),
        "if branches with unit/scalar mismatch should be rejected",
    );
}

