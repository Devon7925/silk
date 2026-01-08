use silk::parsing::{parse_block, BindingAnnotation, BindingPattern, ExpressionKind, Identifier};

#[test]
fn for_loop_desugars_with_identifier_iterator() {
    let (expr, remaining) = parse_block("for item in iter do (item)").unwrap();
    assert!(remaining.trim().is_empty());

    let ExpressionKind::Block(items) = expr.kind else {
        panic!("expected block from for loop");
    };
    assert_eq!(items.len(), 2);

    let ExpressionKind::Binding(binding) = &items[0].kind else {
        panic!("expected iterator binding");
    };
    let BindingPattern::Annotated { pattern, .. } = &binding.pattern else {
        panic!("expected annotated iterator binding");
    };
    assert!(matches!(
        pattern.as_ref(),
        BindingPattern::Identifier(Identifier { name, .. }, _) if name == "iter"
    ));

    let ExpressionKind::Loop { body } = &items[1].kind else {
        panic!("expected loop body from for desugaring");
    };
    let ExpressionKind::If { condition, .. } = &body.kind else {
        panic!("expected if inside loop");
    };
    let ExpressionKind::Binding(condition_binding) = &condition.kind else {
        panic!("expected binding condition");
    };
    let BindingPattern::EnumVariant { payload, .. } = &condition_binding.pattern else {
        panic!("expected enum variant pattern");
    };
    assert!(matches!(
        payload.as_deref(),
        Some(BindingPattern::Identifier(Identifier { name, .. }, _)) if name == "item"
    ));
}

#[test]
fn for_loop_parses_iterator_call_expression() {
    let (expr, remaining) = parse_block("for value in make_range(3 + 1) do (value)").unwrap();
    assert!(remaining.trim().is_empty());

    let ExpressionKind::Block(items) = expr.kind else {
        panic!("expected block from for loop");
    };
    let ExpressionKind::Binding(binding) = &items[0].kind else {
        panic!("expected iterator binding");
    };
    let BindingPattern::Annotated {
        annotations,
        pattern,
        ..
    } = &binding.pattern
    else {
        panic!("expected annotated iterator binding");
    };
    assert!(annotations
        .iter()
        .any(|ann| matches!(ann, BindingAnnotation::Mutable(_))));
    assert!(matches!(
        pattern.as_ref(),
        BindingPattern::Identifier(Identifier { name, .. }, _) if name == "__for_iter"
    ));
    let ExpressionKind::FunctionCall { function, argument } = &binding.expr.kind else {
        panic!("expected iterator call expression");
    };
    assert!(matches!(
        &function.kind,
        ExpressionKind::Identifier(Identifier { name, .. }) if name == "make_range"
    ));
    let ExpressionKind::Operation { operator, .. } = &argument.kind else {
        panic!("expected argument expression");
    };
    assert_eq!(operator, "+");
}
