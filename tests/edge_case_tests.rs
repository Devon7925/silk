use silk::parsing::{Expression, ExpressionLiteral, parse_block};
use silk::test_support::{
    Context, interpret_program, intrinsic_context, simplify_context, simplify_expression,
};

fn evaluate_text_to_simplified_expression(
    program: &str,
) -> Result<(Expression, Context), silk::Diagnostic> {
    let (expression, remaining) = parse_block(program).expect("Failed to parse program text");
    assert!(
        remaining.trim().is_empty(),
        "Parser did not consume entire input, remaining: {remaining:?}"
    );

    let mut context = intrinsic_context();
    let (result, context) = interpret_program(expression, &mut context)?;
    let simplified_expression = simplify_expression(result)?;
    let simplified_context = simplify_context(context)?;
    Ok((simplified_expression, simplified_context))
}

fn assert_final_number(result: Expression, expected: i32) {
    match result {
        Expression::Literal(ExpressionLiteral::Number(value), _) => {
            assert_eq!(value, expected)
        }
        Expression::Block(expressions, _) => match expressions.last() {
            Some(Expression::Literal(ExpressionLiteral::Number(value), _)) => {
                assert_eq!(*value, expected)
            }
            other => panic!("Expected final numeric literal, got {:?}", other),
        },
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn test_shadowing_in_block() {
    let program = "
        let (export wasm) test_shadow = (x: i32) => (
            let y = 10;
            (
                let y = 20;
                x + y
            )
        );
        {}
        ";
    let (_result, context) =
        evaluate_text_to_simplified_expression(program).expect("interpretation should succeed");

    let bindings = context.annotated_bindings();
    let _binding = bindings
        .iter()
        .find(|b| b.name == "test_shadow")
        .expect("binding found");
}

#[test]
fn test_unused_bindings() {
    let program = "
        let (export wasm) unused_test = (x: i32) => (
            let unused = 100;
            x
        );
        {}
        ";
    evaluate_text_to_simplified_expression(program).expect("interpretation should succeed");
}

#[test]
fn test_nested_arithmetic() {
    let program = "
        let (export wasm) math_test = (x: i32) => (
            let a = x * 2;
            let b = a + 5;
            b / 2
        );
        {}
        ";
    evaluate_text_to_simplified_expression(program).expect("interpretation should succeed");
}

#[test]
fn test_division_by_zero_errors() {
    let program = "
        let (export wasm) div_zero = 1 / 0;
        {}
        ";
    let (expression, remaining) = parse_block(program).expect("Failed to parse program text");
    assert!(
        remaining.trim().is_empty(),
        "Parser did not consume entire input, remaining: {remaining:?}"
    );

    let mut context = intrinsic_context();
    let result = interpret_program(expression, &mut context);

    match result {
        Err(diag) => assert_eq!(diag.message, "Division by zero"),
        Ok(_) => panic!("division by zero should not interpret successfully"),
    }
}

#[test]
fn test_negative_number_literal() {
    let program = "
        let (export wasm) negative_literal = -5;
        {}
        ";
    let (_result, context) =
        evaluate_text_to_simplified_expression(program).expect("interpretation should succeed");

    let binding = context
        .annotated_bindings()
        .into_iter()
        .find(|b| b.name == "negative_literal")
        .expect("exported binding found");

    assert!(matches!(
        binding.value,
        Expression::Literal(ExpressionLiteral::Number(-5), _)
    ));
}

#[test]
fn test_mutable_assignment_updates_binding() {
    let program = "
        let mut counter = 1;
        counter = counter + 1;
        counter
        ";

    let (result, _context) =
        evaluate_text_to_simplified_expression(program).expect("interpretation should succeed");

    assert_final_number(result, 2);
}

#[test]
fn test_shadowed_mutable_bindings_respect_scope() {
    let program = "
        let mut x = 1;
        (
            let mut x = 2;
            x = x + 5;
            x
        );
        x
        ";

    let (result, _context) =
        evaluate_text_to_simplified_expression(program).expect("interpretation should succeed");

    assert_final_number(result, 1);
}

#[test]
fn test_mutable_struct_destructuring_propagates_mut() {
    let program = "
        let mut { first = a, second = b } = { first = 2, second = 3 };
        a = a + b;
        a
        ";

    let (result, _context) =
        evaluate_text_to_simplified_expression(program).expect("interpretation should succeed");

    assert_final_number(result, 5);
}

#[test]
fn test_mutable_struct_property_updates_through_rebinding() {
    let program = "
        let mut pair = { first = 1, second = 2 };
        pair = { first = pair.first + 5, second = pair.second };
        pair.first
        ";

    let (result, _context) =
        evaluate_text_to_simplified_expression(program).expect("interpretation should succeed");

    assert_final_number(result, 6);
}

#[test]
fn test_assignment_requires_mut_annotation() {
    let program = "
        let counter = 1;
        counter = 2;
        ";

    let (expression, remaining) = parse_block(program).expect("Failed to parse program text");
    assert!(remaining.trim().is_empty());

    let mut context = intrinsic_context();
    let result = interpret_program(expression, &mut context);

    match result {
        Err(diag) => assert_eq!(
            diag.message,
            "Cannot assign to immutable identifier: counter"
        ),
        Ok(_) => panic!("assignment to immutable binding should fail"),
    }
}

#[test]
fn test_assignment_respects_type_hints() {
    let program = "
        let mut counter: i32 = 1;
        counter = true;
        ";

    let (expression, remaining) = parse_block(program).expect("Failed to parse program text");
    assert!(remaining.trim().is_empty());

    let mut context = intrinsic_context();
    let result = interpret_program(expression, &mut context);

    match result {
        Err(diag) => assert_eq!(
            diag.message,
            "Cannot assign value of mismatched type to counter"
        ),
        Ok(_) => panic!("type mismatched assignment should fail"),
    }
}
