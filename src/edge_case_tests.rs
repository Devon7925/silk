#[cfg(test)]
mod tests {
    use crate::interpret::{Context, interpret_program, intrinsic_context};
    use crate::parsing::Expression;
    use crate::simplify::{simplify_context, simplify_expression};

    fn evaluate_text_to_simplified_expression(
        program: &str,
    ) -> Result<(Expression, Context), crate::diagnostics::Diagnostic> {
        let (expression, remaining) =
            crate::parsing::parse_block(program).expect("Failed to parse program text");
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

    #[test]
    fn test_shadowing_in_block() {
        let program = "
        let export(wasm) test_shadow = fn(x: i32) -> i32 (
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
        let export(wasm) unused_test = fn(x: i32) -> i32 (
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
        let export(wasm) math_test = fn(x: i32) -> i32 (
            let a = x * 2;
            let b = a + 5;
            b / 2
        );
        {}
        ";
        evaluate_text_to_simplified_expression(program).expect("interpretation should succeed");
    }
}
