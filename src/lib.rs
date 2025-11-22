mod diagnostics;
mod enum_normalization;
mod interpret;
pub mod parsing;
mod simplify;
mod wasm;

pub mod test_support {
    pub use crate::interpret::{
        Context, evaluate_text_to_expression, interpret_program, intrinsic_context,
    };
    pub use crate::simplify::{simplify_context, simplify_expression};
}

pub use diagnostics::{Diagnostic, SourceSpan};

pub fn compile(file: String) -> Result<Vec<u8>, Diagnostic> {
    let (ast, remaining) = parsing::parse_block(&file)?;
    let leftover = remaining.trim_start();
    if !leftover.is_empty() {
        let token_len = leftover
            .chars()
            .take_while(|c| !c.is_whitespace())
            .map(|c| c.len_utf8())
            .sum::<usize>()
            .max(1);
        let start = file.len().checked_sub(leftover.len()).unwrap_or(0);
        let span = SourceSpan::new(start, token_len);
        return Err(Diagnostic::new("Unexpected trailing input").with_span(span));
    }
    let mut context = interpret::intrinsic_context();
    let (value, program_context) = interpret::interpret_program(ast, &mut context)?;
    let _simplified_value = simplify::simplify_expression(value)?;
    let simplified_context = simplify::simplify_context(program_context)?;
    wasm::compile_exports(&simplified_context)
}
