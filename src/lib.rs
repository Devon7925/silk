mod diagnostics;
pub mod parsing;
mod uniquify;
mod interpret;
mod intermediate;
mod wasm;

pub mod test_support {
    pub use crate::interpret::{
        Context, evaluate_text_to_expression, interpret_program, intrinsic_context,
    };
    pub use crate::wasm::compile_exports;
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
        let start = file.len().saturating_sub(leftover.len());
        let span = SourceSpan::new(start, token_len);
        return Err(Diagnostic::new("Unexpected trailing input").with_span(span));
    }
    let uniquified = uniquify::uniquify_program(ast);
    let mut context = interpret::intrinsic_context();
    let (_value, program_context) = interpret::interpret_program(uniquified, &mut context)?;
    wasm::compile_exports(&program_context)
}
