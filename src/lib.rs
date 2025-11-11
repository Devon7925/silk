mod diagnostics;
mod interpret;
mod parsing;

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
    interpret::interpret_expression(ast, &mut context)?;
    Ok(vec![])
}
