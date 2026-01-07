use std::collections::HashMap;

use crate::{
    diagnostics::{Diagnostic, SourceSpan},
    parsing::{self, Expression},
    uniquify,
};

pub(crate) fn normalize_path(path: &str) -> String {
    let mut normalized = path.replace('\\', "/");
    if let Some(stripped) = normalized.strip_prefix("./") {
        normalized = stripped.to_string();
    }
    normalized
}

pub(crate) fn build_parsed_files(
    files: Vec<(&str, &str)>,
) -> Result<HashMap<String, Expression>, Diagnostic> {
    let mut map = HashMap::new();
    for (path, source) in files {
        let normalized = normalize_path(path);
        let parsed = parse_source_block(source)?;
        map.insert(normalized, uniquify::uniquify_program(parsed));
    }
    Ok(map)
}

pub(crate) fn parse_source_block(source: &str) -> Result<Expression, Diagnostic> {
    let (ast, remaining) = parsing::parse_block(source)?;
    let leftover = remaining.trim_start();
    if !leftover.is_empty() {
        let token_len = leftover
            .chars()
            .take_while(|c| !c.is_whitespace())
            .map(|c| c.len_utf8())
            .sum::<usize>()
            .max(1);
        let start = source.len().saturating_sub(leftover.len());
        let span = SourceSpan::new(start, token_len);
        return Err(Diagnostic::new("Unexpected trailing input").with_span(span));
    }
    Ok(ast)
}
