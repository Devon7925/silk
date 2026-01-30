use std::collections::HashMap;

use crate::{
    diagnostics::Diagnostic,
    parsing::Expression,
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
        map.insert(normalized, parsed);
    }
    Ok(map)
}

pub(crate) fn parse_source_block(source: &str) -> Result<Expression, Diagnostic> {
    crate::silk_parser::parse_block(source)
}
