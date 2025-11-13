mod diagnostics;
mod interpret;
mod parsing;
mod simplify;
mod wasm;

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

#[cfg(test)]
mod tests {
    use super::*;
    use wasmparser::{Operator, Parser, Payload};

    #[test]
    fn compiles_const_wasm_export() {
        let program = r#"
let export(wasm) answer = fn{} -> i32 (
    42
);
answer
"#;
        let wasm = compile(program.to_string()).expect("compilation should succeed");
        assert!(
            !wasm.is_empty(),
            "expected wasm module bytes for wasm export"
        );

        let mut exports = Vec::new();
        let mut const_values = Vec::new();
        for payload in Parser::new(0).parse_all(&wasm) {
            match payload.expect("failed to parse wasm payload") {
                Payload::ExportSection(section) => {
                    for export in section {
                        let export = export.expect("invalid export entry");
                        exports.push(export.name.to_string());
                    }
                }
                Payload::CodeSectionEntry(body) => {
                    let mut reader = body.get_operators_reader().expect("operators");
                    match reader.read().expect("operator") {
                        Operator::I32Const { value } => const_values.push(value),
                        other => panic!("unexpected operator: {:?}", other),
                    }
                    assert!(
                        matches!(reader.read().expect("end"), Operator::End),
                        "expected function to end after constant"
                    );
                    assert!(
                        reader.read().is_err(),
                        "expected no more operators in function body"
                    );
                }
                _ => {}
            }
        }

        assert_eq!(exports, vec!["answer"]);
        assert_eq!(const_values, vec![42]);
    }

    #[test]
    fn compiles_parameterized_wasm_export() {
        let program = r#"
let export(wasm) add_one = fn(x: i32) -> i32 (
    x + 1
);
{}
"#;
        let wasm = compile(program.to_string()).expect("compilation should succeed");
        assert!(
            !wasm.is_empty(),
            "expected wasm module bytes for wasm export"
        );

        let mut exports = Vec::new();
        let mut found_body = false;

        for payload in Parser::new(0).parse_all(&wasm) {
            match payload.expect("failed to parse wasm payload") {
                Payload::ExportSection(section) => {
                    for export in section {
                        let export = export.expect("invalid export entry");
                        exports.push(export.name.to_string());
                    }
                }
                Payload::CodeSectionEntry(body) => {
                    let mut reader = body.get_operators_reader().expect("operators");
                    match reader.read().expect("local.get") {
                        Operator::LocalGet { local_index } => assert_eq!(local_index, 0),
                        other => panic!("expected local.get, saw {:?}", other),
                    }
                    match reader.read().expect("i32.const") {
                        Operator::I32Const { value } => assert_eq!(value, 1),
                        other => panic!("expected i32.const, saw {:?}", other),
                    }
                    match reader.read().expect("i32.add") {
                        Operator::I32Add => {}
                        other => panic!("expected i32.add, saw {:?}", other),
                    }
                    match reader.read().expect("end") {
                        Operator::End => {}
                        other => panic!("expected end, saw {:?}", other),
                    }
                    assert!(
                        reader.read().is_err(),
                        "expected no extra operators in function body"
                    );
                    found_body = true;
                }
                _ => {}
            }
        }

        assert_eq!(exports, vec!["add_one"]);
        assert!(found_body, "expected to find function body");
    }

    #[test]
    fn compile_without_wasm_exports_returns_empty() {
        let program = r#"
let answer = 5;
answer
"#;
        let wasm = compile(program.to_string()).expect("compilation should not fail");
        assert!(wasm.is_empty(), "expected no wasm bytes without exports");
    }

    #[test]
    fn exporting_non_function_reports_diagnostic() {
        let program = r#"
let export(wasm) answer: i32 = 42;
answer
"#;
        let err = compile(program.to_string()).expect_err("expected failure");
        assert!(
            err.message
                .contains("Only functions can be exported to wasm"),
            "unexpected diagnostic message: {}",
            err.message
        );
    }
}
