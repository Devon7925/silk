use silk::compile;
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

#[test]
fn compiles_wasm_export_with_bindings() {
    let program = r#"
        let export(wasm) double_add = fn(x: i32) -> i32 (
            let y = x * 2;
            y + y
        );
        {}
        "#;
    let wasm = compile(program.to_string()).expect("compilation should succeed");
    assert!(!wasm.is_empty());

    let mut found_body = false;
    for payload in Parser::new(0).parse_all(&wasm) {
        if let Payload::CodeSectionEntry(body) = payload.expect("payload") {
            let mut reader = body.get_operators_reader().expect("operators");
            // x * 2
            match reader.read().expect("op") {
                Operator::LocalGet { local_index } => assert_eq!(local_index, 0), // x
                op => panic!("expected local.get 0, got {:?}", op),
            }
            match reader.read().expect("op") {
                Operator::I32Const { value } => assert_eq!(value, 2),
                op => panic!("expected i32.const 2, got {:?}", op),
            }
            match reader.read().expect("op") {
                Operator::I32Mul => {}
                op => panic!("expected i32.mul, got {:?}", op),
            }
            // let y = ... (local.set)
            match reader.read().expect("op") {
                Operator::LocalSet { local_index } => assert_eq!(local_index, 1), // y
                op => panic!("expected local.set 1, got {:?}", op),
            }
            // y + y
            match reader.read().expect("op") {
                Operator::LocalGet { local_index } => assert_eq!(local_index, 1), // y
                op => panic!("expected local.get 1, got {:?}", op),
            }
            match reader.read().expect("op") {
                Operator::LocalGet { local_index } => assert_eq!(local_index, 1), // y
                op => panic!("expected local.get 1, got {:?}", op),
            }
            match reader.read().expect("op") {
                Operator::I32Add => {}
                op => panic!("expected i32.add, got {:?}", op),
            }
            match reader.read().expect("op") {
                Operator::End => {}
                op => panic!("expected end, got {:?}", op),
            }
            found_body = true;
        }
    }
    assert!(found_body, "expected to find function body");
}
