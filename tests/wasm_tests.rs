use silk::compile;
use wasmparser::{Operator, Parser, Payload};

#[test]
fn compiles_const_wasm_export() {
    let program = r#"
(export wasm) answer := {} => (
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
                match reader.read().expect("terminator") {
                    Operator::Return => assert!(
                        matches!(reader.read().expect("end"), Operator::End),
                        "expected function to end after return"
                    ),
                    Operator::End => assert!(reader.read().is_err()),
                    other => panic!("unexpected terminator: {:?}", other),
                }
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
(export wasm) add_one := (x: i32) => (
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
                match reader.read().expect("terminator") {
                    Operator::Return => {
                        assert!(matches!(reader.read().expect("end"), Operator::End))
                    }
                    Operator::End => assert!(reader.read().is_err()),
                    other => panic!("unexpected terminator: {:?}", other),
                }
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
answer := 5;
answer
"#;
    let wasm = compile(program.to_string()).expect("compilation should not fail");
    assert!(wasm.is_empty(), "expected no wasm bytes without exports");
}

#[test]
fn exporting_non_function_reports_diagnostic() {
    let program = r#"
(export wasm) answer: i32 := 42;
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
        (export wasm) double_add := (x: i32) => (
            y := x * 2;
            y + y
        );
        {}
        "#;
    let wasm = compile(program.to_string()).expect("compilation should succeed");
    assert!(!wasm.is_empty());
}

#[test]
fn wasm_emits_assignment_updates() {
    let program = r#"
        (export wasm) increment_twice := (x: i32) => (
            mut total := x;
            total = total + 1;
            total = total + 1;
            total
        );
        {}
        "#;
    let wasm = compile(program.to_string()).expect("compilation should succeed");

    let mut instructions = Vec::new();
    for payload in Parser::new(0).parse_all(&wasm) {
        if let Payload::CodeSectionEntry(body) = payload.expect("valid wasm payload") {
            let mut reader = body.get_operators_reader().expect("operators reader");
            while let Ok(op) = reader.read() {
                if matches!(op, Operator::End) {
                    break;
                }
                instructions.push(op);
            }
        }
    }

    let has_expected_sequence = instructions.windows(4).any(|window| {
        matches!(
            window,
            [
                Operator::LocalGet { local_index: 0 },
                Operator::LocalSet { .. },
                Operator::I32Const { value: 1 },
                Operator::Drop,
            ]
        )
    }) && instructions
        .windows(4)
        .filter(|window| matches!(window[0], Operator::LocalGet { .. }))
        .any(|window| {
            matches!(
                window,
                [
                    Operator::LocalGet { local_index: 1 },
                    Operator::I32Const { value: 1 },
                    Operator::I32Add,
                    Operator::LocalTee { local_index: 1 },
                ]
            )
        });

    assert!(
        has_expected_sequence,
        "expected assignments to emit stores back to the local, got {:?}",
        instructions
    );
}

#[test]
fn wasm_supports_destructured_mut_locals() {
    let program = r#"
        (export wasm) destructure_mut := {} => (
            mut { first = a, second = b } := { first = 3, second = 4 };
            a = a + b;
            a
        );
        {}
        "#;

    let wasm = compile(program.to_string()).expect("compilation should succeed");
    assert!(!wasm.is_empty());
}
