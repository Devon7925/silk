use silk::compile;

fn compile_program(program: &str) -> Vec<u8> {
    compile(vec![("main.silk", program)], "main.silk").expect("compilation should succeed")
}
use wasmparser::{Parser, Validator, WasmFeatures};

fn validate_wasm(bytes: &[u8]) {
    let mut features = WasmFeatures::default();
    features.set(WasmFeatures::GC, true);
    features.set(WasmFeatures::REFERENCE_TYPES, true);

    let mut validator = Validator::new_with_features(features);
    if let Err(err) = validator.validate_all(bytes) {
        eprintln!("wasm validation failed: {err}");
        for payload in Parser::new(0).parse_all(bytes) {
            if let Ok(wasmparser::Payload::CodeSectionEntry(body)) = payload {
                let mut reader = body.get_operators_reader().expect("operators");
                while let Ok(op) = reader.read() {
                    eprintln!("op: {:?}", op);
                }
            }
        }

        panic!("wasm should validate");
    }
}

#[test]
fn wasm_loop_with_internal_return_validates() {
    let program = r#"
(export wasm) factorial := (limit: i32) => (
    mut acc := 1;
    mut iter := limit;
    loop (
        acc = acc * iter;
        iter = iter - 1;
        if iter <= 0 then (
            return acc;
        )
    );
    1
);
{};
"#;

    let wasm = compile_program(program);
    validate_wasm(&wasm);
}

#[test]
fn wasm_loop_with_break_value_validates() {
    let program = r#"
(export wasm) first_non_positive := (start: i32) => (
    mut current := start;
    loop (
        if current <= 0 then (
            break current;
        );
        current = current - 1;
    )
);
{};
"#;

    let wasm = compile_program(program);
    validate_wasm(&wasm);
}
