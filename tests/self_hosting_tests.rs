use std::fs;

use silk::parse_block;
use silk::test_support::evaluate_files_to_expression;

fn read(path: &str) -> String {
    fs::read_to_string(path).unwrap_or_else(|err| panic!("failed to read {path}: {err}"))
}

#[test]
#[ignore = "expensive self-hosting check"]
fn parser_silk_self_parse_and_interpret() {
    let parser_source = read("silk_src/parser.silk");
    let types_source = read("silk_src/types.silk");

    parse_block(&parser_source).unwrap_or_else(|err| {
        panic!(
            "parser.silk parse failed:\n{}",
            err.render_with_source(&parser_source)
        )
    });

    evaluate_files_to_expression(
        vec![
            ("parser.silk", parser_source.as_str()),
            ("types.silk", types_source.as_str()),
        ],
        "parser.silk",
    )
    .unwrap_or_else(|err| {
        panic!(
            "parser.silk interpret failed:\nmessage: {}\nrender parser.silk:\n{}\nrender types.silk:\n{}",
            err.message,
            err.render_with_source(&parser_source),
            err.render_with_source(&types_source)
        )
    });
}

#[test]
#[ignore = "expensive self-hosting check"]
fn interpreter_silk_self_parse_and_interpret() {
    let interpreter_source = read("silk_src/interpreter.silk");
    let types_source = read("silk_src/types.silk");

    parse_block(&interpreter_source).unwrap_or_else(|err| {
        panic!(
            "interpreter.silk parse failed:\n{}",
            err.render_with_source(&interpreter_source)
        )
    });

    evaluate_files_to_expression(
        vec![
            ("interpreter.silk", interpreter_source.as_str()),
            ("types.silk", types_source.as_str()),
        ],
        "interpreter.silk",
    )
    .unwrap_or_else(|err| {
        panic!(
            "interpreter.silk interpret failed:\nmessage: {}\nrender interpreter.silk:\n{}\nrender types.silk:\n{}",
            err.message,
            err.render_with_source(&interpreter_source),
            err.render_with_source(&types_source)
        )
    });
}

#[test]
#[ignore = "expensive self-hosting check"]
fn types_silk_import_only() {
    let types_source = read("silk_src/types.silk");
    let root = "types := use \"types.silk\"; {}";

    evaluate_files_to_expression(
        vec![("main.silk", root), ("types.silk", types_source.as_str())],
        "main.silk",
    )
    .unwrap_or_else(|err| {
        panic!(
            "types import failed:\nmessage: {}\nrender main.silk:\n{}\nrender types.silk:\n{}",
            err.message,
            err.render_with_source(root),
            err.render_with_source(&types_source)
        )
    });
}
