use std::collections::HashMap;
use std::time::Instant;

mod diagnostics;
mod intermediate;
mod interpret;
mod js;
mod loader;
pub mod parsing;
mod silk_intermediate;
mod silk_interpreter;
mod silk_parser;
mod wasm;
mod wgsl;

#[cfg(test)]
mod wasm_parsing_tests;

pub mod test_support {
    pub use crate::intermediate::{IntermediateResult, context_to_intermediate};
    pub use crate::interpret::{
        Context, evaluate_files_to_expression, evaluate_text_to_expression, interpret_program,
        intrinsic_context, intrinsic_context_with_files,
    };
    pub use crate::wasm::compile_exports;
}

pub use diagnostics::{Diagnostic, SourceSpan};

pub fn parse_block(source: &str) -> Result<parsing::Expression, Diagnostic> {
    silk_parser::parse_block(source)
}

#[derive(Debug, Clone)]
pub struct CompilationArtifact {
    pub name: String,
    pub content: Vec<u8>,
    pub kind: ArtifactKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArtifactKind {
    Wasm,
    JS,
    Wgsl,
}

pub fn compile(
    files: Vec<(&str, &str)>,
    root: &str,
) -> Result<Vec<CompilationArtifact>, Diagnostic> {
    let timings_enabled = std::env::var_os("SILK_TIMINGS").is_some();
    let total_start = Instant::now();

    let parse_start = Instant::now();
    let mut file_sources = HashMap::with_capacity(files.len());
    for (path, source) in files {
        file_sources.insert(loader::normalize_path(path), source.to_string());
    }
    let root = loader::normalize_path(root);
    let root_source = file_sources
        .get(&root)
        .ok_or_else(|| Diagnostic::new(format!("Missing root source for {root}")))?
        .clone();
    if timings_enabled {
        eprintln!(
            "SILK_TIMINGS parse_files_ms={:.2}",
            parse_start.elapsed().as_secs_f64() * 1_000.0
        );
    }

    let interpret_start = Instant::now();
    let eval_inputs: Vec<(&str, &str)> = file_sources
        .iter()
        .map(|(path, source)| (path.as_str(), source.as_str()))
        .collect();
    let (_evaluated_value, program_context) =
        interpret::evaluate_files_to_expression(eval_inputs, &root)?;
    if timings_enabled {
        eprintln!(
            "SILK_TIMINGS interpret_ms={:.2}",
            interpret_start.elapsed().as_secs_f64() * 1_000.0
        );
    }

    let intermediate_start = Instant::now();
    let (intermediate, intermediate_backend) =
        silk_intermediate::lower_context(&program_context, &root_source)?;
    if timings_enabled {
        eprintln!(
            "SILK_TIMINGS lower_intermediate_ms={:.2} backend={}",
            intermediate_start.elapsed().as_secs_f64() * 1_000.0,
            intermediate_backend.as_str(),
        );
    }

    let mut artifacts = Vec::new();

    if intermediate
        .exports
        .iter()
        .any(|e| matches!(e.target, parsing::TargetLiteral::WasmTarget))
        || intermediate
            .wrappers
            .iter()
            .any(|w| matches!(w.wrap_target, parsing::TargetLiteral::WasmTarget))
    {
        let wasm_start = Instant::now();
        let content = wasm::compile_exports(&intermediate)?;
        if timings_enabled {
            eprintln!(
                "SILK_TIMINGS compile_wasm_ms={:.2}",
                wasm_start.elapsed().as_secs_f64() * 1_000.0
            );
        }
        artifacts.push(CompilationArtifact {
            name: "main".to_string(), // TODO: use file name
            content,
            kind: ArtifactKind::Wasm,
        });
    }

    if intermediate
        .exports
        .iter()
        .any(|e| matches!(e.target, parsing::TargetLiteral::JSTarget))
        || intermediate
            .wrappers
            .iter()
            .any(|w| matches!(w.wrap_target, parsing::TargetLiteral::JSTarget))
        || intermediate.wrappers.iter().any(|w| {
            matches!(
                (w.wrap_target.clone(), w.source_target.clone()),
                (
                    parsing::TargetLiteral::WasmTarget,
                    parsing::TargetLiteral::JSTarget
                ) | (
                    parsing::TargetLiteral::WasmTarget,
                    parsing::TargetLiteral::WgslTarget
                )
            )
        })
    {
        let js_start = Instant::now();
        let content = js::compile_exports(&intermediate)?;
        if timings_enabled {
            eprintln!(
                "SILK_TIMINGS compile_js_ms={:.2}",
                js_start.elapsed().as_secs_f64() * 1_000.0
            );
        }
        artifacts.push(CompilationArtifact {
            name: "main".to_string(), // TODO: use file name
            content: content.into_bytes(),
            kind: ArtifactKind::JS,
        });
    }

    if intermediate
        .exports
        .iter()
        .any(|e| matches!(e.target, parsing::TargetLiteral::WgslTarget))
    {
        let wgsl_start = Instant::now();
        let content = wgsl::compile_exports(&intermediate)?;
        if timings_enabled {
            eprintln!(
                "SILK_TIMINGS compile_wgsl_ms={:.2}",
                wgsl_start.elapsed().as_secs_f64() * 1_000.0
            );
        }
        artifacts.push(CompilationArtifact {
            name: "main".to_string(), // TODO: use file name
            content: content.into_bytes(),
            kind: ArtifactKind::Wgsl,
        });
    }

    if timings_enabled {
        eprintln!(
            "SILK_TIMINGS total_ms={:.2}",
            total_start.elapsed().as_secs_f64() * 1_000.0
        );
    }

    Ok(artifacts)
}
