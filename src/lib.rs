mod diagnostics;
mod intermediate;
mod interpret;
mod loader;
pub mod parsing;
mod uniquify;
mod wasm;

pub mod test_support {
    pub use crate::intermediate::{IntermediateResult, context_to_intermediate};
    pub use crate::interpret::{
        Context, evaluate_files_to_expression, evaluate_text_to_expression, interpret_program,
        intrinsic_context, intrinsic_context_with_files,
    };
    pub use crate::wasm::compile_exports;
}

pub use diagnostics::{Diagnostic, SourceSpan};

pub fn compile(files: Vec<(&str, &str)>, root: &str) -> Result<Vec<u8>, Diagnostic> {
    let file_map = loader::build_parsed_files(files)?;
    let root = loader::normalize_path(root);
    let ast = file_map
        .get(&root)
        .ok_or_else(|| Diagnostic::new(format!("Missing root source for {root}")))?
        .clone();
    let mut context = interpret::intrinsic_context_with_files(file_map);
    let (_value, program_context) = interpret::interpret_program(ast, &mut context)?;
    let intermediate = intermediate::context_to_intermediate(&program_context);
    wasm::compile_exports(&intermediate)
}
