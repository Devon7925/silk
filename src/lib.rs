mod parsing;
mod interpret;

pub fn compile(file: String) -> Result<Vec<u8>, String> {
    let (ast, remaining) = parsing::parse_block(&file)?;
    assert!(remaining.is_empty(), "Unexpected remaining input: {}", remaining);
    let mut context = interpret::intrinsic_context();
    interpret::interpret_expression(ast, &mut context)?;
    return Ok(vec![]);
}
