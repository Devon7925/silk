use crate::parsing::Expression;

struct Context {
    bindings: std::collections::HashMap<String, Expression>,
}

fn interpret_expression(expr: Expression, context: &Context) -> Result<Expression, String> {
    match expr {
        expr @ Expression::Literal(_) => Ok(expr),
        Expression::Identifier(identifier) => todo!(),
        Expression::Operation { operator, left, right } => todo!(),
        Expression::Binding(binding) => todo!(),
        Expression::Block(expressions) => todo!(),
    }
}