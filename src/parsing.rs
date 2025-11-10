#[derive(Clone)]
pub struct Identifier(String);

#[derive(Clone)]
pub enum BindingPattern {
    Identifier(Identifier),
}

#[derive(Clone)]
pub enum ExpressionLiteral {
    Number(i32),
}

#[derive(Clone)]
pub enum Expression {
    Literal(ExpressionLiteral),
    Identifier(Identifier),
    Operation {
        operator: String,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    FunctionCall {
        function: Box<Expression>,
        argument: Box<Expression>,
    },
    Binding(Box<Binding>),
    Block(Vec<Expression>),
}

#[derive(Clone)]
pub struct Binding {
    pattern: BindingPattern,
    type_hint: Option<Expression>,
    expr: Expression,
}

pub fn parse_let(file: &str) -> Option<&str> {
    file.strip_prefix("let")
}

pub fn parse_eq(file: &str) -> Option<&str> {
    file.strip_prefix("=")
}

pub fn parse_type_decl(file: &str) -> Option<&str> {
    file.strip_prefix(":")
}

fn parse_semicolon(file: &str) -> Result<&str, String> {
    file.strip_prefix(";").ok_or("Expected ;".to_string())
}

pub fn parse_whitespace(file: &str) -> Option<&str> {
    file.starts_with(char::is_whitespace)
        .then(|| file.trim_start())
}

pub fn parse_optional_whitespace(file: &str) -> &str {
    file.trim_start()
}

pub fn parse_identifier(file: &str) -> Option<(Identifier, &str)> {
    let identifier = file
        .chars()
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect::<String>();

    if identifier.is_empty() {
        return None;
    }

    if identifier.chars().next().unwrap().is_digit(10) {
        return None;
    }

    let remaining = &file[identifier.len()..];
    Some((Identifier(identifier), remaining))
}

pub fn parse_literal(file: &str) -> Option<(ExpressionLiteral, &str)> {
    let number_str = file
        .chars()
        .take_while(|c| c.is_digit(10))
        .collect::<String>();

    if number_str.is_empty() {
        return None;
    }

    let number: i32 = number_str.parse().ok()?;
    let remaining = &file[number_str.len()..];
    Some((ExpressionLiteral::Number(number), remaining))
}

pub fn parse_binding_pattern(file: &str) -> Result<(BindingPattern, &str), String> {
    if let Some((identifier, remaining)) = parse_identifier(file) {
        return Ok((BindingPattern::Identifier(identifier), remaining));
    }
    Err("Expected binding pattern".to_string())
}

pub fn parse_type_hint(file: &str) -> Option<Result<(Expression, &str), String>> {
    let file = parse_optional_whitespace(file);
    let file = parse_type_decl(file)?;
    let file = parse_optional_whitespace(file);

    Some(parse_operation_expression(file))
}

pub fn parse_grouping_expression(file: &str) -> Option<(Expression, &str)> {
    let file = file.strip_prefix("(")?;
    let file = parse_optional_whitespace(file);
    let (expr, file) = parse_block(file).ok()?;
    let file = parse_optional_whitespace(file);
    let file = file.strip_prefix(")")?;
    Some((expr, file))
}

pub fn parse_operator(file: &str) -> Option<(String, &str)> {
    let operator_chars: Vec<char> = vec!['+', '-', '*', '/'];
    let operator = file
        .chars()
        .take_while(|c| operator_chars.contains(c))
        .collect::<String>();

    if operator.is_empty() {
        return None;
    }

    let remaining = &file[operator.len()..];
    Some((operator, remaining))
}

fn operator_precedence(operator: &str) -> u8 {
    match operator {
        "*" | "/" => 2,
        "+" | "-" => 1,
        _ => 1,
    }
}

pub fn parse_isolated_expression(file: &str) -> Result<(Expression, &str), String> {
    if let Some((expr, remaining)) = parse_grouping_expression(file) {
        return Ok((expr, remaining));
    }
    if let Some((identifier, remaining)) = parse_identifier(file) {
        return Ok((Expression::Identifier(identifier), remaining));
    }
    if let Some((literal, remaining)) = parse_literal(file) {
        return Ok((Expression::Literal(literal), remaining));
    }
    Err(format!(
        "Expected expression at: {}",
        file.chars().take(10).collect::<String>()
    ))
}

pub fn parse_function_call(file: &str) -> Result<(Expression, &str), String> {
    let mut exprs = vec![];
    let (function_expr, mut remaining) = parse_isolated_expression(file)?;
    remaining = parse_optional_whitespace(remaining);
    exprs.push(function_expr);
    while let Ok((argument_expr, rest)) = parse_isolated_expression(remaining) {
        remaining = parse_optional_whitespace(rest);
        exprs.push(argument_expr);
    }
    Ok((
        exprs
            .into_iter()
            .reduce(|function, argument| Expression::FunctionCall {
                function: Box::new(function),
                argument: Box::new(argument),
            })
            .unwrap(),
        remaining,
    ))
}

pub fn parse_operation_expression(file: &str) -> Result<(Expression, &str), String> {
    fn parse_operations(file: &str) -> Result<(Vec<Expression>, Vec<String>, &str), String> {
        let mut expressions: Vec<Expression> = Vec::new();
        let mut operators: Vec<String> = Vec::new();
        let (expression, mut remaining) = parse_function_call(file)?;
        remaining = parse_optional_whitespace(remaining);
        expressions.push(expression);
        while let Some((operator, rest)) = parse_operator(remaining) {
            let rest = parse_optional_whitespace(rest);
            let (next_expression, rest) = parse_function_call(rest)?;
            let rest = parse_optional_whitespace(rest);
            operators.push(operator);
            expressions.push(next_expression);
            remaining = rest;
        }
        Ok((expressions, operators, remaining))
    }

    let (expressions, operators, remaining) = parse_operations(file)?;

    fn reduce_stacks(
        operand_stack: &mut Vec<Expression>,
        operator_stack: &mut Vec<String>,
    ) -> Result<(), String> {
        let operator = operator_stack
            .pop()
            .ok_or_else(|| "Expected operator when reducing operation".to_string())?;
        let right = operand_stack
            .pop()
            .ok_or_else(|| "Expected right operand when reducing operation".to_string())?;
        let left = operand_stack
            .pop()
            .ok_or_else(|| "Expected left operand when reducing operation".to_string())?;
        operand_stack.push(Expression::Operation {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        });
        Ok(())
    }

    let mut operand_stack: Vec<Expression> = Vec::new();
    let mut operator_stack: Vec<String> = Vec::new();

    let mut expression_iter = expressions.into_iter();
    let first_expression = expression_iter
        .next()
        .ok_or_else(|| "Expected expression to start parsing operation".to_string())?;
    operand_stack.push(first_expression);

    for operator in operators {
        let next_expression = expression_iter
            .next()
            .ok_or_else(|| "Expected expression after operator".to_string())?;
        while operator_stack.last().map_or(false, |existing| {
            operator_precedence(existing) >= operator_precedence(&operator)
        }) {
            reduce_stacks(&mut operand_stack, &mut operator_stack)?;
        }
        operator_stack.push(operator);
        operand_stack.push(next_expression);
    }

    while !operator_stack.is_empty() {
        reduce_stacks(&mut operand_stack, &mut operator_stack)?;
    }

    let final_expression = operand_stack
        .pop()
        .ok_or_else(|| "Expected expression after parsing operations".to_string())?;
    Ok((final_expression, remaining))
}

pub fn parse_binding(file: &str) -> Option<Result<(Binding, &str), String>> {
    let file = parse_let(file)?;
    fn parse_binding(file: &str) -> Result<(Binding, &str), String> {
        let file = parse_whitespace(file).ok_or("Expected whitespace after let".to_string())?;
        let (pattern, file) = parse_binding_pattern(file)?;
        let (type_hint, file) = parse_type_hint(file)
            .map(|h| h.map(|(expr, file)| (Some(expr), file)))
            .unwrap_or(Ok((None, file)))?;
        let file = parse_optional_whitespace(file);
        let file = parse_eq(file).ok_or("Expected = after binding pattern".to_string())?;
        let file = parse_optional_whitespace(file);
        let (expr, file) = parse_isolated_expression(file)?;

        Ok((
            Binding {
                pattern,
                type_hint,
                expr,
            },
            file,
        ))
    }
    Some(parse_binding(file))
}

pub fn parse_individual_expression(file: &str) -> Result<(Expression, &str), String> {
    if let Some(binding_parse) = parse_binding(file) {
        return binding_parse
            .map(|(binding, remaining)| (Expression::Binding(Box::new(binding)), remaining));
    }
    parse_operation_expression(file)
}

pub fn parse_block(file: &str) -> Result<(Expression, &str), String> {
    let mut expressions = Vec::new();
    let mut remaining = file;
    remaining = parse_optional_whitespace(remaining);

    let (expression, rest) = parse_individual_expression(remaining)?;
    expressions.push(expression);
    remaining = rest;
    remaining = parse_optional_whitespace(remaining);

    while !remaining.trim().is_empty() {
        let rest = parse_semicolon(remaining)?;
        let rest = parse_optional_whitespace(rest);
        let (expression, rest) = parse_individual_expression(rest)?;
        expressions.push(expression);
        remaining = rest;
        remaining = parse_optional_whitespace(remaining);
    }

    if expressions.len() == 1 {
        return Ok((expressions.into_iter().next().unwrap(), remaining));
    }

    Ok((Expression::Block(expressions), remaining))
}

#[test]
fn parse_basic_let() {
    let parsed = parse_block(
        "
let x = 42;
let y: i32 = x
    ",
    )
    .unwrap();

    let (Expression::Block(parsed), "") = parsed else {
        panic!()
    };
    assert_eq!(parsed.len(), 2);

    let Expression::Binding(binding1) = &parsed[0] else {
        panic!()
    };
    assert_eq!(
        matches!(binding1.pattern, BindingPattern::Identifier(ref id) if id.0 == "x"),
        true
    );
    assert_eq!(
        matches!(binding1.expr, Expression::Literal(ExpressionLiteral::Number(lit)) if lit == 42),
        true
    );

    let Expression::Binding(binding2) = &parsed[1] else {
        panic!()
    };
    assert_eq!(
        matches!(binding2.pattern, BindingPattern::Identifier(ref id) if id.0 == "y"),
        true
    );
    assert_eq!(
        matches!(binding2.type_hint, Some(Expression::Identifier(ref hint)) if hint.0 == "i32"),
        true
    );
    assert_eq!(
        matches!(binding2.expr, Expression::Identifier(ref lit) if lit.0 == "x"),
        true
    );
}

#[test]
fn parse_operation_expression_precedence() {
    let (expr, remaining) = parse_operation_expression("1 + 2 * 3 / 4 - 5").unwrap();
    assert_eq!(remaining, "");
    let Expression::Operation {
        operator,
        left,
        right,
    } = expr
    else {
        panic!()
    };
    assert_eq!(operator, "-");
    assert!(matches!(
        *right,
        Expression::Literal(ExpressionLiteral::Number(5))
    ));

    let Expression::Operation {
        operator,
        left,
        right,
    } = *left
    else {
        panic!();
    };
    assert_eq!(operator, "+");
    assert!(matches!(
        *left,
        Expression::Literal(ExpressionLiteral::Number(1))
    ));

    let Expression::Operation {
        operator,
        left,
        right,
    } = *right
    else {
        panic!();
    };
    assert_eq!(operator, "/");
    assert!(matches!(
        *right,
        Expression::Literal(ExpressionLiteral::Number(4))
    ));

    let Expression::Operation {
        operator,
        left,
        right,
    } = *left
    else {
        panic!();
    };
    assert_eq!(operator, "*");
    assert!(matches!(
        *left,
        Expression::Literal(ExpressionLiteral::Number(2))
    ));
    assert!(matches!(
        *right,
        Expression::Literal(ExpressionLiteral::Number(3))
    ));
}
