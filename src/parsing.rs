use crate::diagnostics::{Diagnostic, SourceSpan};

#[derive(Clone, Debug)]
pub struct Identifier(pub String);

#[derive(Clone, Debug)]
pub enum BindingPattern {
    Identifier(Identifier),
    Struct(Vec<(Identifier, BindingPattern)>),
    TypeHint(Box<BindingPattern>, Box<Expression>),
}

#[derive(Clone, Debug)]
pub enum ExpressionLiteral {
    Number(i32),
}

#[derive(Clone, Debug)]
pub enum IntrinsicType {
    I32,
    Type,
}

#[derive(Clone, Debug)]
pub enum BinaryIntrinsicOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Clone, Debug)]
pub enum IntrinsicOperation {
    Binary(Box<Expression>, Box<Expression>, BinaryIntrinsicOperator),
}

#[derive(Clone, Debug)]
pub enum Expression {
    IntrinsicType(IntrinsicType),
    IntrinsicOperation(IntrinsicOperation),
    AttachImplementation {
        type_expr: Box<Expression>,
        implementation: Box<Expression>,
    },
    Function {
        parameter: BindingPattern,
        return_type: Box<Expression>,
        body: Box<Expression>,
    },
    FunctionType {
        parameter: Box<Expression>,
        return_type: Box<Expression>,
    },
    Struct(Vec<(Identifier, Expression)>),
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
    PropertyAccess {
        object: Box<Expression>,
        property: String,
    },
    Binding(Box<Binding>),
    Block(Vec<Expression>),
}

#[derive(Clone, Debug)]
pub struct Binding {
    pub pattern: BindingPattern,
    pub expr: Expression,
}

fn remainder_span(source: &str, remaining: &str, len: usize) -> SourceSpan {
    let start = source
        .len()
        .checked_sub(remaining.len())
        .expect("remaining slice should originate from source");
    let available = source.len().saturating_sub(start);
    let clamped_len = len.min(available);
    SourceSpan::new(start, clamped_len)
}

fn diagnostic_here(
    source: &str,
    remaining: &str,
    len: usize,
    message: impl Into<String>,
) -> Diagnostic {
    Diagnostic::new(message).with_span(remainder_span(source, remaining, len))
}

fn diagnostic_at_eof(source: &str, message: impl Into<String>) -> Diagnostic {
    let eof = &source[source.len()..];
    diagnostic_here(source, eof, 0, message)
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

fn parse_semicolon<'a>(source: &'a str, file: &'a str) -> Result<&'a str, Diagnostic> {
    file
        .strip_prefix(";")
        .ok_or_else(|| diagnostic_here(source, file, 1, "Expected ;"))
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

fn parse_simple_binding_pattern<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(BindingPattern, &'a str), Diagnostic> {
    if let Some((identifier, remaining)) = parse_identifier(file) {
        return Ok((BindingPattern::Identifier(identifier), remaining));
    }
    Err(diagnostic_here(source, file, 1, "Expected binding pattern"))
}

fn parse_type_hint<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(Expression, &'a str), Diagnostic>> {
    let file = parse_optional_whitespace(file);
    let file = parse_type_decl(file)?;
    let file = parse_optional_whitespace(file);

    Some(parse_operation_expression_with_source(source, file))
}

fn parse_binding_pattern_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(BindingPattern, &'a str), Diagnostic> {
    let file = parse_optional_whitespace(file);
    let (pattern, file) = if file.starts_with("{") {
        parse_struct_binding_pattern_with_source(source, file)?
    } else {
        parse_simple_binding_pattern(source, file)?
    };
    if let Some(type_hint_parse) = parse_type_hint(source, file) {
        let (type_expr, remaining) = type_hint_parse?;
        return Ok((
            BindingPattern::TypeHint(Box::new(pattern), Box::new(type_expr)),
            remaining,
        ));
    }

    Ok((pattern, file))
}

#[cfg(test)]
pub fn parse_struct_binding_pattern(file: &str) -> Result<(BindingPattern, &str), Diagnostic> {
    parse_struct_binding_pattern_with_source(file, file)
}

fn parse_struct_binding_pattern_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(BindingPattern, &'a str), Diagnostic> {
    let mut remaining = file
        .strip_prefix("{")
        .ok_or_else(|| diagnostic_here(source, file, 1, "Expected { to start struct binding pattern"))?;
    let mut fields = Vec::new();
    let mut tuple_index = 0usize;

    loop {
        remaining = parse_optional_whitespace(remaining);

        if let Some(rest) = remaining.strip_prefix("}") {
            return Ok((BindingPattern::Struct(fields), rest));
        }

        if let Some((field_identifier, after_identifier)) = parse_identifier(remaining) {
            let after_ws = parse_optional_whitespace(after_identifier);
            if let Some(rest_after_equals) = after_ws.strip_prefix("=") {
                let rest_after_equals = parse_optional_whitespace(rest_after_equals);
                let (field_pattern, rest) = parse_binding_pattern_with_source(source, rest_after_equals)?;
                fields.push((field_identifier, field_pattern));
                remaining = parse_optional_whitespace(rest);
            } else {
                let (field_pattern, rest) =
                    parse_binding_pattern_with_source(source, remaining)?;
                fields.push((Identifier(tuple_index.to_string()), field_pattern));
                tuple_index += 1;
                remaining = parse_optional_whitespace(rest);
            }
        } else {
            let (field_pattern, rest) = parse_binding_pattern_with_source(source, remaining)?;
            fields.push((Identifier(tuple_index.to_string()), field_pattern));
            tuple_index += 1;
            remaining = parse_optional_whitespace(rest);
        }

        if let Some(rest) = remaining.strip_prefix("}") {
            return Ok((BindingPattern::Struct(fields), rest));
        }

        remaining = remaining
            .strip_prefix(",")
            .ok_or_else(|| {
                diagnostic_here(
                    source,
                    remaining,
                    1,
                    "Expected , or } in struct binding pattern",
                )
            })?;
    }
}

fn parse_grouping_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(Expression, &'a str), Diagnostic>> {
    let file = file.strip_prefix("(")?;
    let (expr, file) = match parse_block_with_terminators(source, file, &[')']) {
        Ok(result) => result,
        Err(err) => return Some(Err(err)),
    };
    let Some(file) = file.strip_prefix(")") else {
        return Some(Err(diagnostic_here(
            source,
            file,
            1,
            "Expected ) to close grouping expression",
        )));
    };
    Some(Ok((expr, file)))
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

fn parse_function_literal<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(Expression, &'a str), Diagnostic>> {
    fn parse_function_literal_inner<'a>(
        source: &'a str,
        file: &'a str,
    ) -> Result<(Expression, &'a str), Diagnostic> {
        let file = parse_optional_whitespace(file);
        let (parameter, file) = parse_function_parameter(source, file)?;
        let file = parse_optional_whitespace(file);
        let file = file.strip_prefix("->").ok_or_else(|| {
            diagnostic_here(source, file, 2, "Expected -> after function parameter")
        })?;
        let file = parse_optional_whitespace(file);
        let (return_type, file) = parse_operation_expression_with_source(source, file)?;
        let file = parse_optional_whitespace(file);
        let (return_type, body, file) = if file.starts_with("(") {
            match parse_grouping_expression_with_source(source, file) {
                Some(Ok((body, file))) => (return_type, body, file),
                Some(Err(err)) => return Err(err),
                None => {
                    return Err(diagnostic_here(
                        source,
                        file,
                        1,
                        "Expected function body expression",
                    ))
                }
            }
        } else {
            match return_type {
                Expression::FunctionCall { function, argument } => (*function, *argument, file),
                _ => {
                    return Err(diagnostic_here(
                        source,
                        file,
                        1,
                        "Expected function body expression",
                    ))
                }
            }
        };
        Ok((
            Expression::Function {
                parameter,
                return_type: Box::new(return_type),
                body: Box::new(body),
            },
            file,
        ))
    }

    let remaining = file.strip_prefix("fn")?;
    if remaining
        .chars()
        .next()
        .filter(|c| c.is_alphanumeric() || *c == '_')
        .is_some()
    {
        return None;
    }
    Some(parse_function_literal_inner(source, remaining))
}

fn parse_function_parameter<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(BindingPattern, &'a str), Diagnostic> {
    let file = parse_optional_whitespace(file);
    if let Some(remaining) = file.strip_prefix("(") {
        let remaining = parse_optional_whitespace(remaining);
        let (pattern, remaining) = parse_binding_pattern_with_source(source, remaining)?;
        let remaining = parse_optional_whitespace(remaining);
        let remaining = remaining.strip_prefix(")").ok_or_else(|| {
            diagnostic_here(source, remaining, 1, "Expected ) after function parameter")
        })?;
        Ok((pattern, remaining))
    } else if file.starts_with("{") {
        parse_struct_binding_pattern_with_source(source, file)
    } else {
        Err(diagnostic_here(
            source,
            file,
            2,
            "Expected function parameter after fn",
        ))
    }
}

fn parse_struct_expression<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(Expression, &'a str), Diagnostic>> {
    fn parse_struct_expression_inner<'a>(
        source: &'a str,
        file: &'a str,
    ) -> Result<(Expression, &'a str), Diagnostic> {
        let mut remaining = file.strip_prefix("{").ok_or_else(|| {
            diagnostic_here(source, file, 1, "Expected { to start struct literal")
        })?;
        let mut items = Vec::new();
        let mut tuple_index = 0usize;

        loop {
            remaining = parse_optional_whitespace(remaining);

            if let Some(rest) = remaining.strip_prefix("}") {
                return Ok((Expression::Struct(items), rest));
            }

            if let Some((identifier, after_identifier)) = parse_identifier(remaining) {
                let after_ws = parse_optional_whitespace(after_identifier);
                if let Some(after_equals) = after_ws.strip_prefix("=") {
                    let after_ws = parse_optional_whitespace(after_equals);
                    let (value_expr, rest) = parse_operation_expression_with_source(source, after_ws)?;
                    items.push((identifier, value_expr));
                    remaining = rest;
                    remaining = parse_optional_whitespace(remaining);

                    if let Some(rest) = remaining.strip_prefix("}") {
                        return Ok((Expression::Struct(items), rest));
                    }

                    remaining = remaining.strip_prefix(",").ok_or_else(|| {
                        diagnostic_here(
                            source,
                            remaining,
                            1,
                            "Expected , or } in struct literal",
                        )
                    })?;
                    continue;
                }
            }

            let (value_expr, rest) = parse_operation_expression_with_source(source, remaining)?;
            items.push((Identifier(tuple_index.to_string()), value_expr));
            tuple_index += 1;
            remaining = rest;
            remaining = parse_optional_whitespace(remaining);

            if let Some(rest) = remaining.strip_prefix("}") {
                return Ok((Expression::Struct(items), rest));
            }

            remaining = remaining.strip_prefix(",").ok_or_else(|| {
                diagnostic_here(
                    source,
                    remaining,
                    1,
                    "Expected , or } in struct literal",
                )
            })?;
        }
    }

    if !file.starts_with("{") {
        return None;
    }
    Some(parse_struct_expression_inner(source, file))
}

#[cfg(test)]
pub fn parse_isolated_expression(file: &str) -> Result<(Expression, &str), Diagnostic> {
    parse_isolated_expression_with_source(file, file)
}

fn parse_isolated_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(Expression, &'a str), Diagnostic> {
    if let Some(function_parse) = parse_function_literal(source, file) {
        return function_parse;
    }
    if let Some(group_parse) = parse_grouping_expression_with_source(source, file) {
        return group_parse;
    }
    if let Some(struct_parse) = parse_struct_expression(source, file) {
        return struct_parse;
    }
    if let Some((identifier, remaining)) = parse_identifier(file) {
        return Ok((Expression::Identifier(identifier), remaining));
    }
    if let Some((literal, remaining)) = parse_literal(file) {
        return Ok((Expression::Literal(literal), remaining));
    }
    let preview = file.chars().take(10).collect::<String>();
    if file.is_empty() {
        Err(diagnostic_at_eof(
            source,
            format!("Expected expression near end of input"),
        ))
    } else {
        Err(diagnostic_here(
            source,
            file,
            file.chars().next().map(|c| c.len_utf8()).unwrap_or(1),
            format!("Expected expression at: {preview}"),
        ))
    }
}

fn parse_property_access<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(Expression, &'a str), Diagnostic> {
    let (mut expr, mut remaining) = parse_isolated_expression_with_source(source, file)?;

    loop {
        let lookahead = parse_optional_whitespace(remaining);
        let Some(after_dot) = lookahead.strip_prefix(".") else {
            break;
        };
        let after_dot = parse_optional_whitespace(after_dot);
        let (property_identifier, rest) = parse_identifier(after_dot).ok_or_else(|| {
            diagnostic_here(
                source,
                after_dot,
                after_dot
                    .chars()
                    .next()
                    .map(|c| c.len_utf8())
                    .unwrap_or(1),
                "Expected identifier after . in property access",
            )
        })?;
        expr = Expression::PropertyAccess {
            object: Box::new(expr),
            property: property_identifier.0,
        };
        remaining = rest;
    }

    Ok((expr, remaining))
}

fn parse_function_call<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(Expression, &'a str), Diagnostic> {
    let mut exprs = vec![];
    let (function_expr, mut remaining) = parse_property_access(source, file)?;
    remaining = parse_optional_whitespace(remaining);
    exprs.push(function_expr);
    while let Ok((argument_expr, rest)) = parse_property_access(source, remaining) {
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

#[cfg(test)]
pub fn parse_operation_expression(file: &str) -> Result<(Expression, &str), Diagnostic> {
    parse_operation_expression_with_source(file, file)
}

fn parse_operation_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(Expression, &'a str), Diagnostic> {
    fn parse_operations<'a>(
        source: &'a str,
        file: &'a str,
    ) -> Result<(Vec<Expression>, Vec<String>, &'a str), Diagnostic> {
        let mut expressions: Vec<Expression> = Vec::new();
        let mut operators: Vec<String> = Vec::new();
        let (expression, mut remaining) = parse_function_call(source, file)?;
        remaining = parse_optional_whitespace(remaining);
        expressions.push(expression);
        while let Some((operator, rest)) = parse_operator(remaining) {
            let rest = parse_optional_whitespace(rest);
            let (next_expression, rest) = parse_function_call(source, rest)?;
            let rest = parse_optional_whitespace(rest);
            operators.push(operator);
            expressions.push(next_expression);
            remaining = rest;
        }
        Ok((expressions, operators, remaining))
    }

    let (expressions, operators, remaining) = parse_operations(source, file)?;

    fn reduce_stacks(
        operand_stack: &mut Vec<Expression>,
        operator_stack: &mut Vec<String>,
        source: &str,
    ) -> Result<(), Diagnostic> {
        let operator = operator_stack
            .pop()
            .ok_or_else(|| diagnostic_at_eof(source, "Expected operator when reducing operation"))?;
        let right = operand_stack
            .pop()
            .ok_or_else(|| {
                diagnostic_at_eof(source, "Expected right operand when reducing operation")
            })?;
        let left = operand_stack
            .pop()
            .ok_or_else(|| {
                diagnostic_at_eof(source, "Expected left operand when reducing operation")
            })?;
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
        .ok_or_else(|| diagnostic_at_eof(source, "Expected expression to start parsing operation"))?;
    operand_stack.push(first_expression);

    for operator in operators {
        let next_expression = expression_iter
            .next()
            .ok_or_else(|| diagnostic_at_eof(source, "Expected expression after operator"))?;
        while operator_stack.last().map_or(false, |existing| {
            operator_precedence(existing) >= operator_precedence(&operator)
        }) {
            reduce_stacks(&mut operand_stack, &mut operator_stack, source)?;
        }
        operator_stack.push(operator);
        operand_stack.push(next_expression);
    }

    while !operator_stack.is_empty() {
        reduce_stacks(&mut operand_stack, &mut operator_stack, source)?;
    }

    let final_expression = operand_stack
        .pop()
        .ok_or_else(|| diagnostic_at_eof(source, "Expected expression after parsing operations"))?;
    Ok((final_expression, remaining))
}

fn parse_binding<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(Binding, &'a str), Diagnostic>> {
    let file = parse_let(file)?;
    fn parse_binding_inner<'a>(
        source: &'a str,
        file: &'a str,
    ) -> Result<(Binding, &'a str), Diagnostic> {
        let file = parse_whitespace(file)
            .ok_or_else(|| diagnostic_here(source, file, 1, "Expected whitespace after let"))?;
        let (pattern, file) = parse_binding_pattern_with_source(source, file)?;
        let file = parse_optional_whitespace(file);
        let file = parse_eq(file)
            .ok_or_else(|| diagnostic_here(source, file, 1, "Expected = after binding pattern"))?;
        let file = parse_optional_whitespace(file);
        let (expr, file) = parse_isolated_expression_with_source(source, file)?;

        Ok((
            Binding {
                pattern,
                expr,
            },
            file,
        ))
    }
    Some(parse_binding_inner(source, file))
}

fn parse_individual_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(Expression, &'a str), Diagnostic> {
    if let Some(binding_parse) = parse_binding(source, file) {
        let (binding, remaining) = binding_parse?;
        return Ok((Expression::Binding(Box::new(binding)), remaining));
    }
    parse_operation_expression_with_source(source, file)
}

pub fn parse_block(file: &str) -> Result<(Expression, &str), Diagnostic> {
    parse_block_with_terminators(file, file, &[])
}

fn parse_block_with_terminators<'a>(
    source: &'a str,
    file: &'a str,
    terminators: &[char],
) -> Result<(Expression, &'a str), Diagnostic> {
    let mut expressions = Vec::new();
    let mut remaining = parse_optional_whitespace(file);

    loop {
        if remaining.is_empty() {
            break;
        }

        if let Some(ch) = remaining.chars().next() {
            if terminators.contains(&ch) {
                break;
            }
        }

        let (expression, rest) = parse_individual_expression_with_source(source, remaining)?;
        expressions.push(expression);
        remaining = parse_optional_whitespace(rest);

        if remaining.is_empty() {
            break;
        }

        if let Some(ch) = remaining.chars().next() {
            if terminators.contains(&ch) {
                break;
            }
        }

        let rest = parse_semicolon(source, remaining)?;
        remaining = parse_optional_whitespace(rest);
    }

    if expressions.is_empty() {
        return Err(diagnostic_here(
            source,
            remaining,
            remaining.chars().next().map(|c| c.len_utf8()).unwrap_or(0),
            "Cannot parse empty block",
        ));
    }

    if expressions.len() == 1 {
        Ok((expressions.into_iter().next().unwrap(), remaining))
    } else {
        Ok((Expression::Block(expressions), remaining))
    }
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
        matches!(binding2.expr, Expression::Identifier(ref lit) if lit.0 == "x"),
        true
    );
    let BindingPattern::TypeHint(binding2, binding2_type) = &binding2.pattern else {
        panic!()
    };
    assert!(matches!(**binding2, BindingPattern::Identifier(ref hint) if hint.0 == "y"));
    assert_eq!(
        matches!(**binding2_type, Expression::Identifier(ref hint) if hint.0 == "i32"),
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

#[test]
fn parse_function_binding_and_call() {
    let (expr, remaining) = parse_block(
        "
let foo = fn(bar: i32) -> i32 (
    bar + 1
);
foo(123)
    ",
    )
    .unwrap();

    assert!(remaining.trim().is_empty());

    let Expression::Block(items) = expr else {
        panic!("expected block with binding and call");
    };
    assert_eq!(items.len(), 2);

    let Expression::Binding(binding) = &items[0] else {
        panic!("first expression should be binding");
    };
    assert!(matches!(
        binding.pattern,
        BindingPattern::Identifier(Identifier(ref name)) if name == "foo"
    ));

    let Expression::Function {
        parameter,
        return_type,
        body,
    } = &binding.expr
    else {
        panic!("binding should store function expression");
    };

    let BindingPattern::TypeHint(inner, type_hint) = parameter else {
        panic!("parameter should include type hint");
    };
    assert!(matches!(
        **inner,
        BindingPattern::Identifier(Identifier(ref name)) if name == "bar"
    ));
    assert!(matches!(
        **type_hint,
        Expression::Identifier(Identifier(ref name)) if name == "i32"
    ));
    assert!(matches!(
        **return_type,
        Expression::Identifier(Identifier(ref name)) if name == "i32"
    ));
    assert!(matches!(
        **body,
        Expression::Operation {
            operator: ref op,
            ..
        } if op == "+"
    ));

    let Expression::FunctionCall { function, argument } = &items[1] else {
        panic!("expected function call as second expression");
    };
    assert!(matches!(
        **function,
        Expression::Identifier(Identifier(ref name)) if name == "foo"
    ));
    assert!(matches!(
        **argument,
        Expression::Literal(ExpressionLiteral::Number(123))
    ));
}

#[test]
fn parse_function_struct_parameter_pattern() {
    let (expr, remaining) = parse_isolated_expression(
        "fn{bar1: i32, bar2: i32} -> i32 (
    bar1 + bar2
)
    ",
    )
    .unwrap();
    assert!(remaining.trim().is_empty());

    let Expression::Function { parameter, .. } = expr else {
        panic!("expected function expression");
    };
    let BindingPattern::Struct(fields) = parameter else {
        panic!("expected struct binding pattern for function parameter");
    };
    assert_eq!(fields.len(), 2);

    let (first_name, first_pattern) = &fields[0];
    assert_eq!(first_name.0, "0");
    let BindingPattern::TypeHint(first_inner, first_type) = first_pattern else {
        panic!("expected type hint for first parameter");
    };
    assert!(matches!(
        **first_inner,
        BindingPattern::Identifier(Identifier(ref name)) if name == "bar1"
    ));
    assert!(matches!(
        **first_type,
        Expression::Identifier(Identifier(ref name)) if name == "i32"
    ));

    let (second_name, second_pattern) = &fields[1];
    assert_eq!(second_name.0, "1");
    let BindingPattern::TypeHint(second_inner, second_type) = second_pattern else {
        panic!("expected type hint for second parameter");
    };
    assert!(matches!(
        **second_inner,
        BindingPattern::Identifier(Identifier(ref name)) if name == "bar2"
    ));
    assert!(matches!(
        **second_type,
        Expression::Identifier(Identifier(ref name)) if name == "i32"
    ));
}

#[test]
fn parse_struct_literal_named_and_tuple_fields() {
    let (expr, remaining) = parse_isolated_expression("{foo = 10, 20, bar = 30}").unwrap();
    assert!(remaining.trim().is_empty());
    let Expression::Struct(items) = expr else {
        panic!("expected struct literal");
    };
    assert_eq!(items.len(), 3);
    assert_eq!(items[0].0 .0, "foo");
    assert_eq!(items[1].0 .0, "0");
    assert_eq!(items[2].0 .0, "bar");
}

#[test]
fn parse_struct_binding_pattern_named_fields() {
    let (pattern, remaining) =
        parse_struct_binding_pattern("{foo = first: i32, second: i32}").expect("pattern parse");
    assert!(remaining.trim().is_empty());
    let BindingPattern::Struct(fields) = pattern else {
        panic!("expected struct pattern");
    };
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].0 .0, "foo");
    assert_eq!(fields[1].0 .0, "0");
}

#[test]
fn parse_struct_property_access_chain() {
    let (expr, remaining) = parse_operation_expression("foo.bar.baz").expect("parse");
    assert!(remaining.trim().is_empty());

    let Expression::PropertyAccess { object, property } = expr else {
        panic!("expected outer property access");
    };
    assert_eq!(property, "baz");

    let Expression::PropertyAccess {
        object: inner_object,
        property: inner_property,
    } = *object
    else {
        panic!("expected inner property access");
    };
    assert_eq!(inner_property, "bar");
    assert!(matches!(
        *inner_object,
        Expression::Identifier(Identifier(ref name)) if name == "foo"
    ));
}

#[test]
fn parse_struct_property_access_then_call() {
    let (expr, remaining) = parse_operation_expression("foo.bar baz").expect("parse");
    assert!(remaining.trim().is_empty());

    let Expression::FunctionCall { function, argument } = expr else {
        panic!("expected function call");
    };
    assert!(matches!(
        *argument,
        Expression::Identifier(Identifier(ref name)) if name == "baz"
    ));
    let Expression::PropertyAccess { object, property } = *function else {
        panic!("expected property access as function part");
    };
    assert_eq!(property, "bar");
    assert!(matches!(
        *object,
        Expression::Identifier(Identifier(ref name)) if name == "foo"
    ));
}

#[test]
fn diagnostics_include_binding_pattern_source_reference() {
    let source = "let = 5;";
    let err = parse_block(source).expect_err("binding should fail");
    let rendered = err.render_with_source(source);
    assert!(rendered.contains("Expected binding pattern"));
    assert!(rendered.contains("line 1, column 5"));
    assert!(rendered.contains("^"));
}

#[test]
fn diagnostics_include_grouping_closure_reference() {
    let source = "(1 + 2";
    let err = parse_isolated_expression(source).expect_err("missing ) should fail");
    let rendered = err.render_with_source(source);
    assert!(rendered.contains("Expected ) to close grouping expression"));
    assert!(rendered.contains("line 1, column 7"));
    assert!(rendered.contains("^"));
}
