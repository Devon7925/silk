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
    Binding(Box<Binding>),
    Block(Vec<Expression>),
}

#[derive(Clone, Debug)]
pub struct Binding {
    pub pattern: BindingPattern,
    pub expr: Expression,
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

pub fn parse_simple_binding_pattern(file: &str) -> Result<(BindingPattern, &str), String> {
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

pub fn parse_binding_pattern(file: &str) -> Result<(BindingPattern, &str), String> {
    let file = parse_optional_whitespace(file);
    let (pattern, file) = if file.starts_with("{") {
        parse_struct_binding_pattern(file)?
    } else {
        parse_simple_binding_pattern(file)?
    };
    if let Some(type_hint_parse) = parse_type_hint(file) {
        let (type_expr, remaining) = type_hint_parse?;
        return Ok((
            BindingPattern::TypeHint(Box::new(pattern), Box::new(type_expr)),
            remaining,
        ));
    }

    Ok((pattern, file))
}

fn parse_struct_binding_pattern(file: &str) -> Result<(BindingPattern, &str), String> {
    let mut remaining = file
        .strip_prefix("{")
        .ok_or_else(|| "Expected { to start struct binding pattern".to_string())?;
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
                let (field_pattern, rest) = parse_binding_pattern(rest_after_equals)?;
                fields.push((field_identifier, field_pattern));
                remaining = parse_optional_whitespace(rest);
            } else {
                let (field_pattern, rest) = parse_binding_pattern(remaining)?;
                fields.push((Identifier(tuple_index.to_string()), field_pattern));
                tuple_index += 1;
                remaining = parse_optional_whitespace(rest);
            }
        } else {
            let (field_pattern, rest) = parse_binding_pattern(remaining)?;
            fields.push((Identifier(tuple_index.to_string()), field_pattern));
            tuple_index += 1;
            remaining = parse_optional_whitespace(rest);
        }

        if let Some(rest) = remaining.strip_prefix("}") {
            return Ok((BindingPattern::Struct(fields), rest));
        }

        remaining = remaining
            .strip_prefix(",")
            .ok_or_else(|| "Expected , or } in struct binding pattern".to_string())?;
    }
}

pub fn parse_grouping_expression(file: &str) -> Option<(Expression, &str)> {
    let file = file.strip_prefix("(")?;
    let (expr, file) = parse_block_with_terminators(file, &[')']).ok()?;
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

fn parse_function_literal(file: &str) -> Option<Result<(Expression, &str), String>> {
    fn parse_function_literal_inner(file: &str) -> Result<(Expression, &str), String> {
        let file = parse_optional_whitespace(file);
        let (parameter, file) = parse_function_parameter(file)?;
        let file = parse_optional_whitespace(file);
        let file = file
            .strip_prefix("->")
            .ok_or_else(|| "Expected -> after function parameter".to_string())?;
        let file = parse_optional_whitespace(file);
        let (return_type, file) = parse_operation_expression(file)?;
        let file = parse_optional_whitespace(file);
        let (return_type, body, file) = if file.starts_with("(") {
            let (body, file) = parse_grouping_expression(file)
                .ok_or_else(|| "Expected function body expression".to_string())?;
            (return_type, body, file)
        } else {
            match return_type {
                Expression::FunctionCall { function, argument } => (*function, *argument, file),
                _ => return Err("Expected function body expression".to_string()),
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
    Some(parse_function_literal_inner(remaining))
}

fn parse_function_parameter(file: &str) -> Result<(BindingPattern, &str), String> {
    let file = parse_optional_whitespace(file);
    if let Some(remaining) = file.strip_prefix("(") {
        let remaining = parse_optional_whitespace(remaining);
        let (pattern, remaining) = parse_binding_pattern(remaining)?;
        let remaining = parse_optional_whitespace(remaining);
        let remaining = remaining
            .strip_prefix(")")
            .ok_or_else(|| "Expected ) after function parameter".to_string())?;
        Ok((pattern, remaining))
    } else if file.starts_with("{") {
        parse_struct_binding_pattern(file)
    } else {
        Err("Expected function parameter after fn".to_string())
    }
}

fn parse_struct_expression(file: &str) -> Option<Result<(Expression, &str), String>> {
    fn parse_struct_expression_inner(file: &str) -> Result<(Expression, &str), String> {
        let mut remaining = file
            .strip_prefix("{")
            .ok_or_else(|| "Expected { to start struct literal".to_string())?;
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
                    let (value_expr, rest) = parse_operation_expression(after_ws)?;
                    items.push((identifier, value_expr));
                    remaining = rest;
                    remaining = parse_optional_whitespace(remaining);

                    if let Some(rest) = remaining.strip_prefix("}") {
                        return Ok((Expression::Struct(items), rest));
                    }

                    remaining = remaining
                        .strip_prefix(",")
                        .ok_or_else(|| "Expected , or } in struct literal".to_string())?;
                    continue;
                }
            }

            let (value_expr, rest) = parse_operation_expression(remaining)?;
            items.push((Identifier(tuple_index.to_string()), value_expr));
            tuple_index += 1;
            remaining = rest;
            remaining = parse_optional_whitespace(remaining);

            if let Some(rest) = remaining.strip_prefix("}") {
                return Ok((Expression::Struct(items), rest));
            }

            remaining = remaining
                .strip_prefix(",")
                .ok_or_else(|| "Expected , or } in struct literal".to_string())?;
        }
    }

    if !file.starts_with("{") {
        return None;
    }
    Some(parse_struct_expression_inner(file))
}

pub fn parse_isolated_expression(file: &str) -> Result<(Expression, &str), String> {
    if let Some(function_parse) = parse_function_literal(file) {
        return function_parse;
    }
    if let Some((expr, remaining)) = parse_grouping_expression(file) {
        return Ok((expr, remaining));
    }
    if let Some(struct_parse) = parse_struct_expression(file) {
        return struct_parse;
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
        let file = parse_optional_whitespace(file);
        let file = parse_eq(file).ok_or("Expected = after binding pattern".to_string())?;
        let file = parse_optional_whitespace(file);
        let (expr, file) = parse_isolated_expression(file)?;

        Ok((
            Binding {
                pattern,
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
    parse_block_with_terminators(file, &[])
}

fn parse_block_with_terminators<'a>(
    file: &'a str,
    terminators: &[char],
) -> Result<(Expression, &'a str), String> {
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

        let (expression, rest) = parse_individual_expression(remaining)?;
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

        let rest = parse_semicolon(remaining)?;
        remaining = parse_optional_whitespace(rest);
    }

    if expressions.is_empty() {
        return Err("Cannot parse empty block".to_string());
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
