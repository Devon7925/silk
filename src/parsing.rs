use crate::diagnostics::{Diagnostic, SourceSpan};

#[derive(Clone, Debug)]
pub struct Identifier(pub String);

#[derive(Clone, Debug)]
pub enum LValue {
    Identifier(Identifier, SourceSpan),
    PropertyAccess {
        object: Box<LValue>,
        property: String,
        span: SourceSpan,
    },
}

impl LValue {
    pub fn span(&self) -> SourceSpan {
        match self {
            LValue::Identifier(_, span) => *span,
            LValue::PropertyAccess { span, .. } => *span,
        }
    }
}

#[derive(Clone, Debug)]
pub enum BindingPattern {
    Identifier(Identifier, SourceSpan),
    Literal(ExpressionLiteral, SourceSpan),
    Struct(Vec<(Identifier, BindingPattern)>, SourceSpan),
    EnumVariant {
        enum_type: Box<Expression>,
        variant: Identifier,
        payload: Option<Box<BindingPattern>>,
        span: SourceSpan,
    },
    TypeHint(Box<BindingPattern>, Box<Expression>, SourceSpan),
    Annotated {
        annotations: Vec<BindingAnnotation>,
        pattern: Box<BindingPattern>,
        span: SourceSpan,
    },
}

impl BindingPattern {
    pub fn span(&self) -> SourceSpan {
        match self {
            BindingPattern::Identifier(_, span)
            | BindingPattern::Literal(_, span)
            | BindingPattern::Struct(_, span)
            | BindingPattern::EnumVariant { span, .. }
            | BindingPattern::TypeHint(_, _, span)
            | BindingPattern::Annotated { span, .. } => *span,
        }
    }
}

#[derive(Clone, Debug)]
pub enum TargetLiteral {
    JSTarget,
    WasmTarget,
}

#[derive(Clone, Debug)]
pub enum ExpressionLiteral {
    Number(i32),
    Boolean(bool),
    Target(TargetLiteral),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntrinsicType {
    I32,
    Boolean,
    Type,
    Target,
}

#[derive(Clone, Debug)]
pub enum BinaryIntrinsicOperator {
    I32Add,
    I32Subtract,
    I32Multiply,
    I32Divide,
    I32Equal,
    I32NotEqual,
    I32LessThan,
    I32GreaterThan,
    I32LessThanOrEqual,
    I32GreaterThanOrEqual,
    BooleanAnd,
    BooleanOr,
    BooleanXor,
}

#[derive(Clone, Debug)]
pub enum IntrinsicOperation {
    Binary(Box<Expression>, Box<Expression>, BinaryIntrinsicOperator),
    EnumFromStruct,
}

#[derive(Clone, Debug)]
pub enum Expression {
    IntrinsicType(IntrinsicType, SourceSpan),
    IntrinsicOperation(IntrinsicOperation, SourceSpan),
    EnumType(Vec<(Identifier, Expression)>, SourceSpan),
    EnumValue {
        enum_type: Box<Expression>,
        variant: Identifier,
        variant_index: usize,
        payload: Option<Box<Expression>>,
        span: SourceSpan,
    },
    EnumConstructor {
        enum_type: Box<Expression>,
        variant: Identifier,
        variant_index: usize,
        payload_type: Box<Expression>,
        span: SourceSpan,
    },
    EnumAccess {
        enum_expr: Box<Expression>,
        variant: Identifier,
        span: SourceSpan,
    },
    If {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Option<Box<Expression>>,
        span: SourceSpan,
    },
    AttachImplementation {
        type_expr: Box<Expression>,
        implementation: Box<Expression>,
        span: SourceSpan,
    },
    Function {
        parameter: BindingPattern,
        return_type: Box<Expression>,
        body: Box<Expression>,
        span: SourceSpan,
    },
    FunctionType {
        parameter: Box<Expression>,
        return_type: Box<Expression>,
        span: SourceSpan,
    },
    Struct(Vec<(Identifier, Expression)>, SourceSpan),
    Literal(ExpressionLiteral, SourceSpan),
    Identifier(Identifier, SourceSpan),
    Operation {
        operator: String,
        left: Box<Expression>,
        right: Box<Expression>,
        span: SourceSpan,
    },
    Assignment {
        target: LValue,
        expr: Box<Expression>,
        span: SourceSpan,
    },
    FunctionCall {
        function: Box<Expression>,
        argument: Box<Expression>,
        span: SourceSpan,
    },
    PropertyAccess {
        object: Box<Expression>,
        property: String,
        span: SourceSpan,
    },
    Binding(Box<Binding>, SourceSpan),
    Block(Vec<Expression>, SourceSpan),
}

impl Expression {
    pub fn span(&self) -> SourceSpan {
        match self {
            Expression::IntrinsicType(_, span)
            | Expression::IntrinsicOperation(_, span)
            | Expression::EnumType(_, span)
            | Expression::EnumAccess { span, .. }
            | Expression::Struct(_, span)
            | Expression::If { span, .. }
            | Expression::Literal(_, span)
            | Expression::Identifier(_, span)
            | Expression::Binding(_, span)
            | Expression::Block(_, span)
            | Expression::Assignment { span, .. }
            | Expression::EnumValue { span, .. }
            | Expression::EnumConstructor { span, .. } => *span,
            Expression::AttachImplementation { span, .. }
            | Expression::Function { span, .. }
            | Expression::FunctionType { span, .. }
            | Expression::Operation { span, .. }
            | Expression::FunctionCall { span, .. }
            | Expression::PropertyAccess { span, .. } => *span,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Binding {
    pub pattern: BindingPattern,
    pub expr: Expression,
}

#[derive(Clone, Debug)]
pub enum BindingAnnotation {
    Export(Expression, SourceSpan),
    Mutable(SourceSpan),
}

fn parse_if_expression<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(Expression, &'a str), Diagnostic>> {
    if !file.starts_with("if") {
        return None;
    }

    let after_keyword = &file[2..];
    if after_keyword
        .chars()
        .next()
        .filter(|c| c.is_alphanumeric() || *c == '_')
        .is_some()
    {
        return None;
    }

    Some(parse_if_expression_with_source(source, file))
}

fn parse_if_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(Expression, &'a str), Diagnostic> {
    let start_slice = file;
    let mut remaining = file
        .strip_prefix("if")
        .ok_or_else(|| diagnostic_here(source, file, 2, "Expected if"))?;

    remaining = parse_optional_whitespace(remaining);
    let (mut condition, mut after_condition) =
        parse_operation_expression_with_guard(source, remaining, true)?;
    after_condition = parse_optional_whitespace(after_condition);

    let mut inferred_then_branch: Option<Expression> = None;
    if !after_condition.starts_with('(') {
        if let Expression::FunctionCall {
            function, argument, ..
        } = condition.clone()
        {
            if let Expression::Block(_, _) = *argument {
                inferred_then_branch = Some(*argument);
                condition = *function;
            }
        }
    }

    let (then_branch, mut remaining) = if let Some(branch) = inferred_then_branch {
        (branch, after_condition)
    } else {
        match parse_grouping_expression_with_source(source, after_condition) {
            Some(result) => result?,
            None => {
                return Err(diagnostic_here(
                    source,
                    after_condition,
                    after_condition
                        .chars()
                        .next()
                        .map(|c| c.len_utf8())
                        .unwrap_or(1),
                    "Expected ( to start if body",
                ));
            }
        }
    };

    remaining = parse_optional_whitespace(remaining);

    let else_branch = if let Some(rest) = remaining.strip_prefix("else") {
        let rest = parse_optional_whitespace(rest);
        if let Some(if_parse) = parse_if_expression(source, rest) {
            let (else_expr, remaining) = if_parse?;
            Some((Some(else_expr), remaining))
        } else if let Some(group_parse) = parse_grouping_expression_with_source(source, rest) {
            let (else_expr, remaining) = group_parse?;
            Some((Some(else_expr), remaining))
        } else {
            return Err(diagnostic_here(
                source,
                rest,
                rest.chars().next().map(|c| c.len_utf8()).unwrap_or(1),
                "Expected ( to start else body",
            ));
        }
    } else {
        None
    };

    let (else_branch, remaining) = else_branch.unwrap_or((None, remaining));

    let span = consumed_span(source, start_slice, remaining);
    Ok((
        Expression::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
            span,
        },
        remaining,
    ))
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

fn consumed_span(source: &str, before: &str, after: &str) -> SourceSpan {
    let start = source
        .len()
        .checked_sub(before.len())
        .expect("slice should originate from source");
    let end = source
        .len()
        .checked_sub(after.len())
        .expect("slice should originate from source");
    SourceSpan::new(start, end.saturating_sub(start))
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
    file.strip_prefix(";")
        .ok_or_else(|| diagnostic_here(source, file, 1, "Expected ;"))
}

pub fn parse_whitespace(file: &str) -> Option<&str> {
    file.starts_with(char::is_whitespace)
        .then(|| file.trim_start())
}

pub fn parse_optional_whitespace(file: &str) -> &str {
    let mut remaining = file.trim_start();

    // Skip line comments as if they were whitespace.
    loop {
        if let Some(after_comment) = remaining.strip_prefix("//") {
            if let Some((_, rest)) = after_comment.split_once('\n') {
                remaining = rest.trim_start();
                continue;
            } else {
                return "";
            }
        }

        return remaining;
    }
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
    let mut consumed = 0;
    let is_negative = matches!(file.chars().next(), Some('-'));

    let digits = file
        .chars()
        .skip(is_negative as usize)
        .take_while(|c| c.is_digit(10))
        .collect::<String>();

    if digits.is_empty() {
        return None;
    }

    consumed += digits.len();
    if is_negative {
        consumed += 1;
    }

    let number: i32 = digits.parse().ok()?;
    let remaining = &file[consumed..];
    Some((
        ExpressionLiteral::Number(if is_negative { -number } else { number }),
        remaining,
    ))
}

fn parse_simple_binding_pattern<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(BindingPattern, &'a str), Diagnostic> {
    let search_slice = if let Some(eq_index) = file.find('=') {
        &file[..eq_index]
    } else {
        file
    };

    if let Some(sep_index) = search_slice.find("::") {
        let start_slice = file;
        let (enum_part, after_enum) = file.split_at(sep_index);
        let (enum_type, _enum_remaining) =
            parse_operation_expression_with_source(source, enum_part)?;

        let mut after_enum = &after_enum["::".len()..];
        after_enum = parse_optional_whitespace(after_enum);
        let (variant_identifier, after_variant) =
            parse_identifier(after_enum).ok_or_else(|| {
                diagnostic_here(
                    source,
                    after_enum,
                    after_enum.chars().next().map(|c| c.len_utf8()).unwrap_or(1),
                    "Expected variant identifier after ::",
                )
            })?;

        let after_variant = parse_optional_whitespace(after_variant);
        let (payload, remaining) = if let Some(after_paren) = after_variant.strip_prefix("(") {
            let after_paren = parse_optional_whitespace(after_paren);
            let (payload, rest) = parse_binding_pattern_with_source(source, after_paren)?;
            let rest = parse_optional_whitespace(rest);
            let rest = rest.strip_prefix(")").ok_or_else(|| {
                diagnostic_here(
                    source,
                    rest,
                    rest.chars().next().map(|c| c.len_utf8()).unwrap_or(1),
                    "Expected ) after enum payload pattern",
                )
            })?;
            (Some(Box::new(payload)), rest)
        } else {
            (None, after_variant)
        };
        let span = consumed_span(source, start_slice, remaining);
        return Ok((
            BindingPattern::EnumVariant {
                enum_type: Box::new(enum_type),
                variant: variant_identifier,
                payload,
                span,
            },
            remaining,
        ));
    }
    if let Some((identifier, remaining)) = parse_identifier(file) {
        let span = consumed_span(source, file, remaining);
        return Ok((BindingPattern::Identifier(identifier, span), remaining));
    }
    if let Some((literal, remaining)) = parse_literal(file) {
        let span = consumed_span(source, file, remaining);
        return Ok((BindingPattern::Literal(literal, span), remaining));
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
    let annotation_start_slice = file;
    let (annotations, file) = parse_binding_annotations(source, file)?;
    let (mut pattern, mut remaining) = if file.starts_with("{") {
        parse_struct_binding_pattern_with_source(source, file)?
    } else {
        parse_simple_binding_pattern(source, file)?
    };
    if let Some(type_hint_parse) = parse_type_hint(source, remaining) {
        let (type_expr, new_remaining) = type_hint_parse?;
        let span = pattern.span().merge(&type_expr.span());
        pattern = BindingPattern::TypeHint(Box::new(pattern), Box::new(type_expr), span);
        remaining = new_remaining;
    }

    if !annotations.is_empty() {
        let span = consumed_span(source, annotation_start_slice, remaining);
        pattern = BindingPattern::Annotated {
            annotations,
            pattern: Box::new(pattern),
            span,
        };
    }

    Ok((pattern, remaining))
}

#[cfg(test)]
pub fn parse_struct_binding_pattern(file: &str) -> Result<(BindingPattern, &str), Diagnostic> {
    parse_struct_binding_pattern_with_source(file, file)
}

fn parse_struct_binding_pattern_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(BindingPattern, &'a str), Diagnostic> {
    let start_slice = file;
    let mut remaining = file.strip_prefix("{").ok_or_else(|| {
        diagnostic_here(
            source,
            file,
            1,
            "Expected { to start struct binding pattern",
        )
    })?;
    let mut fields = Vec::new();
    let mut tuple_index = 0usize;

    loop {
        remaining = parse_optional_whitespace(remaining);

        if let Some(rest) = remaining.strip_prefix("}") {
            let span = consumed_span(source, start_slice, rest);
            return Ok((BindingPattern::Struct(fields, span), rest));
        }

        if let Some((field_identifier, after_identifier)) = parse_identifier(remaining) {
            let after_ws = parse_optional_whitespace(after_identifier);
            if let Some(rest_after_equals) = after_ws.strip_prefix("=") {
                let rest_after_equals = parse_optional_whitespace(rest_after_equals);
                let (field_pattern, rest) =
                    parse_binding_pattern_with_source(source, rest_after_equals)?;
                fields.push((field_identifier, field_pattern));
                remaining = parse_optional_whitespace(rest);
            } else {
                let (field_pattern, rest) = parse_binding_pattern_with_source(source, remaining)?;
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
            let span = consumed_span(source, start_slice, rest);
            return Ok((BindingPattern::Struct(fields, span), rest));
        }

        remaining = remaining.strip_prefix(",").ok_or_else(|| {
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
    let operator_chars: Vec<char> = vec!['+', '-', '*', '/', '=', '!', '<', '>', '&', '|', '^'];
    let operator = file
        .chars()
        .take_while(|c| operator_chars.contains(c))
        .collect::<String>();

    if operator.is_empty() {
        return None;
    }

    if operator == "=" {
        return None;
    }

    let remaining = &file[operator.len()..];
    Some((operator, remaining))
}

fn operator_precedence(operator: &str) -> u8 {
    match operator {
        "*" | "/" => 4,
        "+" | "-" => 3,
        "==" | "!=" | "<" | ">" | "<=" | ">=" => 2,
        "&&" => 1,
        "||" | "^" => 0,
        _ => 0,
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
        let start_slice = file;
        let file = file
            .strip_prefix("fn")
            .ok_or_else(|| diagnostic_here(source, file, 2, "Expected fn"))?;
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
                    ));
                }
            }
        } else {
            match return_type {
                Expression::FunctionCall {
                    function,
                    argument,
                    span: _,
                } => (*function, *argument, file),
                _ => {
                    return Err(diagnostic_here(
                        source,
                        file,
                        1,
                        "Expected function body expression",
                    ));
                }
            }
        };
        let span = consumed_span(source, start_slice, file);
        Ok((
            Expression::Function {
                parameter,
                return_type: Box::new(return_type),
                body: Box::new(body),
                span,
            },
            file,
        ))
    }

    if !file.starts_with("fn") {
        return None;
    }
    let after_fn = &file[2..];
    if after_fn
        .chars()
        .next()
        .filter(|c| c.is_alphanumeric() || *c == '_')
        .is_some()
    {
        return None;
    }
    Some(parse_function_literal_inner(source, file))
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
        let start_slice = file;
        let mut remaining = file.strip_prefix("{").ok_or_else(|| {
            diagnostic_here(source, file, 1, "Expected { to start struct literal")
        })?;
        let mut items = Vec::new();
        let mut tuple_index = 0usize;

        loop {
            remaining = parse_optional_whitespace(remaining);

            if let Some(rest) = remaining.strip_prefix("}") {
                let span = consumed_span(source, start_slice, rest);
                return Ok((Expression::Struct(items, span), rest));
            }

            if let Some((identifier, after_identifier)) = parse_identifier(remaining) {
                let after_ws = parse_optional_whitespace(after_identifier);
                if let Some(after_equals) = after_ws.strip_prefix("=") {
                    let after_ws = parse_optional_whitespace(after_equals);
                    let (value_expr, rest) =
                        parse_operation_expression_with_source(source, after_ws)?;
                    items.push((identifier, value_expr));
                    remaining = rest;
                    remaining = parse_optional_whitespace(remaining);

                    if let Some(rest) = remaining.strip_prefix("}") {
                        let span = consumed_span(source, start_slice, rest);
                        return Ok((Expression::Struct(items, span), rest));
                    }

                    remaining = remaining.strip_prefix(",").ok_or_else(|| {
                        diagnostic_here(source, remaining, 1, "Expected , or } in struct literal")
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
                let span = consumed_span(source, start_slice, rest);
                return Ok((Expression::Struct(items, span), rest));
            }

            remaining = remaining.strip_prefix(",").ok_or_else(|| {
                diagnostic_here(source, remaining, 1, "Expected , or } in struct literal")
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
    parse_isolated_expression_with_source_with_guard(source, file, false)
}

fn parse_isolated_expression_with_source_with_guard<'a>(
    source: &'a str,
    file: &'a str,
    stop_before_grouping: bool,
) -> Result<(Expression, &'a str), Diagnostic> {
    if let Some(binding_parse) = parse_binding(source, file, stop_before_grouping) {
        return binding_parse;
    }
    if let Some(function_parse) = parse_function_literal(source, file) {
        return function_parse;
    }
    if let Some(if_parse) = parse_if_expression(source, file) {
        return if_parse;
    }
    if let Some(group_parse) = parse_grouping_expression_with_source(source, file) {
        return group_parse;
    }
    if let Some(struct_parse) = parse_struct_expression(source, file) {
        return struct_parse;
    }
    if let Some((identifier, remaining)) = parse_identifier(file) {
        let span = consumed_span(source, file, remaining);
        return Ok((Expression::Identifier(identifier, span), remaining));
    }
    if let Some((literal, remaining)) = parse_literal(file) {
        let span = consumed_span(source, file, remaining);
        return Ok((Expression::Literal(literal, span), remaining));
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
    stop_before_grouping: bool,
) -> Result<(Expression, &'a str), Diagnostic> {
    let expression_start = file;
    let (mut expr, mut remaining) =
        parse_isolated_expression_with_source_with_guard(source, file, stop_before_grouping)?;

    loop {
        let lookahead = parse_optional_whitespace(remaining);
        let (after_separator, is_enum_access) = if let Some(rest) = lookahead.strip_prefix("::") {
            (rest, true)
        } else if let Some(rest) = lookahead.strip_prefix(".") {
            (rest, false)
        } else {
            break;
        };

        let after_separator = parse_optional_whitespace(after_separator);
        let (property_identifier, rest) = parse_identifier(after_separator).ok_or_else(|| {
            diagnostic_here(
                source,
                after_separator,
                after_separator
                    .chars()
                    .next()
                    .map(|c| c.len_utf8())
                    .unwrap_or(1),
                if is_enum_access {
                    "Expected identifier after :: in enum access"
                } else {
                    "Expected identifier after . in property access"
                },
            )
        })?;
        let span = consumed_span(source, expression_start, rest);
        expr = if is_enum_access {
            Expression::EnumAccess {
                enum_expr: Box::new(expr),
                variant: property_identifier,
                span,
            }
        } else {
            Expression::PropertyAccess {
                object: Box::new(expr),
                property: property_identifier.0,
                span,
            }
        };
        remaining = rest;
    }

    Ok((expr, remaining))
}

fn parse_function_call<'a>(
    source: &'a str,
    file: &'a str,
    stop_before_grouping: bool,
) -> Result<(Expression, &'a str), Diagnostic> {
    let mut exprs = vec![];
    let (function_expr, mut remaining) = parse_property_access(source, file, stop_before_grouping)?;
    remaining = parse_optional_whitespace(remaining);
    exprs.push(function_expr);
    loop {
        let lookahead = parse_optional_whitespace(remaining);
        if stop_before_grouping && lookahead.starts_with('(') {
            break;
        }

        let Ok((argument_expr, rest)) =
            parse_property_access(source, remaining, stop_before_grouping)
        else {
            break;
        };

        if stop_before_grouping {
            if let Expression::Block(_, _) = argument_expr {
                break;
            }
        }

        remaining = parse_optional_whitespace(rest);
        exprs.push(argument_expr);
    }
    Ok((
        exprs
            .into_iter()
            .reduce(|function, argument| {
                let span = function.span().merge(&argument.span());
                Expression::FunctionCall {
                    function: Box::new(function),
                    argument: Box::new(argument),
                    span,
                }
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
    parse_operation_expression_with_guard(source, file, false)
}

fn parse_operation_expression_with_guard<'a>(
    source: &'a str,
    file: &'a str,
    stop_before_grouping: bool,
) -> Result<(Expression, &'a str), Diagnostic> {
    parse_operation_expression_with_min_precedence(source, file, stop_before_grouping, 0)
}

fn parse_assignment_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(Expression, &'a str), Diagnostic> {
    parse_assignment_expression_with_guard(source, file, false)
}

fn parse_assignment_expression_with_guard<'a>(
    source: &'a str,
    file: &'a str,
    stop_before_grouping: bool,
) -> Result<(Expression, &'a str), Diagnostic> {
    let trimmed = parse_optional_whitespace(file);
    let start_slice = trimmed;
    if let Some(Ok((target, after_target))) = parse_lvalue(source, trimmed) {
        let after_target = parse_optional_whitespace(after_target);
        if let Some(after_equals) = after_target.strip_prefix('=') {
            if !after_equals.starts_with('=') {
                let after_equals = parse_optional_whitespace(after_equals);
                let (expr, remaining) = parse_operation_expression_with_guard(
                    source,
                    after_equals,
                    stop_before_grouping,
                )?;
                let span = consumed_span(source, start_slice, remaining);
                return Ok((
                    Expression::Assignment {
                        target,
                        expr: Box::new(expr),
                        span,
                    },
                    remaining,
                ));
            }
        }
    }

    parse_operation_expression_with_guard(source, file, stop_before_grouping)
}

fn parse_lvalue<'a>(source: &'a str, file: &'a str) -> Option<Result<(LValue, &'a str), Diagnostic>> {
    let trimmed = parse_optional_whitespace(file);
    let start_slice = trimmed;
    let (identifier, mut remaining) = parse_identifier(trimmed)?;
    let mut current_span = consumed_span(source, start_slice, remaining);
    let mut lvalue = LValue::Identifier(identifier, current_span);

    loop {
        let lookahead = parse_optional_whitespace(remaining);
        let Some(after_dot) = lookahead.strip_prefix('.') else {
            break;
        };

        let after_dot = parse_optional_whitespace(after_dot);
        let Some((property_identifier, rest)) = parse_identifier(after_dot) else {
            return Some(Err(diagnostic_here(
                source,
                after_dot,
                after_dot.chars().next().map(|c| c.len_utf8()).unwrap_or(1),
                "Expected identifier after . in assignment target",
            )));
        };

        current_span = consumed_span(source, start_slice, rest);
        lvalue = LValue::PropertyAccess {
            object: Box::new(lvalue),
            property: property_identifier.0,
            span: current_span,
        };
        remaining = rest;
    }

    Some(Ok((lvalue, remaining)))
}

fn parse_operation_expression_with_min_precedence<'a>(
    source: &'a str,
    file: &'a str,
    stop_before_grouping: bool,
    min_precedence: u8,
) -> Result<(Expression, &'a str), Diagnostic> {
    fn parse_operations<'a>(
        source: &'a str,
        file: &'a str,
        stop_before_grouping: bool,
        min_precedence: u8,
    ) -> Result<(Vec<Expression>, Vec<String>, &'a str), Diagnostic> {
        let mut expressions: Vec<Expression> = Vec::new();
        let mut operators: Vec<String> = Vec::new();
        let (expression, mut remaining) = parse_function_call(source, file, stop_before_grouping)?;
        remaining = parse_optional_whitespace(remaining);
        expressions.push(expression);
        while let Some((operator, rest)) = parse_operator(remaining) {
            // Stop if operator precedence is below minimum
            if operator_precedence(&operator) < min_precedence {
                break;
            }
            let rest = parse_optional_whitespace(rest);
            let (next_expression, rest) = parse_function_call(source, rest, stop_before_grouping)?;
            let rest = parse_optional_whitespace(rest);
            operators.push(operator);
            expressions.push(next_expression);
            remaining = rest;
        }
        Ok((expressions, operators, remaining))
    }

    let (expressions, operators, remaining) =
        parse_operations(source, file, stop_before_grouping, min_precedence)?;

    fn reduce_stacks(
        operand_stack: &mut Vec<Expression>,
        operator_stack: &mut Vec<String>,
        source: &str,
    ) -> Result<(), Diagnostic> {
        let operator = operator_stack.pop().ok_or_else(|| {
            diagnostic_at_eof(source, "Expected operator when reducing operation")
        })?;
        let right = operand_stack.pop().ok_or_else(|| {
            diagnostic_at_eof(source, "Expected right operand when reducing operation")
        })?;
        let left = operand_stack.pop().ok_or_else(|| {
            diagnostic_at_eof(source, "Expected left operand when reducing operation")
        })?;
        let span = left.span().merge(&right.span());
        operand_stack.push(Expression::Operation {
            operator,
            left: Box::new(left),
            right: Box::new(right),
            span,
        });
        Ok(())
    }

    let mut operand_stack: Vec<Expression> = Vec::new();
    let mut operator_stack: Vec<String> = Vec::new();

    let mut expression_iter = expressions.into_iter();
    let first_expression = expression_iter.next().ok_or_else(|| {
        diagnostic_at_eof(source, "Expected expression to start parsing operation")
    })?;
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

fn parse_binding_annotations<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(Vec<BindingAnnotation>, &'a str), Diagnostic> {
    let mut annotations = Vec::new();
    let mut remaining = file;

    loop {
        let trimmed = parse_optional_whitespace(remaining);
        match parse_binding_annotation(source, trimmed) {
            Some(result) => {
                let (annotation, rest) = result?;
                annotations.push(annotation);
                remaining = rest;
            }
            None => return Ok((annotations, trimmed)),
        }
    }
}

fn parse_binding_annotation<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(BindingAnnotation, &'a str), Diagnostic>> {
    parse_export_binding_annotation(source, file)
        .or_else(|| parse_mut_binding_annotation(source, file))
}

fn parse_export_binding_annotation<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(BindingAnnotation, &'a str), Diagnostic>> {
    const KEYWORD: &str = "export";
    if !file.starts_with(KEYWORD) {
        return None;
    }

    let after_keyword = &file[KEYWORD.len()..];
    let after_keyword = parse_optional_whitespace(after_keyword);
    if !after_keyword.starts_with('(') {
        return None;
    }

    let annotation_start_slice = file;
    let after_paren = &after_keyword[1..];
    let after_paren = parse_optional_whitespace(after_paren);
    Some(
        parse_isolated_expression_with_source(source, after_paren).and_then(
            |(target_expr, rest)| {
                let rest = parse_optional_whitespace(rest);
                let rest = rest.strip_prefix(")").ok_or_else(|| {
                    diagnostic_here(
                        source,
                        rest,
                        rest.chars().next().map(|c| c.len_utf8()).unwrap_or(1),
                        "Expected ) to close export annotation",
                    )
                })?;
                let span = consumed_span(source, annotation_start_slice, rest);
                Ok((BindingAnnotation::Export(target_expr, span), rest))
            },
        ),
    )
}

fn parse_mut_binding_annotation<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(BindingAnnotation, &'a str), Diagnostic>> {
    const KEYWORD: &str = "mut";
    if !file.starts_with(KEYWORD) {
        return None;
    }

    let after_keyword = &file[KEYWORD.len()..];
    if after_keyword
        .chars()
        .next()
        .map(|c| c.is_alphanumeric() || c == '_')
        .unwrap_or(false)
    {
        return None;
    }

    let annotation_start_slice = file;
    let rest = parse_optional_whitespace(after_keyword);
    let span = consumed_span(source, annotation_start_slice, rest);
    Some(Ok((BindingAnnotation::Mutable(span), rest)))
}

fn parse_binding<'a>(
    source: &'a str,
    file: &'a str,
    stop_before_grouping: bool,
) -> Option<Result<(Expression, &'a str), Diagnostic>> {
    let start_slice = file;
    let file = parse_let(file)?;

    fn parse_binding_inner<'a>(
        source: &'a str,
        file: &'a str,
        stop_before_grouping: bool,
    ) -> Result<(Binding, &'a str), Diagnostic> {
        let file = parse_whitespace(file)
            .ok_or_else(|| diagnostic_here(source, file, 1, "Expected whitespace after let"))?;
        let (pattern, file) = parse_binding_pattern_with_source(source, file)?;
        let file = parse_optional_whitespace(file);
        let file = parse_eq(file)
            .ok_or_else(|| diagnostic_here(source, file, 1, "Expected = after binding pattern"))?;
        let file = parse_optional_whitespace(file);
        // Use min_precedence=2 to stop before && (precedence 1) and || (precedence 0)
        // This enables let chains: "let x = a && b" parses as "(let x = a) && b"
        let (expr, file) =
            parse_operation_expression_with_min_precedence(source, file, stop_before_grouping, 2)?;

        Ok((Binding { pattern, expr }, file))
    }

    Some(
        parse_binding_inner(source, file, stop_before_grouping).map(|(binding, remaining)| {
            let span = consumed_span(source, start_slice, remaining);
            (Expression::Binding(Box::new(binding), span), remaining)
        }),
    )
}

fn parse_individual_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(Expression, &'a str), Diagnostic> {
    if let Some(binding_parse) = parse_binding(source, file, false) {
        return binding_parse;
    }
    parse_assignment_expression_with_source(source, file)
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
    let mut ended_with_semicolon = false;

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
        ended_with_semicolon = false;

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
        ended_with_semicolon = true;
    }

    if expressions.is_empty() {
        return Err(diagnostic_here(
            source,
            remaining,
            remaining.chars().next().map(|c| c.len_utf8()).unwrap_or(0),
            "Cannot parse empty block",
        ));
    }

    if ended_with_semicolon {
        if let Some(last_span) = expressions.last().map(|expr| expr.span()) {
            expressions.push(Expression::Struct(
                vec![],
                SourceSpan::new(last_span.end(), 0),
            ));
        }
    }

    if expressions.len() == 1 {
        Ok((expressions.into_iter().next().unwrap(), remaining))
    } else {
        let span = expressions
            .iter()
            .skip(1)
            .fold(expressions[0].span(), |acc, expr| acc.merge(&expr.span()));
        Ok((Expression::Block(expressions, span), remaining))
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

    let (Expression::Block(parsed, _), "") = parsed else {
        panic!()
    };
    assert_eq!(parsed.len(), 2);

    let Expression::Binding(binding1, _) = &parsed[0] else {
        panic!()
    };
    assert_eq!(
        matches!(binding1.pattern, BindingPattern::Identifier(ref id, _) if id.0 == "x"),
        true
    );
    assert_eq!(
        matches!(binding1.expr, Expression::Literal(ExpressionLiteral::Number(lit), _) if lit == 42),
        true
    );

    let Expression::Binding(binding2, _) = &parsed[1] else {
        panic!()
    };
    assert_eq!(
        matches!(binding2.expr, Expression::Identifier(ref lit, _) if lit.0 == "x"),
        true
    );
    let BindingPattern::TypeHint(binding2, binding2_type, _) = &binding2.pattern else {
        panic!()
    };
    assert!(matches!(**binding2, BindingPattern::Identifier(ref hint, _) if hint.0 == "y"));
    assert_eq!(
        matches!(**binding2_type, Expression::Identifier(ref hint, _) if hint.0 == "i32"),
        true
    );
}

#[test]
fn parse_binding_with_export_annotation() {
    let parsed = parse_block(
        "
let export(js) foo = 42;
let export(wasm) export(js) bar = foo;
bar
    ",
    )
    .unwrap();

    let (Expression::Block(parsed, _), "") = parsed else {
        panic!()
    };
    assert_eq!(parsed.len(), 3);

    let Expression::Binding(binding1, _) = &parsed[0] else {
        panic!()
    };
    let BindingPattern::Annotated {
        annotations,
        pattern: _,
        ..
    } = &binding1.pattern
    else {
        panic!("expected annotated pattern");
    };
    assert_eq!(annotations.len(), 1);
    match &annotations[0] {
        BindingAnnotation::Export(target_expr, _) => match target_expr {
            Expression::Identifier(identifier, _) => assert_eq!(identifier.0, "js"),
            other => panic!("expected target identifier, got {:?}", other),
        },
        other => panic!("unexpected annotation: {:?}", other),
    }

    let Expression::Binding(binding2, _) = &parsed[1] else {
        panic!()
    };
    let BindingPattern::Annotated { annotations, .. } = &binding2.pattern else {
        panic!("expected annotated pattern")
    };
    assert_eq!(annotations.len(), 2);
    assert!(matches!(annotations[0], BindingAnnotation::Export(_, _)));
    assert!(matches!(annotations[1], BindingAnnotation::Export(_, _)));
}

#[test]
fn parse_struct_pattern_with_inner_export_annotation() {
    let parsed = parse_block(
        "
let { foo = export(js) foo_binding, bar } = value;
foo_binding
    ",
    )
    .unwrap();

    let (Expression::Block(parsed, _), "") = parsed else {
        panic!()
    };
    let Expression::Binding(binding, _) = &parsed[0] else {
        panic!()
    };
    let BindingPattern::Struct(fields, _) = &binding.pattern else {
        panic!("expected struct pattern")
    };
    let (_, foo_pattern) = fields
        .iter()
        .find(|(identifier, _)| identifier.0 == "foo")
        .expect("missing foo field");
    let BindingPattern::Annotated { annotations, .. } = foo_pattern else {
        panic!("expected annotated foo field")
    };
    assert_eq!(annotations.len(), 1);
}

#[test]
fn parse_mutable_binding_and_assignment() {
    let parsed = parse_block(
        "
let mut foo = 1;
foo = 2
    ",
    )
    .unwrap();

    let (Expression::Block(parsed, _), "") = parsed else {
        panic!()
    };

    let Expression::Binding(binding, _) = &parsed[0] else {
        panic!("expected binding expression")
    };
    let BindingPattern::Annotated { annotations, .. } = &binding.pattern else {
        panic!("expected annotated pattern for mutable binding")
    };
    assert!(
        annotations
            .iter()
            .any(|ann| matches!(ann, BindingAnnotation::Mutable(_)))
    );

    let Expression::Assignment { target, .. } = &parsed[1] else {
        panic!("expected assignment expression")
    };
    assert!(matches!(target, LValue::Identifier(Identifier(name), _) if name == "foo"));
}

#[test]
fn parse_operation_expression_precedence() {
    let (expr, remaining) = parse_operation_expression("1 + 2 * 3 / 4 - 5").unwrap();
    assert_eq!(remaining, "");
    let Expression::Operation {
        operator,
        left,
        right,
        span: _,
    } = expr
    else {
        panic!()
    };
    assert_eq!(operator, "-");
    assert!(matches!(
        *right,
        Expression::Literal(ExpressionLiteral::Number(5), _)
    ));

    let Expression::Operation {
        operator,
        left,
        right,
        span: _,
    } = *left
    else {
        panic!();
    };
    assert_eq!(operator, "+");
    assert!(matches!(
        *left,
        Expression::Literal(ExpressionLiteral::Number(1), _)
    ));

    let Expression::Operation {
        operator,
        left,
        right,
        span: _,
    } = *right
    else {
        panic!();
    };
    assert_eq!(operator, "/");
    assert!(matches!(
        *right,
        Expression::Literal(ExpressionLiteral::Number(4), _)
    ));

    let Expression::Operation {
        operator,
        left,
        right,
        span: _,
    } = *left
    else {
        panic!();
    };
    assert_eq!(operator, "*");
    assert!(matches!(
        *left,
        Expression::Literal(ExpressionLiteral::Number(2), _)
    ));
    assert!(matches!(
        *right,
        Expression::Literal(ExpressionLiteral::Number(3), _)
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

    let Expression::Block(items, _) = expr else {
        panic!("expected block with binding and call");
    };
    assert_eq!(items.len(), 2);

    let Expression::Binding(binding, _) = &items[0] else {
        panic!("first expression should be binding");
    };
    assert!(matches!(
        &binding.pattern,
        BindingPattern::Identifier(Identifier(name), _) if name == "foo"
    ));

    let Expression::Function {
        parameter,
        return_type,
        body,
        span: _,
    } = &binding.expr
    else {
        panic!("binding should store function expression");
    };

    let BindingPattern::TypeHint(inner, type_hint, _) = parameter else {
        panic!("parameter should include type hint");
    };
    assert!(matches!(
        inner.as_ref(),
        BindingPattern::Identifier(Identifier(name), _) if name == "bar"
    ));
    assert!(matches!(
        type_hint.as_ref(),
        Expression::Identifier(Identifier(name), _) if name == "i32"
    ));
    assert!(matches!(
        return_type.as_ref(),
        Expression::Identifier(Identifier(name), _) if name == "i32"
    ));
    assert!(matches!(
        **body,
        Expression::Operation {
            operator: ref op,
            ..
        } if op == "+"
    ));

    let Expression::FunctionCall {
        function,
        argument,
        span: _,
    } = &items[1]
    else {
        panic!("expected function call as second expression");
    };
    assert!(matches!(
        function.as_ref(),
        Expression::Identifier(Identifier(name), _) if name == "foo"
    ));
    assert!(matches!(
        **argument,
        Expression::Literal(ExpressionLiteral::Number(123), _)
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
    let BindingPattern::Struct(fields, _) = parameter else {
        panic!("expected struct binding pattern for function parameter");
    };
    assert_eq!(fields.len(), 2);

    let (first_name, first_pattern) = &fields[0];
    assert_eq!(first_name.0, "0");
    let BindingPattern::TypeHint(first_inner, first_type, _) = first_pattern else {
        panic!("expected type hint for first parameter");
    };
    assert!(matches!(
        first_inner.as_ref(),
        BindingPattern::Identifier(Identifier(name), _) if name == "bar1"
    ));
    assert!(matches!(
        first_type.as_ref(),
        Expression::Identifier(Identifier(name), _) if name == "i32"
    ));

    let (second_name, second_pattern) = &fields[1];
    assert_eq!(second_name.0, "1");
    let BindingPattern::TypeHint(second_inner, second_type, _) = second_pattern else {
        panic!("expected type hint for second parameter");
    };
    assert!(matches!(
        second_inner.as_ref(),
        BindingPattern::Identifier(Identifier(name), _) if name == "bar2"
    ));
    assert!(matches!(
        second_type.as_ref(),
        Expression::Identifier(Identifier(name), _) if name == "i32"
    ));
}

#[test]
fn parse_struct_literal_named_and_tuple_fields() {
    let (expr, remaining) = parse_isolated_expression("{foo = 10, 20, bar = 30}").unwrap();
    assert!(remaining.trim().is_empty());
    let Expression::Struct(items, _) = expr else {
        panic!("expected struct literal");
    };
    assert_eq!(items.len(), 3);
    assert_eq!(items[0].0.0, "foo");
    assert_eq!(items[1].0.0, "0");
    assert_eq!(items[2].0.0, "bar");
}

#[test]
fn parse_struct_binding_pattern_named_fields() {
    let (pattern, remaining) =
        parse_struct_binding_pattern("{foo = first: i32, second: i32}").expect("pattern parse");
    assert!(remaining.trim().is_empty());
    let BindingPattern::Struct(fields, _) = pattern else {
        panic!("expected struct pattern");
    };
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].0.0, "foo");
    assert_eq!(fields[1].0.0, "0");
}

#[test]
fn parse_struct_property_access_chain() {
    let (expr, remaining) = parse_operation_expression("foo.bar.baz").expect("parse");
    assert!(remaining.trim().is_empty());

    let Expression::PropertyAccess {
        object,
        property,
        span: _,
    } = expr
    else {
        panic!("expected outer property access");
    };
    assert_eq!(property, "baz");

    let Expression::PropertyAccess {
        object: inner_object,
        property: inner_property,
        span: _,
    } = *object
    else {
        panic!("expected inner property access");
    };
    assert_eq!(inner_property, "bar");
    assert!(matches!(
        *inner_object,
        Expression::Identifier(Identifier(name), _) if name == "foo"
    ));
}

#[test]
fn parse_struct_property_access_then_call() {
    let (expr, remaining) = parse_operation_expression("foo.bar baz").expect("parse");
    assert!(remaining.trim().is_empty());

    let Expression::FunctionCall {
        function,
        argument,
        span: _,
    } = expr
    else {
        panic!("expected function call");
    };
    assert!(matches!(
        *argument,
        Expression::Identifier(Identifier(name), _) if name == "baz"
    ));
    let Expression::PropertyAccess {
        object,
        property,
        span: _,
    } = *function
    else {
        panic!("expected property access as function part");
    };
    assert_eq!(property, "bar");
    assert!(matches!(
        *object,
        Expression::Identifier(Identifier(name), _) if name == "foo"
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
