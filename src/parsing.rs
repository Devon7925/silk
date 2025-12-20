use std::collections::HashSet;

use crate::diagnostics::{Diagnostic, SourceSpan};

#[derive(Clone, Debug)]
pub struct Identifier {
    pub name: String,
    pub unique: String,
}

impl Identifier {
    pub fn new(name: impl Into<String>) -> Self {
        let name = name.into();
        Identifier {
            unique: name.clone(),
            name,
        }
    }

    pub fn with_unique(name: impl Into<String>, unique: impl Into<String>) -> Self {
        Identifier {
            name: name.into(),
            unique: unique.into(),
        }
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.unique == other.unique
    }
}

impl Eq for Identifier {}

impl std::hash::Hash for Identifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.unique.hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

    pub fn pretty_print(&self) -> String {
        match self {
            LValue::Identifier(id, _) => id.name.clone(),
            LValue::PropertyAccess {
                object, property, ..
            } => format!("{}.{}", object.pretty_print(), property),
        }
    }

    pub fn get_used_identifiers(&self) -> HashSet<Identifier> {
        match self {
            LValue::Identifier(identifier, ..) => HashSet::from([identifier.clone()]),
            LValue::PropertyAccess { object, .. } => object.get_used_identifiers(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

    pub fn pretty_print(&self) -> String {
        match self {
            BindingPattern::Identifier(id, _) => id.name.clone(),
            BindingPattern::Literal(lit, _) => match lit {
                ExpressionLiteral::Number(n) => n.to_string(),
                ExpressionLiteral::Boolean(b) => b.to_string(),
                ExpressionLiteral::Target(t) => match t {
                    TargetLiteral::JSTarget => "js".to_string(),
                    TargetLiteral::WasmTarget => "wasm".to_string(),
                },
            },
            BindingPattern::Struct(fields, _) => {
                let field_strs: Vec<String> = fields
                    .iter()
                    .map(|(name, pat)| format!("{} = {}", name.name, pat.pretty_print()))
                    .collect();
                format!("{{ {} }}", field_strs.join(", "))
            }
            BindingPattern::EnumVariant {
                enum_type,
                variant,
                payload,
                ..
            } => {
                let payload_str = payload
                    .as_ref()
                    .map(|p| format!("({})", p.pretty_print()))
                    .unwrap_or_default();
                format!(
                    "{}::{}{}",
                    enum_type.pretty_print(),
                    variant.name,
                    payload_str
                )
            }
            BindingPattern::TypeHint(inner, type_expr, _) => {
                format!("{}: {}", inner.pretty_print(), type_expr.pretty_print())
            }
            BindingPattern::Annotated {
                annotations,
                pattern,
                ..
            } => {
                let mut result = String::new();
                for annotation in annotations {
                    result.push('(');
                    match annotation {
                        BindingAnnotation::Export(expr, _) => {
                            result.push_str(&format!("export {}", expr.pretty_print()));
                        }
                        BindingAnnotation::Mutable(_) => {
                            result.push_str("mut");
                        }
                    }
                    result.push_str(") ");
                }
                result.push_str(&pattern.pretty_print());
                result
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TargetLiteral {
    JSTarget,
    WasmTarget,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryIntrinsicOperator {
    BooleanNot,
    EnumFromStruct,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntrinsicOperation {
    Binary(Box<Expression>, Box<Expression>, BinaryIntrinsicOperator),
    Unary(Box<Expression>, UnaryIntrinsicOperator),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DivergeExpressionType {
    Return,
    Break,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpressionKind {
    IntrinsicType(IntrinsicType),
    IntrinsicOperation(IntrinsicOperation),
    EnumType(Vec<(Identifier, Expression)>),
    Match {
        value: Box<Expression>,
        branches: Vec<(BindingPattern, Expression)>,
    },
    EnumValue {
        enum_type: Box<Expression>,
        variant: Identifier,
        variant_index: usize,
        payload: Box<Expression>,
    },
    EnumConstructor {
        enum_type: Box<Expression>,
        variant: Identifier,
        variant_index: usize,
        payload_type: Box<Expression>,
    },
    EnumAccess {
        enum_expr: Box<Expression>,
        variant: Identifier,
    },
    If {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Box<Expression>,
    },
    AttachImplementation {
        type_expr: Box<Expression>,
        implementation: Box<Expression>,
    },
    Function {
        parameter: BindingPattern,
        return_type: Option<Box<Expression>>,
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
    Assignment {
        target: LValue,
        expr: Box<Expression>,
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
    Diverge {
        value: Box<Expression>,
        divergance_type: DivergeExpressionType,
    },
    Loop {
        body: Box<Expression>,
    },
}

impl ExpressionKind {
    pub fn with_span(self, span: SourceSpan) -> Expression {
        Expression { kind: self, span }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: SourceSpan,
}

impl Expression {
    pub fn new(kind: ExpressionKind, span: SourceSpan) -> Self {
        Self { kind, span }
    }

    pub fn span(&self) -> SourceSpan {
        self.span
    }

    // approximately decompiles the expression to a pretty-printed string
    pub fn pretty_print(&self) -> String {
        match &self.kind {
            ExpressionKind::IntrinsicType(ty) => match ty {
                IntrinsicType::I32 => "i32".to_string(),
                IntrinsicType::Boolean => "boolean".to_string(),
                IntrinsicType::Type => "type".to_string(),
                IntrinsicType::Target => "target".to_string(),
            },
            ExpressionKind::IntrinsicOperation(op) => match op {
                IntrinsicOperation::Binary(left, right, op) => {
                    let op_str = match op {
                        BinaryIntrinsicOperator::I32Add => "+",
                        BinaryIntrinsicOperator::I32Subtract => "-",
                        BinaryIntrinsicOperator::I32Multiply => "*",
                        BinaryIntrinsicOperator::I32Divide => "/",
                        BinaryIntrinsicOperator::I32Equal => "==",
                        BinaryIntrinsicOperator::I32NotEqual => "!=",
                        BinaryIntrinsicOperator::I32LessThan => "<",
                        BinaryIntrinsicOperator::I32GreaterThan => ">",
                        BinaryIntrinsicOperator::I32LessThanOrEqual => "<=",
                        BinaryIntrinsicOperator::I32GreaterThanOrEqual => ">=",
                        BinaryIntrinsicOperator::BooleanAnd => "&&",
                        BinaryIntrinsicOperator::BooleanOr => "||",
                        BinaryIntrinsicOperator::BooleanXor => "^",
                    };
                    format!(
                        "{} {} {}",
                        left.pretty_print(),
                        op_str,
                        right.pretty_print()
                    )
                }
                IntrinsicOperation::Unary(operand, op) => {
                    let op_str = match op {
                        UnaryIntrinsicOperator::BooleanNot => "!",
                        UnaryIntrinsicOperator::EnumFromStruct => "enum",
                    };
                    format!("{}({})", op_str, operand.pretty_print())
                }
            },
            ExpressionKind::EnumType(variants) => {
                let variant_strs: Vec<String> = variants
                    .iter()
                    .map(|(name, ty)| format!("{}({})", name.name, ty.pretty_print()))
                    .collect();
                format!("enum {{ {} }}", variant_strs.join(", "))
            }
            ExpressionKind::Match { value, branches } => {
                let branch_strs: Vec<String> = branches
                    .iter()
                    .map(|(pattern, body)| {
                        format!("{} => {}", pattern.pretty_print(), body.pretty_print())
                    })
                    .collect();
                format!(
                    "match {} with ({})",
                    value.pretty_print(),
                    branch_strs.join(", ")
                )
            }
            ExpressionKind::EnumValue {
                enum_type,
                variant,
                payload,
                ..
            } => {
                let payload_str = format!("({})", payload.pretty_print());
                format!(
                    "{}::{}{}",
                    enum_type.pretty_print(),
                    variant.name,
                    payload_str
                )
            }
            ExpressionKind::EnumConstructor {
                enum_type,
                variant,
                payload_type,
                ..
            } => {
                format!(
                    "{}::{}({})",
                    enum_type.pretty_print(),
                    variant.name,
                    payload_type.pretty_print()
                )
            }
            ExpressionKind::EnumAccess { enum_expr, variant } => {
                format!("{}::{}", enum_expr.pretty_print(), variant.name)
            }
            ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let else_str = format!(" else {}", else_branch.pretty_print());
                format!(
                    "if {} then {}{}",
                    condition.pretty_print(),
                    then_branch.pretty_print(),
                    else_str
                )
            }
            ExpressionKind::AttachImplementation {
                type_expr,
                implementation,
                ..
            } => {
                let implementation_str = implementation.pretty_print();
                let elipses = "...";
                let max_len = 30;
                let implementation_str = if implementation_str.len() > max_len {
                    format!(
                        "{}{}",
                        &implementation_str[..max_len - elipses.len()],
                        elipses
                    )
                } else {
                    implementation_str
                };
                format!("{} @ {}", type_expr.pretty_print(), implementation_str)
            }
            ExpressionKind::Function {
                parameter,
                return_type,
                body,
            } => {
                let return_str = return_type
                    .as_ref()
                    .map(|r| format!(" => {}", r.pretty_print()))
                    .unwrap_or_default();
                format!(
                    "({}{}) => {}",
                    parameter.pretty_print(),
                    return_str,
                    body.pretty_print()
                )
            }
            ExpressionKind::FunctionType {
                parameter,
                return_type,
            } => format!(
                "({}) => {}",
                parameter.pretty_print(),
                return_type.pretty_print()
            ),
            ExpressionKind::Struct(fields) => {
                let field_strs: Vec<String> = fields
                    .iter()
                    .map(|(name, value)| format!("{} = {}", name.name, value.pretty_print()))
                    .collect();
                format!("{{ {} }}", field_strs.join(", "))
            }
            ExpressionKind::Literal(lit) => match lit {
                ExpressionLiteral::Number(n) => n.to_string(),
                ExpressionLiteral::Boolean(b) => b.to_string(),
                ExpressionLiteral::Target(t) => match t {
                    TargetLiteral::JSTarget => "js".to_string(),
                    TargetLiteral::WasmTarget => "wasm".to_string(),
                },
            },
            ExpressionKind::Identifier(id) => id.name.clone(),
            ExpressionKind::Operation {
                operator,
                left,
                right,
            } => format!(
                "{} {} {}",
                left.pretty_print(),
                operator,
                right.pretty_print()
            ),
            ExpressionKind::Assignment { target, expr } => {
                format!("{} = {}", target.pretty_print(), expr.pretty_print())
            }
            ExpressionKind::FunctionCall { function, argument } => {
                format!("{}({})", function.pretty_print(), argument.pretty_print())
            }
            ExpressionKind::PropertyAccess { object, property } => {
                format!("{}.{}", object.pretty_print(), property)
            }
            ExpressionKind::Binding(binding) => {
                format!(
                    "{} := {}",
                    binding.pattern.pretty_print(),
                    binding.expr.pretty_print()
                )
            }
            ExpressionKind::Block(exprs) => {
                let expr_strs: Vec<String> = exprs.iter().map(|e| e.pretty_print()).collect();
                format!("( {} )", expr_strs.join("; "))
            }
            ExpressionKind::Diverge {
                value,
                divergance_type,
            } => {
                let keyword = match divergance_type {
                    DivergeExpressionType::Return => "return",
                    DivergeExpressionType::Break => "break",
                };
                format!("{} {}", keyword, value.pretty_print())
            }
            ExpressionKind::Loop { body } => format!("loop {}", body.pretty_print()),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Binding {
    pub pattern: BindingPattern,
    pub expr: Expression,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

fn parse_match_expression<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(Expression, &'a str), Diagnostic>> {
    const KEYWORD: &str = "match";
    if !file.starts_with(KEYWORD) {
        return None;
    }

    let after_keyword = &file[KEYWORD.len()..];
    if after_keyword
        .chars()
        .next()
        .filter(|c| c.is_alphanumeric() || *c == '_')
        .is_some()
    {
        return None;
    }

    Some(parse_match_expression_with_source(source, file))
}

fn parse_match_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(Expression, &'a str), Diagnostic> {
    let start_slice = file;
    let mut remaining = file
        .strip_prefix("match")
        .ok_or_else(|| diagnostic_here(source, file, 5, "Expected match"))?;

    remaining = parse_optional_whitespace(remaining);
    let (value, mut remaining) = parse_operation_expression_with_guard(source, remaining)?;
    remaining = parse_optional_whitespace(remaining);
    remaining = remaining
        .strip_prefix("with")
        .ok_or_else(|| diagnostic_here(source, remaining, 4, "Expected with after match value"))?;
    remaining = parse_optional_whitespace(remaining);

    let mut branches = Vec::new();

    remaining = remaining
        .strip_prefix("(")
        .ok_or_else(|| diagnostic_here(source, remaining, 1, "Expected ( to start match"))?;

    loop {
        remaining = parse_optional_whitespace(remaining);
        if let Some(rest) = remaining.strip_prefix(")") {
            remaining = rest;
            break;
        }
        let (branch_expr, rest) = parse_operation_expression_with_guard(source, remaining)?;
        let ExpressionKind::Function {
            parameter, body, ..
        } = branch_expr.kind
        else {
            return Err(diagnostic_here(
                source,
                remaining,
                1,
                "Expected match branch to be a function",
            ));
        };
        branches.push((parameter, *body));
        remaining = parse_optional_whitespace(rest);

        if let Some(rest) = remaining.strip_prefix(",") {
            remaining = parse_optional_whitespace(rest);
        }
    }

    let span = consumed_span(source, start_slice, remaining);

    Ok((
        ExpressionKind::Match {
            value: Box::new(value),
            branches,
        }
        .with_span(span),
        remaining,
    ))
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
    let (condition, mut after_condition) =
        parse_assignment_expression_with_source(source, remaining)?;
    after_condition = parse_optional_whitespace(after_condition);
    let remaining = after_condition.strip_prefix("then").ok_or_else(|| {
        diagnostic_here(
            source,
            after_condition,
            4,
            "Expected then after if condition",
        )
    })?;

    let remaining = parse_optional_whitespace(remaining);

    let (then_branch, remaining) = parse_operation_expression_with_source(source, remaining)?;

    let remaining = parse_optional_whitespace(remaining);

    let else_branch = if let Some(rest) = remaining.strip_prefix("else") {
        let rest = parse_optional_whitespace(rest);
        if let Some(if_parse) = parse_if_expression(source, rest) {
            let (else_expr, remaining) = if_parse?;
            Some((Some(else_expr), remaining))
        } else {
            let (else_expr, remaining) = parse_operation_expression_with_source(source, rest)?;
            Some((Some(else_expr), remaining))
        }
    } else {
        None
    };

    let (else_branch, remaining) = else_branch.unwrap_or((None, remaining));

    let span = consumed_span(source, start_slice, remaining);
    Ok((
        ExpressionKind::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch.unwrap_or(Expression {
                kind: ExpressionKind::Struct(vec![]),
                span: SourceSpan::new(span.end(), 0),
            })),
        }
        .with_span(span),
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

fn parse_semicolon<'a>(source: &'a str, file: &'a str) -> Result<&'a str, Diagnostic> {
    file.strip_prefix(";")
        .ok_or_else(|| diagnostic_here(source, file, 1, "Expected ;"))
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

    if identifier.chars().next().unwrap().is_ascii_digit() {
        return None;
    }

    let remaining = &file[identifier.len()..];
    Some((Identifier::new(identifier), remaining))
}

pub fn parse_literal(file: &str) -> Option<(ExpressionLiteral, &str)> {
    let mut consumed = 0;
    let is_negative = matches!(file.chars().next(), Some('-'));

    let digits = file
        .chars()
        .skip(is_negative as usize)
        .take_while(|c| c.is_ascii_digit())
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

#[test]
fn test_enum_pattern_parse2() {
    let source = "Option::Some(value)";
    let (pattern, remaining) = parse_operation_expression_with_guard(source, source).unwrap();
    assert!(remaining.is_empty());
    let pattern = pattern_expression_to_binding_pattern(pattern).unwrap();

    match pattern {
        BindingPattern::EnumVariant {
            enum_type,
            variant,
            payload,
            ..
        } => {
            match enum_type.kind {
                ExpressionKind::Identifier(Identifier { name, .. }) if name == "Option" => {}
                _ => panic!("Expected enum type identifier 'Option'"),
            }
            assert_eq!(variant.name, "Some");
            match payload {
                Some(boxed_pattern) => match *boxed_pattern {
                    BindingPattern::Identifier(Identifier { name, .. }, _) if name == "value" => {}
                    _ => panic!("Expected payload identifier 'value'"),
                },
                None => panic!("Expected payload in enum variant pattern"),
            }
        }
        _ => panic!("Expected EnumVariant binding pattern"),
    }
}

fn expression_to_lvalue(expression: Expression) -> Result<LValue, Diagnostic> {
    let span = expression.span;
    match expression.kind {
        ExpressionKind::Identifier(identifier) => Ok(LValue::Identifier(identifier, span)),
        ExpressionKind::PropertyAccess { object, property } => {
            let object_lvalue = expression_to_lvalue(*object)?;
            Ok(LValue::PropertyAccess {
                object: Box::new(object_lvalue),
                property,
                span,
            })
        }
        _ => {
            Err(Diagnostic::new("Invalid binding pattern expression").with_span(expression.span()))
        }
    }
}

fn pattern_expression_to_binding_pattern(
    pattern_expression: Expression,
) -> Result<BindingPattern, Diagnostic> {
    let span = pattern_expression.span;
    match pattern_expression.kind {
        ExpressionKind::Literal(expression_literal) => {
            Ok(BindingPattern::Literal(expression_literal, span))
        }
        ExpressionKind::Identifier(identifier) => Ok(BindingPattern::Identifier(identifier, span)),
        ExpressionKind::Struct(items) => {
            let mut field_patterns = Vec::new();
            for (identifier, expr) in items {
                let field_pattern = pattern_expression_to_binding_pattern(expr)?;
                field_patterns.push((identifier, field_pattern));
            }
            Ok(BindingPattern::Struct(field_patterns, span))
        }
        ExpressionKind::Operation {
            operator,
            left,
            right,
        } if operator == ":" => {
            let left_pattern = pattern_expression_to_binding_pattern(*left)?;
            Ok(BindingPattern::TypeHint(
                Box::new(left_pattern),
                right,
                span,
            ))
        }
        ExpressionKind::FunctionCall { function, argument } => match *function {
            Expression {
                kind:
                    ExpressionKind::EnumAccess {
                        enum_expr, variant, ..
                    },
                ..
            } => Ok(BindingPattern::EnumVariant {
                enum_type: enum_expr,
                variant,
                payload: Some(Box::new(pattern_expression_to_binding_pattern(*argument)?)),
                span,
            }),
            other => Ok(BindingPattern::Annotated {
                annotations: extract_binding_annotations_from_expression(other)?,
                pattern: Box::new(pattern_expression_to_binding_pattern(*argument)?),
                span,
            }),
        },
        ExpressionKind::EnumAccess {
            enum_expr, variant, ..
        } => Ok(BindingPattern::EnumVariant {
            enum_type: enum_expr,
            variant,
            payload: None,
            span,
        }),

        _ => Err(Diagnostic::new("Invalid binding pattern expression").with_span(span)),
    }
}

fn extract_binding_annotations_from_expression(
    expression: Expression,
) -> Result<Vec<BindingAnnotation>, Diagnostic> {
    let span = expression.span();
    match expression.kind {
        ExpressionKind::Identifier(Identifier { name: id, .. }) if id == "mut" => {
            Ok(vec![BindingAnnotation::Mutable(span)])
        }
        ExpressionKind::FunctionCall { function, argument } => match *function {
            Expression {
                kind: ExpressionKind::Identifier(Identifier { name: id, .. }),
                span: ann_span,
            } if id == "export" => Ok(vec![BindingAnnotation::Export(*argument, ann_span)]),
            other => {
                let mut annotations = extract_binding_annotations_from_expression(other)?;
                annotations.extend(extract_binding_annotations_from_expression(*argument)?);
                Ok(annotations)
            }
        },
        _ => Err(Diagnostic::new("Invalid binding annotation").with_span(span)),
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
    let operator_chars: Vec<char> = vec![
        '+', '-', '*', '/', '=', '!', '<', '>', '&', '|', '^', ':', '.',
    ];

    let stop_sequences = vec![";", ")", ",", "}", "]"];
    for stop_sequence in stop_sequences {
        if file.starts_with(stop_sequence) {
            return None;
        }
    }

    let stop_words = [
        "if", "then", "else", "match", "with", "fn", "let", "return", "break", "loop", "while",
        "do",
    ];
    let word = file
        .chars()
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect::<String>();
    if stop_words.contains(&word.as_str()) {
        return None;
    }

    if file.is_empty() {
        return None;
    }

    let operator = file
        .chars()
        .take_while(|c| operator_chars.contains(c))
        .collect::<String>();

    let remaining = &file[operator.len()..];
    Some((operator, remaining))
}

fn operator_precedence(operator: &str) -> u8 {
    match operator {
        "" | "::" | "." => 8,
        ":" => 7,
        "*" | "/" => 6,
        "+" | "-" => 5,
        "==" | "!=" | "<" | ">" | "<=" | ">=" => 4,
        "&&" => 3,
        "||" | "^" => 2,
        "=>" | "->" | "|>" => 1,
        ":=" | "=" => 0,
        _ => 5,
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
                return Ok((ExpressionKind::Struct(items).with_span(span), rest));
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
                        return Ok((ExpressionKind::Struct(items).with_span(span), rest));
                    }

                    remaining = remaining.strip_prefix(",").ok_or_else(|| {
                        diagnostic_here(source, remaining, 1, "Expected , or } in struct literal")
                    })?;
                    continue;
                }
            }

            let (value_expr, rest) = parse_operation_expression_with_source(source, remaining)?;
            items.push((Identifier::new(tuple_index.to_string()), value_expr));
            tuple_index += 1;
            remaining = rest;
            remaining = parse_optional_whitespace(remaining);

            if let Some(rest) = remaining.strip_prefix("}") {
                let span = consumed_span(source, start_slice, rest);
                return Ok((ExpressionKind::Struct(items).with_span(span), rest));
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

fn parse_return_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(Expression, &'a str), Diagnostic>> {
    const KEYWORD: &str = "return";
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

    let start_slice = file;
    let remaining = parse_optional_whitespace(after_keyword);

    let value_start = remaining.chars().next();
    let (value, rest) = if remaining.is_empty()
        || matches!(value_start, Some(';') | Some(')') | Some(']') | Some('}') | Some(','))
    {
        (None, remaining)
    } else {
        match parse_operation_expression_with_guard(source, remaining) {
            Ok((expr, rest)) => (Some(expr), rest),
            Err(err) => return Some(Err(err)),
        }
    };

    let span = consumed_span(source, start_slice, rest);
    Some(Ok((
        ExpressionKind::Diverge {
            divergance_type: DivergeExpressionType::Return,
            value: Box::new(value.unwrap_or(Expression::new(ExpressionKind::Struct(vec![]), span))),
        }
        .with_span(span),
        rest,
    )))
}

fn parse_break_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(Expression, &'a str), Diagnostic>> {
    const KEYWORD: &str = "break";
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

    let start_slice = file;
    let remaining = parse_optional_whitespace(after_keyword);
    let value_start = remaining.chars().next();

    let (value, rest) = if remaining.is_empty()
        || matches!(value_start, Some(';') | Some(')') | Some(']') | Some('}') | Some(','))
    {
        (None, remaining)
    } else {
        match parse_operation_expression_with_guard(source, remaining) {
            Ok((expr, rest)) => (Some(expr), rest),
            Err(err) => return Some(Err(err)),
        }
    };

    let span = consumed_span(source, start_slice, rest);
    Some(Ok((
        ExpressionKind::Diverge {
            divergance_type: DivergeExpressionType::Break,
            value: Box::new(value.unwrap_or(Expression::new(ExpressionKind::Struct(vec![]), span))),
        }
        .with_span(span),
        rest,
    )))
}

fn parse_loop_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(Expression, &'a str), Diagnostic>> {
    const KEYWORD: &str = "loop";
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

    let start_slice = file;
    let remaining = parse_optional_whitespace(after_keyword);
    let Some(group_parsed) = parse_grouping_expression_with_source(source, remaining) else {
        return Some(Err(diagnostic_here(
            source,
            remaining,
            1,
            "Expected loop body expression",
        )));
    };

    match group_parsed {
        Ok((body, rest)) => {
            let span = consumed_span(source, start_slice, rest);
            Some(Ok((
                ExpressionKind::Loop {
                    body: Box::new(body),
                }
                .with_span(span),
                rest,
            )))
        }
        Err(err) => Some(Err(err)),
    }
}

fn parse_while_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(Expression, &'a str), Diagnostic>> {
    const KEYWORD: &str = "while";
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

    let start_slice = file;
    let remaining = parse_optional_whitespace(after_keyword);
    let (condition, after_condition) =
        match parse_operation_expression_with_source(source, remaining) {
            Ok(parsed) => parsed,
            Err(err) => return Some(Err(err)),
        };

    let remaining = parse_optional_whitespace(after_condition);
    let Some(remaining) = remaining.strip_prefix("do") else {
        return Some(Err(diagnostic_here(
            source,
            remaining,
            2,
            "Expected do after while condition",
        )));
    };
    let remaining = parse_optional_whitespace(remaining);
    let Some(group_parsed) = parse_grouping_expression_with_source(source, remaining) else {
        return Some(Err(diagnostic_here(
            source,
            remaining,
            1,
            "Expected while body expression",
        )));
    };

    match group_parsed {
        Ok((body, rest)) => {
            let span = consumed_span(source, start_slice, rest);
            let condition_span = condition.span();
            Some(Ok((
                ExpressionKind::Loop {
                    body: Box::new(
                        ExpressionKind::If {
                            condition: Box::new(condition),
                            then_branch: Box::new(body),
                            else_branch: Box::new(
                                ExpressionKind::Diverge {
                                    value: Box::new(Expression::new(
                                        ExpressionKind::Struct(vec![]),
                                        span,
                                    )),
                                    divergance_type: DivergeExpressionType::Break,
                                }
                                .with_span(condition_span),
                            ),
                        }
                        .with_span(condition_span),
                    ),
                }
                .with_span(span),
                rest,
            )))
        }
        Err(err) => Some(Err(err)),
    }
}

#[cfg(test)]
pub fn parse_isolated_expression(file: &str) -> Result<(Expression, &str), Diagnostic> {
    parse_isolated_expression_with_source(file, file)
}

fn parse_isolated_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(Expression, &'a str), Diagnostic> {
    parse_isolated_expression_with_source_with_guard(source, file)
}

fn parse_isolated_expression_with_source_with_guard<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(Expression, &'a str), Diagnostic> {
    if let Some(return_parse) = parse_return_expression_with_source(source, file) {
        return return_parse;
    }
    if let Some(break_parse) = parse_break_expression_with_source(source, file) {
        return break_parse;
    }
    if let Some(while_parse) = parse_while_expression_with_source(source, file) {
        return while_parse;
    }
    if let Some(loop_parse) = parse_loop_expression_with_source(source, file) {
        return loop_parse;
    }
    if let Some(match_parse) = parse_match_expression(source, file) {
        return match_parse;
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
        return Ok((
            ExpressionKind::Identifier(identifier).with_span(span),
            remaining,
        ));
    }
    if let Some((literal, remaining)) = parse_literal(file) {
        let span = consumed_span(source, file, remaining);
        return Ok((ExpressionKind::Literal(literal).with_span(span), remaining));
    }
    Err(diagnostic_here(
        source,
        file,
        file.chars().next().map(|c| c.len_utf8()).unwrap_or(1),
        "Expected expression",
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
    parse_operation_expression_with_guard(source, file)
}

fn parse_operation_expression_with_guard<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(Expression, &'a str), Diagnostic> {
    parse_operation_expression_with_min_precedence(source, file, 0)
}

fn parse_assignment_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(Expression, &'a str), Diagnostic> {
    parse_assignment_expression_with_guard(source, file)
}

fn parse_assignment_expression_with_guard<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(Expression, &'a str), Diagnostic> {
    let trimmed = parse_optional_whitespace(file);
    let start_slice = trimmed;
    if let Some(Ok((target, after_target))) = parse_lvalue(source, trimmed) {
        let after_target = parse_optional_whitespace(after_target);
        if let Some(after_equals) = after_target.strip_prefix('=')
            && !after_equals.starts_with('=')
        {
            let after_equals = parse_optional_whitespace(after_equals);
            let (expr, remaining) = parse_operation_expression_with_guard(source, after_equals)?;
            let span = consumed_span(source, start_slice, remaining);
            return Ok((
                ExpressionKind::Assignment {
                    target,
                    expr: Box::new(expr),
                }
                .with_span(span),
                remaining,
            ));
        }
    }

    parse_operation_expression_with_guard(source, file)
}

fn parse_lvalue<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(LValue, &'a str), Diagnostic>> {
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
        let (property_identifier, rest) =
            if let Some((identifier, rest)) = parse_identifier(after_dot) {
                (identifier, rest)
            } else {
                let tuple_index = after_dot
                    .chars()
                    .take_while(|c| c.is_ascii_digit())
                    .collect::<String>();

                if tuple_index.is_empty() {
                    return Some(Err(diagnostic_here(
                        source,
                        after_dot,
                        after_dot.chars().next().map(|c| c.len_utf8()).unwrap_or(1),
                        "Expected identifier after . in assignment target",
                    )));
                }

                let rest = &after_dot[tuple_index.len()..];
                (Identifier::new(tuple_index), rest)
            };

        current_span = consumed_span(source, start_slice, rest);
        lvalue = LValue::PropertyAccess {
            object: Box::new(lvalue),
            property: property_identifier.name,
            span: current_span,
        };
        remaining = rest;
    }

    Some(Ok((lvalue, remaining)))
}

fn parse_operation_expression_with_min_precedence<'a>(
    source: &'a str,
    file: &'a str,
    min_precedence: u8,
) -> Result<(Expression, &'a str), Diagnostic> {
    fn parse_operations<'a>(
        source: &'a str,
        file: &'a str,
        min_precedence: u8,
    ) -> Result<(Vec<Expression>, Vec<String>, &'a str), Diagnostic> {
        let mut expressions: Vec<Expression> = Vec::new();
        let mut operators: Vec<String> = Vec::new();
        let (expression, mut remaining) = parse_isolated_expression_with_source(source, file)?;
        remaining = parse_optional_whitespace(remaining);
        expressions.push(expression);
        while let Some((operator, rest)) = parse_operator(remaining) {
            // Stop if operator precedence is below minimum
            if operator_precedence(&operator) < min_precedence {
                break;
            }
            let rest = parse_optional_whitespace(rest);
            let (next_expression, rest) = parse_isolated_expression_with_source(source, rest)?;
            let rest = parse_optional_whitespace(rest);
            operators.push(operator);
            expressions.push(next_expression);
            remaining = rest;
        }
        Ok((expressions, operators, remaining))
    }

    let (expressions, operators, remaining) = parse_operations(source, file, min_precedence)?;

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
        let processed_operation = match operator.as_str() {
            "=" => {
                let target = expression_to_lvalue(left)?;
                ExpressionKind::Assignment {
                    target,
                    expr: Box::new(right),
                }
            }
            ":=" => {
                let pattern = pattern_expression_to_binding_pattern(left)?;
                ExpressionKind::Binding(Box::new(Binding {
                    pattern,
                    expr: right,
                }))
            }
            "=>" => {
                let pattern = pattern_expression_to_binding_pattern(left)?;
                ExpressionKind::Function {
                    parameter: pattern,
                    return_type: None,
                    body: Box::new(right),
                }
            }
            "->" => ExpressionKind::FunctionType {
                parameter: Box::new(left),
                return_type: Box::new(right),
            },
        "::" => {
            let ExpressionKind::Identifier(variant) = right.kind else {
                return Err(diagnostic_here(
                    source,
                    "",
                    1,
                    "Expected identifier as enum variant in enum access",
                ));
            };
            ExpressionKind::EnumAccess {
                enum_expr: Box::new(left),
                variant,
            }
        }
            "." => match right.kind {
                ExpressionKind::Identifier(property) => ExpressionKind::PropertyAccess {
                    object: Box::new(left),
                    property: property.name,
                },
                ExpressionKind::Literal(ExpressionLiteral::Number(num)) => {
                    ExpressionKind::PropertyAccess {
                        object: Box::new(left),
                        property: num.to_string(),
                    }
                }
                _ => {
                    return Err(diagnostic_here(
                        source,
                        "",
                        1,
                        "Expected identifier as property name in property access",
                    ));
                }
            },
        "" => ExpressionKind::FunctionCall {
            function: Box::new(left),
            argument: Box::new(right),
        },
        "|>" => ExpressionKind::FunctionCall {
            function: Box::new(right),
            argument: Box::new(left),
        },
        operator => ExpressionKind::Operation {
            operator: operator.to_string(),
            left: Box::new(left),
            right: Box::new(right),
        },
        };
        operand_stack.push(Expression::new(processed_operation, span));
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
        while operator_stack
            .last()
            .is_some_and(|existing| operator_precedence(existing) >= operator_precedence(&operator))
        {
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

        if let Some(ch) = remaining.chars().next()
            && terminators.contains(&ch)
        {
            break;
        }

        let (expression, rest) = parse_operation_expression_with_guard(source, remaining)?;
        expressions.push(expression);
        remaining = parse_optional_whitespace(rest);
        ended_with_semicolon = false;

        if remaining.is_empty() {
            break;
        }

        if let Some(ch) = remaining.chars().next()
            && terminators.contains(&ch)
        {
            break;
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

    if ended_with_semicolon && let Some(last_span) = expressions.last().map(|expr| expr.span()) {
        expressions
            .push(ExpressionKind::Struct(vec![]).with_span(SourceSpan::new(last_span.end(), 0)));
    }

    if expressions.len() == 1 {
        Ok((expressions.into_iter().next().unwrap(), remaining))
    } else {
        let span = expressions
            .iter()
            .skip(1)
            .fold(expressions[0].span(), |acc, expr| acc.merge(&expr.span()));
        Ok((
            Expression::new(ExpressionKind::Block(expressions), span),
            remaining,
        ))
    }
}

#[test]
fn parse_basic_let() {
    let Ok((parsed, "")) = parse_block(
        "
x := 42;
y: i32 := x
    ",
    ) else {
        panic!()
    };

    let ExpressionKind::Block(parsed) = parsed.kind else {
        panic!()
    };
    assert_eq!(parsed.len(), 2);

    let ExpressionKind::Binding(binding1) = &parsed[0].kind else {
        panic!()
    };
    assert_eq!(
        matches!(binding1.pattern, BindingPattern::Identifier(ref id, _) if id.name == "x"),
        true
    );
    assert_eq!(
        matches!(&binding1.expr.kind, ExpressionKind::Literal(ExpressionLiteral::Number(lit)) if *lit == 42),
        true
    );

    let ExpressionKind::Binding(binding2) = &parsed[1].kind else {
        panic!()
    };
    assert_eq!(
        matches!(&binding2.expr.kind, ExpressionKind::Identifier(lit) if lit.name == "x"),
        true
    );
    let BindingPattern::TypeHint(binding2, binding2_type, _) = &binding2.pattern else {
        panic!()
    };
    assert!(matches!(**binding2, BindingPattern::Identifier(ref hint, _) if hint.name == "y"));
    assert_eq!(
        matches!(binding2_type.kind, ExpressionKind::Identifier(ref hint) if hint.name == "i32"),
        true
    );
}

#[test]
fn parse_binding_with_export_annotation() {
    let Ok((parsed, "")) = parse_block(
        "
(export js) foo := 42;
(export wasm) (export js) bar := foo;
bar
    ",
    ) else {
        panic!()
    };

    let ExpressionKind::Block(parsed) = parsed.kind else {
        panic!()
    };
    assert_eq!(parsed.len(), 3);

    let ExpressionKind::Binding(binding1) = &parsed[0].kind else {
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
            Expression {
                kind: ExpressionKind::Identifier(identifier),
                ..
            } => assert_eq!(identifier.name, "js"),
            other => panic!("expected target identifier, got {:?}", other),
        },
        other => panic!("unexpected annotation: {:?}", other),
    }

    let ExpressionKind::Binding(binding2) = &parsed[1].kind else {
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
fn parse_struct_pattern_with_inner_multiple_annotations() {
    let Ok((parsed, "")) = parse_block(
        "
{ foo = (export js) mut foo_binding, bar } := value;
foo_binding
    ",
    ) else {
        panic!()
    };

    let ExpressionKind::Block(parsed) = parsed.kind else {
        panic!()
    };
    let ExpressionKind::Binding(binding) = &parsed[0].kind else {
        panic!()
    };
    let BindingPattern::Struct(fields, _) = &binding.pattern else {
        panic!("expected struct pattern")
    };
    let (_, foo_pattern) = fields
        .iter()
        .find(|(identifier, _)| identifier.name == "foo")
        .expect("missing foo field");
    let BindingPattern::Annotated { annotations, .. } = foo_pattern else {
        panic!("expected annotated foo field")
    };
    assert_eq!(annotations.len(), 2);
    let BindingAnnotation::Export(_, _) = &annotations[0] else {
        panic!("expected export annotation first")
    };
    let BindingAnnotation::Mutable(_) = &annotations[1] else {
        panic!("expected mutable annotation second")
    };
}

#[test]
fn parse_struct_pattern_with_inner_export_annotation() {
    let Ok((parsed, "")) = parse_block(
        "
{ foo = (export js) foo_binding, bar } := value;
foo_binding
    ",
    ) else {
        panic!()
    };

    let ExpressionKind::Block(parsed) = parsed.kind else {
        panic!()
    };
    let ExpressionKind::Binding(binding) = &parsed[0].kind else {
        panic!()
    };
    let BindingPattern::Struct(fields, _) = &binding.pattern else {
        panic!("expected struct pattern")
    };
    let (_, foo_pattern) = fields
        .iter()
        .find(|(identifier, _)| identifier.name == "foo")
        .expect("missing foo field");
    let BindingPattern::Annotated { annotations, .. } = foo_pattern else {
        panic!("expected annotated foo field")
    };
    assert_eq!(annotations.len(), 1);
}

#[test]
fn parse_mutable_binding_and_assignment() {
    let Ok((parsed, "")) = parse_block(
        "
mut foo := 1;
foo = 2
    ",
    ) else {
        panic!()
    };

    let ExpressionKind::Block(parsed) = parsed.kind else {
        panic!()
    };

    let ExpressionKind::Binding(binding) = &parsed[0].kind else {
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

    let ExpressionKind::Assignment { target, .. } = &parsed[1].kind else {
        panic!("expected assignment expression")
    };
    assert!(matches!(
        target,
        LValue::Identifier(Identifier { name, .. }, _) if name == "foo"
    ));
}

#[test]
fn parse_operation_expression_precedence() {
    let Ok((expr, "")) = parse_operation_expression("1 + 2 * 3 / 4 - 5") else {
        panic!();
    };
    let ExpressionKind::Operation {
        operator,
        left,
        right,
    } = &expr.kind
    else {
        panic!()
    };
    assert_eq!(operator, "-");
    assert!(matches!(
        &right.kind,
        ExpressionKind::Literal(ExpressionLiteral::Number(5))
    ));

    let ExpressionKind::Operation {
        operator,
        left,
        right,
    } = &left.kind
    else {
        panic!();
    };
    assert_eq!(operator, "+");
    assert!(matches!(
        &left.kind,
        ExpressionKind::Literal(ExpressionLiteral::Number(1))
    ));

    let ExpressionKind::Operation {
        operator,
        left,
        right,
    } = &right.kind
    else {
        panic!();
    };
    assert_eq!(operator, "/");
    assert!(matches!(
        &right.kind,
        ExpressionKind::Literal(ExpressionLiteral::Number(4))
    ));

    let ExpressionKind::Operation {
        operator,
        left,
        right,
    } = &left.kind
    else {
        panic!();
    };
    assert_eq!(operator, "*");
    assert!(matches!(
        &left.kind,
        ExpressionKind::Literal(ExpressionLiteral::Number(2))
    ));
    assert!(matches!(
        &right.kind,
        ExpressionKind::Literal(ExpressionLiteral::Number(3))
    ));
}

#[test]
fn parse_function_binding_and_call() {
    let (expr, remaining) = parse_block(
        "
foo := (bar: i32) => bar + 1;
foo(123)
    ",
    )
    .unwrap();

    assert!(remaining.trim().is_empty());

    let ExpressionKind::Block(items) = &expr.kind else {
        panic!("expected block with binding and call");
    };
    assert_eq!(items.len(), 2);

    let ExpressionKind::Binding(binding) = &items[0].kind else {
        panic!("first expression should be binding");
    };
    assert!(matches!(
        &binding.pattern,
        BindingPattern::Identifier(Identifier { name, .. }, _) if name == "foo"
    ));

    let ExpressionKind::Function {
        parameter,
        return_type,
        body,
    } = &binding.expr.kind
    else {
        panic!(
            "binding should store function expression, got {:?}",
            binding.expr.kind
        );
    };

    let BindingPattern::TypeHint(inner, type_hint, _) = parameter else {
        panic!("parameter should include type hint");
    };
    assert!(matches!(
        inner.as_ref(),
        BindingPattern::Identifier(Identifier { name, .. }, _) if name == "bar"
    ));
    assert!(matches!(
        &type_hint.kind,
        ExpressionKind::Identifier(Identifier { name, .. }) if name == "i32"
    ));
    assert!(matches!(return_type.as_ref(), None));
    assert!(matches!(
        &body.kind,
        ExpressionKind::Operation {
            operator: op,
            ..
        } if op == "+"
    ));

    let ExpressionKind::FunctionCall { function, argument } = &items[1].kind else {
        panic!("expected function call as second expression");
    };
    assert!(matches!(
        &function.kind,
        ExpressionKind::Identifier(Identifier { name, .. }) if name == "foo"
    ));
    assert!(matches!(
        &argument.kind,
        ExpressionKind::Literal(ExpressionLiteral::Number(123))
    ));
}

#[test]
fn parse_function_struct_parameter_pattern() {
    let (expr, remaining) = parse_operation_expression(
        "{bar1: i32, bar2: i32} => (
    bar1 + bar2
)",
    )
    .unwrap();
    assert!(remaining.trim().is_empty());

    let ExpressionKind::Function { parameter, .. } = &expr.kind else {
        panic!("expected function expression");
    };
    let BindingPattern::Struct(fields, _) = parameter else {
        panic!("expected struct binding pattern for function parameter");
    };
    assert_eq!(fields.len(), 2);

    let (first_name, first_pattern) = &fields[0];
    assert_eq!(first_name.name, "0");
    let BindingPattern::TypeHint(first_inner, first_type, _) = first_pattern else {
        panic!("expected type hint for first parameter");
    };
    assert!(matches!(
        first_inner.as_ref(),
        BindingPattern::Identifier(Identifier { name, .. }, _) if name == "bar1"
    ));
    assert!(matches!(
        &first_type.kind,
        ExpressionKind::Identifier(Identifier { name, .. }) if name == "i32"
    ));

    let (second_name, second_pattern) = &fields[1];
    assert_eq!(second_name.name, "1");
    let BindingPattern::TypeHint(second_inner, second_type, _) = second_pattern else {
        panic!("expected type hint for second parameter");
    };
    assert!(matches!(
        second_inner.as_ref(),
        BindingPattern::Identifier(Identifier { name, .. }, _) if name == "bar2"
    ));
    assert!(matches!(
        &second_type.kind,
        ExpressionKind::Identifier(Identifier { name, .. }) if name == "i32"
    ));
}

#[test]
fn parse_struct_literal_named_and_tuple_fields() {
    let (expr, remaining) = parse_isolated_expression("{foo = 10, 20, bar = 30}").unwrap();
    assert!(remaining.trim().is_empty());
    let ExpressionKind::Struct(items) = &expr.kind else {
        panic!("expected struct literal");
    };
    assert_eq!(items.len(), 3);
    assert_eq!(items[0].0.name, "foo");
    assert_eq!(items[1].0.name, "0");
    assert_eq!(items[2].0.name, "bar");
}

#[test]
fn parse_struct_binding_pattern_named_fields() {
    let source = "{foo = first: i32, second: i32}";
    let (pattern, remaining) =
        parse_operation_expression_with_guard(source, source).expect("pattern parse");
    assert!(remaining.trim().is_empty());
    let pattern = pattern_expression_to_binding_pattern(pattern).unwrap();
    let BindingPattern::Struct(fields, _) = pattern else {
        panic!("expected struct pattern");
    };
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].0.name, "foo");
    assert_eq!(fields[1].0.name, "0");
}

#[test]
fn parse_struct_property_access_chain() {
    let (expr, remaining) = parse_operation_expression("foo.bar.baz").expect("parse");
    assert!(remaining.trim().is_empty());

    let ExpressionKind::PropertyAccess { object, property } = &expr.kind else {
        panic!("expected outer property access");
    };
    assert_eq!(property, "baz");

    let ExpressionKind::PropertyAccess {
        object: inner_object,
        property: inner_property,
    } = &object.kind
    else {
        panic!("expected inner property access");
    };
    assert_eq!(inner_property, "bar");
    assert!(matches!(
        &inner_object.kind,
        ExpressionKind::Identifier(Identifier { name, .. }) if name == "foo"
    ));
}

#[test]
fn parse_struct_property_access_then_call() {
    let (expr, remaining) = parse_operation_expression("foo.bar baz").expect("parse");
    assert!(remaining.trim().is_empty());

    let ExpressionKind::FunctionCall { function, argument } = &expr.kind else {
        panic!("expected function call");
    };
    assert!(matches!(
        &argument.kind,
        ExpressionKind::Identifier(Identifier { name, .. }) if name == "baz"
    ));
    let ExpressionKind::PropertyAccess { object, property } = &function.kind else {
        panic!("expected property access as function part");
    };
    assert_eq!(property, "bar");
    assert!(matches!(
        &object.kind,
        ExpressionKind::Identifier(Identifier { name, .. }) if name == "foo"
    ));
}

#[test]
fn diagnostics_include_binding_pattern_source_reference() {
    let source = ":= 5;";
    let err = parse_block(source).expect_err("binding should fail");
    let rendered = err.render_with_source(source);
    assert!(rendered.contains("Expected expression"));
    assert!(rendered.contains("line 1, column 1"));
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
