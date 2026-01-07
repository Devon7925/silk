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
    ArrayIndex {
        array: Box<LValue>,
        index: Box<Expression>,
        span: SourceSpan,
    },
}

impl LValue {
    pub fn span(&self) -> SourceSpan {
        match self {
            LValue::Identifier(_, span) => *span,
            LValue::PropertyAccess { span, .. } => *span,
            LValue::ArrayIndex { span, .. } => *span,
        }
    }

    pub fn pretty_print(&self) -> String {
        match self {
            LValue::Identifier(id, _) => id.name.clone(),
            LValue::PropertyAccess {
                object, property, ..
            } => {
                format!("{}.{}", object.pretty_print(), property)
            }
            LValue::ArrayIndex { array, index, .. } => {
                format!("{}({})", array.pretty_print(), index.pretty_print())
            }
        }
    }

    pub fn get_used_identifiers(&self) -> HashSet<Identifier> {
        let mut current = self;
        loop {
            match current {
                LValue::Identifier(identifier, ..) => {
                    return HashSet::from([identifier.clone()]);
                }
                LValue::PropertyAccess { object, .. } => {
                    current = object;
                }
                LValue::ArrayIndex { array, .. } => {
                    current = array;
                }
            }
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
        pretty_print_task(PrettyTask::Pattern(self))
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
    Char(u8),
    String(Vec<u8>),
    Target(TargetLiteral),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntrinsicType {
    I32,
    U8,
    Boolean,
    Type,
    Target,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryIntrinsicOperator {
    BooleanNot,
    EnumFromStruct,
    MatchFromStruct,
    UseFromString,
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
    ArrayIndex {
        array: Box<Expression>,
        index: Box<Expression>,
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
        pretty_print_task(PrettyTask::Expr(self))
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

enum PrettyTask<'a> {
    Expr(&'a Expression),
    Pattern(&'a BindingPattern),
    WriteStatic(&'static str),
    WriteOwned(String),
    RenderImplementation {
        expr: &'a Expression,
        max_len: usize,
    },
}

struct PrettyContext<'a> {
    tasks: Vec<PrettyTask<'a>>,
    output: String,
    truncate_limit: Option<usize>,
}

fn pretty_print_task(task: PrettyTask<'_>) -> String {
    fn literal_to_string(lit: &ExpressionLiteral) -> String {
        match lit {
            ExpressionLiteral::Number(n) => n.to_string(),
            ExpressionLiteral::Boolean(b) => b.to_string(),
            ExpressionLiteral::Char(value) => format!("'{}'", escape_literal_byte(*value, '\'')),
            ExpressionLiteral::String(bytes) => {
                let mut output = String::with_capacity(bytes.len() + 2);
                output.push('"');
                for byte in bytes {
                    output.push_str(&escape_literal_byte(*byte, '"'));
                }
                output.push('"');
                output
            }
            ExpressionLiteral::Target(t) => match t {
                TargetLiteral::JSTarget => "js".to_string(),
                TargetLiteral::WasmTarget => "wasm".to_string(),
            },
        }
    }

    let mut contexts: Vec<PrettyContext<'_>> = vec![PrettyContext {
        tasks: vec![task],
        output: String::new(),
        truncate_limit: None,
    }];

    while let Some(context) = contexts.last_mut() {
        let Some(task) = context.tasks.pop() else {
            let finished = contexts.pop().expect("context stack should not be empty");
            let mut output = finished.output;
            if let Some(limit) = finished.truncate_limit {
                let ellipses = "...";
                if output.len() > limit {
                    let keep = limit.saturating_sub(ellipses.len());
                    output.truncate(keep);
                    output.push_str(ellipses);
                }
            }
            if let Some(parent) = contexts.last_mut() {
                parent.output.push_str(&output);
                continue;
            }
            return output;
        };

        match task {
            PrettyTask::WriteStatic(text) => {
                context.output.push_str(text);
            }
            PrettyTask::WriteOwned(text) => {
                context.output.push_str(&text);
            }
            PrettyTask::RenderImplementation { expr, max_len } => {
                contexts.push(PrettyContext {
                    tasks: vec![PrettyTask::Expr(expr)],
                    output: String::new(),
                    truncate_limit: Some(max_len),
                });
            }
            PrettyTask::Expr(expr) => match &expr.kind {
                ExpressionKind::IntrinsicType(ty) => {
                    let ty_str = match ty {
                        IntrinsicType::I32 => "i32",
                        IntrinsicType::U8 => "u8",
                        IntrinsicType::Boolean => "boolean",
                        IntrinsicType::Type => "type",
                        IntrinsicType::Target => "target",
                    };
                    context.tasks.push(PrettyTask::WriteStatic(ty_str));
                }
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
                        context.tasks.push(PrettyTask::Expr(right));
                        context.tasks.push(PrettyTask::WriteStatic(" "));
                        context.tasks.push(PrettyTask::WriteStatic(op_str));
                        context.tasks.push(PrettyTask::WriteStatic(" "));
                        context.tasks.push(PrettyTask::Expr(left));
                    }
                    IntrinsicOperation::Unary(operand, op) => {
                        let op_str = match op {
                            UnaryIntrinsicOperator::BooleanNot => "!",
                            UnaryIntrinsicOperator::EnumFromStruct => "enum",
                            UnaryIntrinsicOperator::MatchFromStruct => "match",
                            UnaryIntrinsicOperator::UseFromString => "use",
                        };
                        context.tasks.push(PrettyTask::WriteStatic(")"));
                        context.tasks.push(PrettyTask::Expr(operand));
                        context.tasks.push(PrettyTask::WriteStatic("("));
                        context.tasks.push(PrettyTask::WriteStatic(op_str));
                    }
                },
                ExpressionKind::EnumType(variants) => {
                    context.tasks.push(PrettyTask::WriteStatic(" }"));
                    let mut first = true;
                    for (name, ty) in variants.iter().rev() {
                        if !first {
                            context.tasks.push(PrettyTask::WriteStatic(", "));
                        }
                        first = false;
                        context.tasks.push(PrettyTask::WriteStatic(")"));
                        context.tasks.push(PrettyTask::Expr(ty));
                        context.tasks.push(PrettyTask::WriteStatic("("));
                        context
                            .tasks
                            .push(PrettyTask::WriteOwned(name.name.clone()));
                    }
                    context.tasks.push(PrettyTask::WriteStatic("enum { "));
                }
                ExpressionKind::Match { value, branches } => {
                    context.tasks.push(PrettyTask::WriteStatic(")"));
                    let mut first = true;
                    for (pattern, body) in branches.iter().rev() {
                        if !first {
                            context.tasks.push(PrettyTask::WriteStatic(", "));
                        }
                        first = false;
                        context.tasks.push(PrettyTask::Expr(body));
                        context.tasks.push(PrettyTask::WriteStatic(" => "));
                        context.tasks.push(PrettyTask::Pattern(pattern));
                    }
                    context.tasks.push(PrettyTask::WriteStatic("("));
                    context.tasks.push(PrettyTask::WriteStatic(" with "));
                    context.tasks.push(PrettyTask::Expr(value));
                    context.tasks.push(PrettyTask::WriteStatic("match "));
                }
                ExpressionKind::EnumValue {
                    enum_type,
                    variant,
                    payload,
                    ..
                } => {
                    context.tasks.push(PrettyTask::WriteStatic(")"));
                    context.tasks.push(PrettyTask::Expr(payload));
                    context.tasks.push(PrettyTask::WriteStatic("("));
                    context
                        .tasks
                        .push(PrettyTask::WriteOwned(variant.name.clone()));
                    context.tasks.push(PrettyTask::WriteStatic("::"));
                    context.tasks.push(PrettyTask::Expr(enum_type));
                }
                ExpressionKind::EnumConstructor {
                    enum_type,
                    variant,
                    payload_type,
                    ..
                } => {
                    context.tasks.push(PrettyTask::WriteStatic(")"));
                    context.tasks.push(PrettyTask::Expr(payload_type));
                    context.tasks.push(PrettyTask::WriteStatic("("));
                    context
                        .tasks
                        .push(PrettyTask::WriteOwned(variant.name.clone()));
                    context.tasks.push(PrettyTask::WriteStatic("::"));
                    context.tasks.push(PrettyTask::Expr(enum_type));
                }
                ExpressionKind::EnumAccess { enum_expr, variant } => {
                    context
                        .tasks
                        .push(PrettyTask::WriteOwned(variant.name.clone()));
                    context.tasks.push(PrettyTask::WriteStatic("::"));
                    context.tasks.push(PrettyTask::Expr(enum_expr));
                }
                ExpressionKind::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    context.tasks.push(PrettyTask::Expr(else_branch));
                    context.tasks.push(PrettyTask::WriteStatic(" else "));
                    context.tasks.push(PrettyTask::Expr(then_branch));
                    context.tasks.push(PrettyTask::WriteStatic(" then "));
                    context.tasks.push(PrettyTask::Expr(condition));
                    context.tasks.push(PrettyTask::WriteStatic("if "));
                }
                ExpressionKind::AttachImplementation {
                    type_expr,
                    implementation,
                } => {
                    context.tasks.push(PrettyTask::RenderImplementation {
                        expr: implementation,
                        max_len: 30,
                    });
                    context.tasks.push(PrettyTask::WriteStatic(" @ "));
                    context.tasks.push(PrettyTask::Expr(type_expr));
                }
                ExpressionKind::Function {
                    parameter,
                    return_type,
                    body,
                } => {
                    context.tasks.push(PrettyTask::Expr(body));
                    context.tasks.push(PrettyTask::WriteStatic(") => "));
                    if let Some(return_type) = return_type.as_ref() {
                        context.tasks.push(PrettyTask::Expr(return_type));
                        context.tasks.push(PrettyTask::WriteStatic(" => "));
                    }
                    context.tasks.push(PrettyTask::Pattern(parameter));
                    context.tasks.push(PrettyTask::WriteStatic("("));
                }
                ExpressionKind::FunctionType {
                    parameter,
                    return_type,
                } => {
                    context.tasks.push(PrettyTask::Expr(return_type));
                    context.tasks.push(PrettyTask::WriteStatic(") => "));
                    context.tasks.push(PrettyTask::Expr(parameter));
                    context.tasks.push(PrettyTask::WriteStatic("("));
                }
                ExpressionKind::Struct(fields) => {
                    context.tasks.push(PrettyTask::WriteStatic(" }"));
                    let mut first = true;
                    for (name, value) in fields.iter().rev() {
                        if !first {
                            context.tasks.push(PrettyTask::WriteStatic(", "));
                        }
                        first = false;
                        context.tasks.push(PrettyTask::Expr(value));
                        context.tasks.push(PrettyTask::WriteStatic(" = "));
                        context
                            .tasks
                            .push(PrettyTask::WriteOwned(name.name.clone()));
                    }
                    context.tasks.push(PrettyTask::WriteStatic("{ "));
                }
                ExpressionKind::Literal(lit) => {
                    context
                        .tasks
                        .push(PrettyTask::WriteOwned(literal_to_string(lit)));
                }
                ExpressionKind::Identifier(id) => {
                    context.tasks.push(PrettyTask::WriteOwned(id.name.clone()));
                }
                ExpressionKind::Operation {
                    operator,
                    left,
                    right,
                } => {
                    context.tasks.push(PrettyTask::Expr(right));
                    context.tasks.push(PrettyTask::WriteStatic(" "));
                    context.tasks.push(PrettyTask::WriteOwned(operator.clone()));
                    context.tasks.push(PrettyTask::WriteStatic(" "));
                    context.tasks.push(PrettyTask::Expr(left));
                }
                ExpressionKind::Assignment { target, expr } => {
                    context.tasks.push(PrettyTask::Expr(expr));
                    context.tasks.push(PrettyTask::WriteStatic(" = "));
                    context
                        .tasks
                        .push(PrettyTask::WriteOwned(target.pretty_print()));
                }
                ExpressionKind::FunctionCall { function, argument } => {
                    context.tasks.push(PrettyTask::WriteStatic(")"));
                    context.tasks.push(PrettyTask::Expr(argument));
                    context.tasks.push(PrettyTask::WriteStatic("("));
                    context.tasks.push(PrettyTask::Expr(function));
                }
                ExpressionKind::ArrayIndex { array, index } => {
                    context.tasks.push(PrettyTask::WriteStatic(")"));
                    context.tasks.push(PrettyTask::Expr(index));
                    context.tasks.push(PrettyTask::WriteStatic("("));
                    context.tasks.push(PrettyTask::Expr(array));
                }
                ExpressionKind::PropertyAccess { object, property } => {
                    context.tasks.push(PrettyTask::WriteOwned(property.clone()));
                    context.tasks.push(PrettyTask::WriteStatic("."));
                    context.tasks.push(PrettyTask::Expr(object));
                }
                ExpressionKind::Binding(binding) => {
                    context.tasks.push(PrettyTask::Expr(&binding.expr));
                    context.tasks.push(PrettyTask::WriteStatic(" := "));
                    context.tasks.push(PrettyTask::Pattern(&binding.pattern));
                }
                ExpressionKind::Block(exprs) => {
                    context.tasks.push(PrettyTask::WriteStatic(" )"));
                    let mut first = true;
                    for expr in exprs.iter().rev() {
                        if !first {
                            context.tasks.push(PrettyTask::WriteStatic("; "));
                        }
                        first = false;
                        context.tasks.push(PrettyTask::Expr(expr));
                    }
                    context.tasks.push(PrettyTask::WriteStatic("( "));
                }
                ExpressionKind::Diverge {
                    value,
                    divergance_type,
                } => {
                    let keyword = match divergance_type {
                        DivergeExpressionType::Return => "return ",
                        DivergeExpressionType::Break => "break ",
                    };
                    context.tasks.push(PrettyTask::Expr(value));
                    context.tasks.push(PrettyTask::WriteStatic(keyword));
                }
                ExpressionKind::Loop { body } => {
                    context.tasks.push(PrettyTask::Expr(body));
                    context.tasks.push(PrettyTask::WriteStatic("loop "));
                }
            },
            PrettyTask::Pattern(pattern) => match pattern {
                BindingPattern::Identifier(id, _) => {
                    context.tasks.push(PrettyTask::WriteOwned(id.name.clone()));
                }
                BindingPattern::Literal(lit, _) => {
                    context
                        .tasks
                        .push(PrettyTask::WriteOwned(literal_to_string(lit)));
                }
                BindingPattern::Struct(fields, _) => {
                    context.tasks.push(PrettyTask::WriteStatic(" }"));
                    let mut first = true;
                    for (name, pat) in fields.iter().rev() {
                        if !first {
                            context.tasks.push(PrettyTask::WriteStatic(", "));
                        }
                        first = false;
                        context.tasks.push(PrettyTask::Pattern(pat));
                        context.tasks.push(PrettyTask::WriteStatic(" = "));
                        context
                            .tasks
                            .push(PrettyTask::WriteOwned(name.name.clone()));
                    }
                    context.tasks.push(PrettyTask::WriteStatic("{ "));
                }
                BindingPattern::EnumVariant {
                    enum_type,
                    variant,
                    payload,
                    ..
                } => {
                    if let Some(payload) = payload.as_ref() {
                        context.tasks.push(PrettyTask::WriteStatic(")"));
                        context.tasks.push(PrettyTask::Pattern(payload));
                        context.tasks.push(PrettyTask::WriteStatic("("));
                    }
                    context
                        .tasks
                        .push(PrettyTask::WriteOwned(variant.name.clone()));
                    context.tasks.push(PrettyTask::WriteStatic("::"));
                    context.tasks.push(PrettyTask::Expr(enum_type));
                }
                BindingPattern::TypeHint(inner, type_expr, _) => {
                    context.tasks.push(PrettyTask::Expr(type_expr));
                    context.tasks.push(PrettyTask::WriteStatic(": "));
                    context.tasks.push(PrettyTask::Pattern(inner));
                }
                BindingPattern::Annotated {
                    annotations,
                    pattern,
                    ..
                } => {
                    context.tasks.push(PrettyTask::Pattern(pattern));
                    for annotation in annotations.iter().rev() {
                        context.tasks.push(PrettyTask::WriteStatic(") "));
                        match annotation {
                            BindingAnnotation::Export(expr, _) => {
                                context.tasks.push(PrettyTask::Expr(expr));
                                context.tasks.push(PrettyTask::WriteStatic("export "));
                            }
                            BindingAnnotation::Mutable(_) => {
                                context.tasks.push(PrettyTask::WriteStatic("mut"));
                            }
                        }
                        context.tasks.push(PrettyTask::WriteStatic("("));
                    }
                }
            },
        }
    }

    String::new()
}

const GROUP_TERMINATORS: [char; 1] = [')'];

fn is_keyword_start(file: &str, keyword: &str) -> bool {
    if !file.starts_with(keyword) {
        return false;
    }
    let after_keyword = &file[keyword.len()..];
    !after_keyword
        .chars()
        .next()
        .map(|c| c.is_alphanumeric() || c == '_')
        .unwrap_or(false)
}

enum Frame<'a> {
    Expr(ExprFrame),
    Block(BlockFrame<'a>),
    Grouping(GroupingFrame),
    Struct(StructFrame<'a>),
    If(IfFrame<'a>),
    For(ForFrame<'a>),
    Loop(LoopFrame<'a>),
    While(WhileFrame<'a>),
    Return(ReturnFrame<'a>),
    Break(BreakFrame<'a>),
}

enum ExprState {
    ExpectOperand,
    ExpectOperator,
}

struct ExprFrame {
    operand_stack: Vec<Expression>,
    operator_stack: Vec<String>,
    min_precedence: u8,
    state: ExprState,
    stop_at_grouping: bool,
}

impl ExprFrame {
    fn new(min_precedence: u8) -> Self {
        Self::new_with_stop(min_precedence, false)
    }

    fn new_with_stop(min_precedence: u8, stop_at_grouping: bool) -> Self {
        Self {
            operand_stack: Vec::new(),
            operator_stack: Vec::new(),
            min_precedence,
            state: ExprState::ExpectOperand,
            stop_at_grouping,
        }
    }
}

enum BlockState {
    ExpectExpr,
    AfterExpr,
}

struct BlockFrame<'a> {
    terminators: &'a [char],
    expressions: Vec<Expression>,
    ended_with_semicolon: bool,
    state: BlockState,
}

impl<'a> BlockFrame<'a> {
    fn new(terminators: &'a [char]) -> Self {
        Self {
            terminators,
            expressions: Vec::new(),
            ended_with_semicolon: false,
            state: BlockState::ExpectExpr,
        }
    }
}

#[derive(PartialEq, Eq)]
enum GroupingState {
    Start,
    ExpectClose,
}

struct GroupingFrame {
    state: GroupingState,
}

impl GroupingFrame {
    fn new() -> Self {
        Self {
            state: GroupingState::Start,
        }
    }
}

enum StructState {
    Start,
    ExpectField,
    ExpectCommaOrEnd,
}

struct StructFrame<'a> {
    start_slice: &'a str,
    items: Vec<(Identifier, Expression)>,
    tuple_index: usize,
    pending_field: Option<Identifier>,
    state: StructState,
}

impl<'a> StructFrame<'a> {
    fn new(start_slice: &'a str) -> Self {
        Self {
            start_slice,
            items: Vec::new(),
            tuple_index: 0,
            pending_field: None,
            state: StructState::Start,
        }
    }
}

enum IfState {
    Start,
    Condition,
    ExpectThen,
    ThenBranch,
    MaybeElse,
    ElseBranch,
}

struct IfFrame<'a> {
    start_slice: &'a str,
    state: IfState,
    condition: Option<Expression>,
    then_branch: Option<Expression>,
}

impl<'a> IfFrame<'a> {
    fn new(start_slice: &'a str) -> Self {
        Self {
            start_slice,
            state: IfState::Start,
            condition: None,
            then_branch: None,
        }
    }
}

enum ForState {
    Start,
    Pattern,
    ExpectIn,
    Iterator,
    ExpectBody,
    Body,
}

struct ForFrame<'a> {
    start_slice: &'a str,
    state: ForState,
    pattern_expr: Option<Expression>,
    iterator_expr: Option<Expression>,
}

impl<'a> ForFrame<'a> {
    fn new(start_slice: &'a str) -> Self {
        Self {
            start_slice,
            state: ForState::Start,
            pattern_expr: None,
            iterator_expr: None,
        }
    }
}

enum LoopState {
    Start,
    Body,
}

struct LoopFrame<'a> {
    start_slice: &'a str,
    state: LoopState,
}

impl<'a> LoopFrame<'a> {
    fn new(start_slice: &'a str) -> Self {
        Self {
            start_slice,
            state: LoopState::Start,
        }
    }
}

enum WhileState {
    Start,
    Condition,
    ExpectDo,
    Body,
}

struct WhileFrame<'a> {
    start_slice: &'a str,
    state: WhileState,
    condition: Option<Expression>,
}

impl<'a> WhileFrame<'a> {
    fn new(start_slice: &'a str) -> Self {
        Self {
            start_slice,
            state: WhileState::Start,
            condition: None,
        }
    }
}

enum DivergeState {
    Start,
    AwaitValue,
}

struct ReturnFrame<'a> {
    start_slice: &'a str,
    state: DivergeState,
}

impl<'a> ReturnFrame<'a> {
    fn new(start_slice: &'a str) -> Self {
        Self {
            start_slice,
            state: DivergeState::Start,
        }
    }
}

struct BreakFrame<'a> {
    start_slice: &'a str,
    state: DivergeState,
}

impl<'a> BreakFrame<'a> {
    fn new(start_slice: &'a str) -> Self {
        Self {
            start_slice,
            state: DivergeState::Start,
        }
    }
}

struct Parser<'a> {
    source: &'a str,
    remaining: &'a str,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str, file: &'a str) -> Self {
        Self {
            source,
            remaining: file,
        }
    }

    #[cfg(test)]
    fn parse_expression_with_min_precedence(
        &mut self,
        min_precedence: u8,
    ) -> Result<Expression, Diagnostic> {
        self.parse_with_frame(Frame::Expr(ExprFrame::new(min_precedence)))
    }

    fn parse_block_with_terminators(
        &mut self,
        terminators: &'a [char],
    ) -> Result<Expression, Diagnostic> {
        self.parse_with_frame(Frame::Block(BlockFrame::new(terminators)))
    }

    fn parse_with_frame(&mut self, frame: Frame<'a>) -> Result<Expression, Diagnostic> {
        let mut frames = vec![frame];
        let mut completed_expr: Option<Expression> = None;

        'parse: loop {
            if frames.is_empty() {
                return completed_expr.ok_or_else(|| {
                    diagnostic_at_eof(self.source, "Expected expression to start parsing")
                });
            }

            let frame = frames.pop().expect("frame stack should not be empty");
            match frame {
                Frame::Expr(mut expr_frame) => {
                    if let Some(expr) = completed_expr.take() {
                        expr_frame.operand_stack.push(expr);
                        expr_frame.state = ExprState::ExpectOperator;
                    }

                    loop {
                        match expr_frame.state {
                            ExprState::ExpectOperand => {
                                self.remaining = parse_optional_whitespace(self.remaining);
                                if self.remaining.is_empty() {
                                    let len = self
                                        .remaining
                                        .chars()
                                        .next()
                                        .map(|c| c.len_utf8())
                                        .unwrap_or(1);
                                    return Err(diagnostic_here(
                                        self.source,
                                        self.remaining,
                                        len,
                                        "Expected expression",
                                    ));
                                }

                                if is_keyword_start(self.remaining, "return") {
                                    frames.push(Frame::Expr(expr_frame));
                                    frames.push(Frame::Return(ReturnFrame::new(self.remaining)));
                                    continue 'parse;
                                }
                                if is_keyword_start(self.remaining, "break") {
                                    frames.push(Frame::Expr(expr_frame));
                                    frames.push(Frame::Break(BreakFrame::new(self.remaining)));
                                    continue 'parse;
                                }
                                if is_keyword_start(self.remaining, "for") {
                                    frames.push(Frame::Expr(expr_frame));
                                    frames.push(Frame::For(ForFrame::new(self.remaining)));
                                    continue 'parse;
                                }
                                if is_keyword_start(self.remaining, "while") {
                                    frames.push(Frame::Expr(expr_frame));
                                    frames.push(Frame::While(WhileFrame::new(self.remaining)));
                                    continue 'parse;
                                }
                                if is_keyword_start(self.remaining, "loop") {
                                    frames.push(Frame::Expr(expr_frame));
                                    frames.push(Frame::Loop(LoopFrame::new(self.remaining)));
                                    continue 'parse;
                                }
                                if is_keyword_start(self.remaining, "if") {
                                    frames.push(Frame::Expr(expr_frame));
                                    frames.push(Frame::If(IfFrame::new(self.remaining)));
                                    continue 'parse;
                                }
                                if self.remaining.starts_with('(') {
                                    frames.push(Frame::Expr(expr_frame));
                                    frames.push(Frame::Grouping(GroupingFrame::new()));
                                    continue 'parse;
                                }
                                if self.remaining.starts_with('{') {
                                    frames.push(Frame::Expr(expr_frame));
                                    frames.push(Frame::Struct(StructFrame::new(self.remaining)));
                                    continue 'parse;
                                }

                                let start_slice = self.remaining;
                                if let Some((identifier, rest)) = parse_identifier(self.remaining) {
                                    let span = consumed_span(self.source, start_slice, rest);
                                    self.remaining = rest;
                                    expr_frame.operand_stack.push(
                                        ExpressionKind::Identifier(identifier).with_span(span),
                                    );
                                    expr_frame.state = ExprState::ExpectOperator;
                                    continue;
                                }
                                if let Some((literal, rest)) = parse_literal(self.remaining) {
                                    let span = consumed_span(self.source, start_slice, rest);
                                    self.remaining = rest;
                                    expr_frame
                                        .operand_stack
                                        .push(ExpressionKind::Literal(literal).with_span(span));
                                    expr_frame.state = ExprState::ExpectOperator;
                                    continue;
                                }

                                let len = self
                                    .remaining
                                    .chars()
                                    .next()
                                    .map(|c| c.len_utf8())
                                    .unwrap_or(1);
                                return Err(diagnostic_here(
                                    self.source,
                                    self.remaining,
                                    len,
                                    "Expected expression",
                                ));
                            }
                            ExprState::ExpectOperator => {
                                let before_ws = self.remaining;
                                self.remaining = parse_optional_whitespace(self.remaining);
                                let skipped_whitespace = before_ws.len() != self.remaining.len();
                                if expr_frame.stop_at_grouping
                                    && skipped_whitespace
                                    && self.remaining.starts_with('(')
                                {
                                    while !expr_frame.operator_stack.is_empty() {
                                        reduce_stacks(
                                            &mut expr_frame.operand_stack,
                                            &mut expr_frame.operator_stack,
                                            self.source,
                                        )?;
                                    }
                                    let expr = expr_frame.operand_stack.pop().ok_or_else(|| {
                                        diagnostic_at_eof(
                                            self.source,
                                            "Expected expression after parsing operations",
                                        )
                                    })?;
                                    completed_expr = Some(expr);
                                    continue 'parse;
                                }
                                if let Some((operator, rest)) = parse_operator(self.remaining) {
                                    if operator_precedence(&operator) < expr_frame.min_precedence {
                                        while !expr_frame.operator_stack.is_empty() {
                                            reduce_stacks(
                                                &mut expr_frame.operand_stack,
                                                &mut expr_frame.operator_stack,
                                                self.source,
                                            )?;
                                        }
                                        let expr =
                                            expr_frame.operand_stack.pop().ok_or_else(|| {
                                                diagnostic_at_eof(
                                                    self.source,
                                                    "Expected expression after parsing operations",
                                                )
                                            })?;
                                        completed_expr = Some(expr);
                                        continue 'parse;
                                    }

                                    while expr_frame.operator_stack.last().is_some_and(|existing| {
                                        operator_precedence(existing)
                                            >= operator_precedence(&operator)
                                    }) {
                                        reduce_stacks(
                                            &mut expr_frame.operand_stack,
                                            &mut expr_frame.operator_stack,
                                            self.source,
                                        )?;
                                    }

                                    expr_frame.operator_stack.push(operator);
                                    self.remaining = rest;
                                    expr_frame.state = ExprState::ExpectOperand;
                                    continue;
                                }

                                while !expr_frame.operator_stack.is_empty() {
                                    reduce_stacks(
                                        &mut expr_frame.operand_stack,
                                        &mut expr_frame.operator_stack,
                                        self.source,
                                    )?;
                                }
                                let expr = expr_frame.operand_stack.pop().ok_or_else(|| {
                                    diagnostic_at_eof(
                                        self.source,
                                        "Expected expression after parsing operations",
                                    )
                                })?;
                                completed_expr = Some(expr);
                                continue 'parse;
                            }
                        }
                    }
                }
                Frame::Block(mut block_frame) => {
                    if let Some(expr) = completed_expr.take() {
                        block_frame.expressions.push(expr);
                        block_frame.ended_with_semicolon = false;
                        block_frame.state = BlockState::AfterExpr;
                    }

                    loop {
                        match block_frame.state {
                            BlockState::ExpectExpr => {
                                self.remaining = parse_optional_whitespace(self.remaining);
                                if self.remaining.is_empty()
                                    || self
                                        .remaining
                                        .chars()
                                        .next()
                                        .is_some_and(|ch| block_frame.terminators.contains(&ch))
                                {
                                    if block_frame.expressions.is_empty() {
                                        let len = self
                                            .remaining
                                            .chars()
                                            .next()
                                            .map(|c| c.len_utf8())
                                            .unwrap_or(0);
                                        return Err(diagnostic_here(
                                            self.source,
                                            self.remaining,
                                            len,
                                            "Cannot parse empty block",
                                        ));
                                    }
                                    let expr = finish_block(block_frame);
                                    completed_expr = Some(expr);
                                    continue 'parse;
                                }

                                frames.push(Frame::Block(block_frame));
                                frames.push(Frame::Expr(ExprFrame::new(0)));
                                continue 'parse;
                            }
                            BlockState::AfterExpr => {
                                self.remaining = parse_optional_whitespace(self.remaining);
                                if self.remaining.is_empty()
                                    || self
                                        .remaining
                                        .chars()
                                        .next()
                                        .is_some_and(|ch| block_frame.terminators.contains(&ch))
                                {
                                    let expr = finish_block(block_frame);
                                    completed_expr = Some(expr);
                                    continue 'parse;
                                }

                                let rest = parse_semicolon(self.source, self.remaining)?;
                                self.remaining = parse_optional_whitespace(rest);
                                block_frame.ended_with_semicolon = true;
                                block_frame.state = BlockState::ExpectExpr;
                                continue;
                            }
                        }
                    }
                }
                Frame::Grouping(mut grouping_frame) => {
                    if let Some(expr) = completed_expr.take() {
                        if grouping_frame.state != GroupingState::ExpectClose {
                            return Err(diagnostic_at_eof(
                                self.source,
                                "Expected ) to close grouping expression",
                            ));
                        }
                        let Some(after) = self.remaining.strip_prefix(')') else {
                            return Err(diagnostic_here(
                                self.source,
                                self.remaining,
                                1,
                                "Expected ) to close grouping expression",
                            ));
                        };
                        self.remaining = after;
                        completed_expr = Some(expr);
                        continue 'parse;
                    }

                    match grouping_frame.state {
                        GroupingState::Start => {
                            let Some(after) = self.remaining.strip_prefix('(') else {
                                return Err(diagnostic_here(
                                    self.source,
                                    self.remaining,
                                    1,
                                    "Expected ( to start grouping expression",
                                ));
                            };
                            self.remaining = after;
                            grouping_frame.state = GroupingState::ExpectClose;
                            frames.push(Frame::Grouping(grouping_frame));
                            frames.push(Frame::Block(BlockFrame::new(&GROUP_TERMINATORS)));
                            continue 'parse;
                        }
                        GroupingState::ExpectClose => {
                            return Err(diagnostic_here(
                                self.source,
                                self.remaining,
                                1,
                                "Expected ) to close grouping expression",
                            ));
                        }
                    }
                }
                Frame::Struct(mut struct_frame) => {
                    if let Some(expr) = completed_expr.take() {
                        let Some(identifier) = struct_frame.pending_field.take() else {
                            return Err(diagnostic_here(
                                self.source,
                                self.remaining,
                                1,
                                "Expected struct field value",
                            ));
                        };
                        struct_frame.items.push((identifier, expr));
                        struct_frame.state = StructState::ExpectCommaOrEnd;
                    }

                    loop {
                        match struct_frame.state {
                            StructState::Start => {
                                let Some(after) = self.remaining.strip_prefix('{') else {
                                    return Err(diagnostic_here(
                                        self.source,
                                        self.remaining,
                                        1,
                                        "Expected { to start struct literal",
                                    ));
                                };
                                self.remaining = after;
                                struct_frame.state = StructState::ExpectField;
                            }
                            StructState::ExpectField => {
                                self.remaining = parse_optional_whitespace(self.remaining);
                                if let Some(rest) = self.remaining.strip_prefix('}') {
                                    self.remaining = rest;
                                    let span =
                                        consumed_span(self.source, struct_frame.start_slice, rest);
                                    completed_expr = Some(
                                        ExpressionKind::Struct(struct_frame.items).with_span(span),
                                    );
                                    continue 'parse;
                                }

                                let mut has_named_field = false;
                                if let Some((identifier, after_identifier)) =
                                    parse_identifier(self.remaining)
                                {
                                    let after_ws = parse_optional_whitespace(after_identifier);
                                    if let Some((operator, after_equals)) = parse_operator(after_ws)
                                    {
                                        if operator == "=" {
                                            self.remaining =
                                                parse_optional_whitespace(after_equals);
                                            struct_frame.pending_field = Some(identifier);
                                            has_named_field = true;
                                        }
                                    }
                                }

                                if !has_named_field {
                                    let tuple_identifier =
                                        Identifier::new(struct_frame.tuple_index.to_string());
                                    struct_frame.tuple_index += 1;
                                    struct_frame.pending_field = Some(tuple_identifier);
                                }

                                frames.push(Frame::Struct(struct_frame));
                                frames.push(Frame::Expr(ExprFrame::new(0)));
                                continue 'parse;
                            }
                            StructState::ExpectCommaOrEnd => {
                                self.remaining = parse_optional_whitespace(self.remaining);
                                if let Some(rest) = self.remaining.strip_prefix('}') {
                                    self.remaining = rest;
                                    let span =
                                        consumed_span(self.source, struct_frame.start_slice, rest);
                                    completed_expr = Some(
                                        ExpressionKind::Struct(struct_frame.items).with_span(span),
                                    );
                                    continue 'parse;
                                }

                                let Some(rest) = self.remaining.strip_prefix(',') else {
                                    return Err(diagnostic_here(
                                        self.source,
                                        self.remaining,
                                        1,
                                        "Expected , or } in struct literal",
                                    ));
                                };
                                self.remaining = rest;
                                struct_frame.state = StructState::ExpectField;
                                continue;
                            }
                        }
                    }
                }
                Frame::If(mut if_frame) => {
                    if let Some(expr) = completed_expr.take() {
                        match if_frame.state {
                            IfState::Condition => {
                                if_frame.condition = Some(expr);
                                if_frame.state = IfState::ExpectThen;
                            }
                            IfState::ThenBranch => {
                                if_frame.then_branch = Some(expr);
                                if_frame.state = IfState::MaybeElse;
                            }
                            IfState::ElseBranch => {
                                let condition = if_frame.condition.take().ok_or_else(|| {
                                    diagnostic_at_eof(
                                        self.source,
                                        "Expected if condition while parsing",
                                    )
                                })?;
                                let then_branch = if_frame.then_branch.take().ok_or_else(|| {
                                    diagnostic_at_eof(
                                        self.source,
                                        "Expected then branch while parsing",
                                    )
                                })?;
                                let span = consumed_span(
                                    self.source,
                                    if_frame.start_slice,
                                    self.remaining,
                                );
                                completed_expr = Some(
                                    ExpressionKind::If {
                                        condition: Box::new(condition),
                                        then_branch: Box::new(then_branch),
                                        else_branch: Box::new(expr),
                                    }
                                    .with_span(span),
                                );
                                continue 'parse;
                            }
                            _ => {
                                return Err(diagnostic_at_eof(
                                    self.source,
                                    "Unexpected expression in if parsing",
                                ));
                            }
                        }
                    }

                    match if_frame.state {
                        IfState::Start => {
                            if !is_keyword_start(self.remaining, "if") {
                                return Err(diagnostic_here(
                                    self.source,
                                    self.remaining,
                                    2,
                                    "Expected if",
                                ));
                            }
                            let after_keyword = &self.remaining[2..];
                            self.remaining = parse_optional_whitespace(after_keyword);
                            if_frame.state = IfState::Condition;
                            frames.push(Frame::If(if_frame));
                            frames.push(Frame::Expr(ExprFrame::new(0)));
                            continue 'parse;
                        }
                        IfState::ExpectThen => {
                            self.remaining = parse_optional_whitespace(self.remaining);
                            let Some(after_then) = self.remaining.strip_prefix("then") else {
                                return Err(diagnostic_here(
                                    self.source,
                                    self.remaining,
                                    4,
                                    "Expected then after if condition",
                                ));
                            };
                            self.remaining = parse_optional_whitespace(after_then);
                            if_frame.state = IfState::ThenBranch;
                            frames.push(Frame::If(if_frame));
                            frames.push(Frame::Expr(ExprFrame::new(0)));
                            continue 'parse;
                        }
                        IfState::MaybeElse => {
                            self.remaining = parse_optional_whitespace(self.remaining);
                            if let Some(rest) = self.remaining.strip_prefix("else") {
                                self.remaining = parse_optional_whitespace(rest);
                                if_frame.state = IfState::ElseBranch;
                                frames.push(Frame::If(if_frame));
                                frames.push(Frame::Expr(ExprFrame::new(0)));
                                continue 'parse;
                            }

                            let condition = if_frame.condition.take().ok_or_else(|| {
                                diagnostic_at_eof(
                                    self.source,
                                    "Expected if condition while parsing",
                                )
                            })?;
                            let then_branch = if_frame.then_branch.take().ok_or_else(|| {
                                diagnostic_at_eof(self.source, "Expected then branch while parsing")
                            })?;
                            let span =
                                consumed_span(self.source, if_frame.start_slice, self.remaining);
                            let empty_else = ExpressionKind::Struct(vec![])
                                .with_span(SourceSpan::new(span.end(), 0));
                            completed_expr = Some(
                                ExpressionKind::If {
                                    condition: Box::new(condition),
                                    then_branch: Box::new(then_branch),
                                    else_branch: Box::new(empty_else),
                                }
                                .with_span(span),
                            );
                            continue 'parse;
                        }
                        _ => {
                            return Err(diagnostic_at_eof(
                                self.source,
                                "Unexpected if parsing state",
                            ));
                        }
                    }
                }
                Frame::For(mut for_frame) => {
                    if let Some(expr) = completed_expr.take() {
                        match for_frame.state {
                            ForState::Pattern => {
                                for_frame.pattern_expr = Some(expr);
                                for_frame.state = ForState::ExpectIn;
                            }
                            ForState::Iterator => {
                                for_frame.iterator_expr = Some(expr);
                                for_frame.state = ForState::ExpectBody;
                            }
                            ForState::Body => {
                                let pattern_expr = for_frame.pattern_expr.take().ok_or_else(|| {
                                    diagnostic_at_eof(
                                        self.source,
                                        "Expected for pattern while parsing",
                                    )
                                })?;
                                let iterator_expr = for_frame
                                    .iterator_expr
                                    .take()
                                    .ok_or_else(|| {
                                        diagnostic_at_eof(
                                            self.source,
                                            "Expected for iterator while parsing",
                                        )
                                    })?;
                                let element_pattern =
                                    pattern_expression_to_binding_pattern(pattern_expr.clone())?;
                                let iterator_identifier = match &iterator_expr.kind {
                                    ExpressionKind::Identifier(identifier) => identifier.clone(),
                                    _ => Identifier::new("__for_iter"),
                                };
                                let iterator_span = iterator_expr.span();
                                let iter_binding_pattern = BindingPattern::Annotated {
                                    annotations: vec![BindingAnnotation::Mutable(iterator_span)],
                                    pattern: Box::new(BindingPattern::Identifier(
                                        iterator_identifier.clone(),
                                        iterator_span,
                                    )),
                                    span: iterator_span,
                                };
                                let iter_binding_expr = ExpressionKind::Binding(Box::new(Binding {
                                    pattern: iter_binding_pattern,
                                    expr: iterator_expr,
                                }))
                                .with_span(iterator_span);
                                let overall_span = consumed_span(
                                    self.source,
                                    for_frame.start_slice,
                                    self.remaining,
                                );
                                let iter_identifier_expr = ExpressionKind::Identifier(
                                    iterator_identifier.clone(),
                                )
                                .with_span(overall_span);
                                let iter_ty_access = ExpressionKind::PropertyAccess {
                                    object: Box::new(iter_identifier_expr.clone()),
                                    property: "iter_ty".to_string(),
                                }
                                .with_span(overall_span);
                                let option_call = ExpressionKind::FunctionCall {
                                    function: Box::new(
                                        ExpressionKind::Identifier(Identifier::new("Option"))
                                            .with_span(overall_span),
                                    ),
                                    argument: Box::new(iter_ty_access),
                                }
                                .with_span(overall_span);
                                let condition_pattern = BindingPattern::EnumVariant {
                                    enum_type: Box::new(option_call),
                                    variant: Identifier::new("Some"),
                                    payload: Some(Box::new(element_pattern)),
                                    span: overall_span,
                                };
                                let next_access = ExpressionKind::PropertyAccess {
                                    object: Box::new(iter_identifier_expr),
                                    property: "next".to_string(),
                                }
                                .with_span(overall_span);
                                let condition_expr = ExpressionKind::Binding(Box::new(Binding {
                                    pattern: condition_pattern,
                                    expr: next_access,
                                }))
                                .with_span(overall_span);
                                let while_expr =
                                    build_while_expression(condition_expr, expr, overall_span);
                                completed_expr = Some(
                                    ExpressionKind::Block(vec![iter_binding_expr, while_expr])
                                        .with_span(overall_span),
                                );
                                continue 'parse;
                            }
                            _ => {
                                return Err(diagnostic_at_eof(
                                    self.source,
                                    "Unexpected expression in for parsing",
                                ));
                            }
                        }
                    }

                    match for_frame.state {
                        ForState::Start => {
                            if !is_keyword_start(self.remaining, "for") {
                                return Err(diagnostic_here(
                                    self.source,
                                    self.remaining,
                                    3,
                                    "Expected for",
                                ));
                            }
                            let after_keyword = &self.remaining[3..];
                            self.remaining = parse_optional_whitespace(after_keyword);
                            for_frame.state = ForState::Pattern;
                            frames.push(Frame::For(for_frame));
                            frames.push(Frame::Expr(ExprFrame::new(0)));
                            continue 'parse;
                        }
                        ForState::ExpectIn => {
                            self.remaining = parse_optional_whitespace(self.remaining);
                            let Some(after_in) = self.remaining.strip_prefix("in") else {
                                return Err(diagnostic_here(
                                    self.source,
                                    self.remaining,
                                    2,
                                    "Expected in after for pattern",
                                ));
                            };
                            self.remaining = parse_optional_whitespace(after_in);
                            for_frame.state = ForState::Iterator;
                            frames.push(Frame::For(for_frame));
                            frames.push(Frame::Expr(ExprFrame::new_with_stop(0, true)));
                            continue 'parse;
                        }
                        ForState::ExpectBody => {
                            self.remaining = parse_optional_whitespace(self.remaining);
                            let Some(after_do) = self.remaining.strip_prefix("do") else {
                                return Err(diagnostic_here(
                                    self.source,
                                    self.remaining,
                                    2,
                                    "Expected do before for body",
                                ));
                            };
                            self.remaining = parse_optional_whitespace(after_do);
                            if !self.remaining.starts_with('(') {
                                return Err(diagnostic_here(
                                    self.source,
                                    self.remaining,
                                    1,
                                    "Expected for body expression",
                                ));
                            }
                            for_frame.state = ForState::Body;
                            frames.push(Frame::For(for_frame));
                            frames.push(Frame::Grouping(GroupingFrame::new()));
                            continue 'parse;
                        }
                        ForState::Pattern => {
                            return Err(diagnostic_at_eof(
                                self.source,
                                "Expected for pattern expression",
                            ));
                        }
                        ForState::Iterator => {
                            return Err(diagnostic_at_eof(
                                self.source,
                                "Expected for iterator expression",
                            ));
                        }
                        ForState::Body => {
                            return Err(diagnostic_at_eof(
                                self.source,
                                "Expected for body expression",
                            ));
                        }
                    }
                }
                Frame::Loop(mut loop_frame) => {
                    if let Some(expr) = completed_expr.take() {
                        let span =
                            consumed_span(self.source, loop_frame.start_slice, self.remaining);
                        completed_expr = Some(
                            ExpressionKind::Loop {
                                body: Box::new(expr),
                            }
                            .with_span(span),
                        );
                        continue 'parse;
                    }

                    match loop_frame.state {
                        LoopState::Start => {
                            if !is_keyword_start(self.remaining, "loop") {
                                return Err(diagnostic_here(
                                    self.source,
                                    self.remaining,
                                    4,
                                    "Expected loop",
                                ));
                            }
                            let after_keyword = &self.remaining[4..];
                            self.remaining = parse_optional_whitespace(after_keyword);
                            if !self.remaining.starts_with('(') {
                                return Err(diagnostic_here(
                                    self.source,
                                    self.remaining,
                                    1,
                                    "Expected loop body expression",
                                ));
                            }
                            loop_frame.state = LoopState::Body;
                            frames.push(Frame::Loop(loop_frame));
                            frames.push(Frame::Grouping(GroupingFrame::new()));
                            continue 'parse;
                        }
                        LoopState::Body => {
                            return Err(diagnostic_at_eof(
                                self.source,
                                "Expected loop body expression",
                            ));
                        }
                    }
                }
                Frame::While(mut while_frame) => {
                    if let Some(expr) = completed_expr.take() {
                        match while_frame.state {
                            WhileState::Condition => {
                                while_frame.condition = Some(expr);
                                while_frame.state = WhileState::ExpectDo;
                            }
                            WhileState::Body => {
                                let condition = while_frame.condition.take().ok_or_else(|| {
                                    diagnostic_at_eof(
                                        self.source,
                                        "Expected while condition while parsing",
                                    )
                                })?;
                                let span = consumed_span(
                                    self.source,
                                    while_frame.start_slice,
                                    self.remaining,
                                );
                                completed_expr = Some(build_while_expression(condition, expr, span));
                                continue 'parse;
                            }
                            _ => {
                                return Err(diagnostic_at_eof(
                                    self.source,
                                    "Unexpected expression in while parsing",
                                ));
                            }
                        }
                    }

                    match while_frame.state {
                        WhileState::Start => {
                            if !is_keyword_start(self.remaining, "while") {
                                return Err(diagnostic_here(
                                    self.source,
                                    self.remaining,
                                    5,
                                    "Expected while",
                                ));
                            }
                            let after_keyword = &self.remaining[5..];
                            self.remaining = parse_optional_whitespace(after_keyword);
                            while_frame.state = WhileState::Condition;
                            frames.push(Frame::While(while_frame));
                            frames.push(Frame::Expr(ExprFrame::new(0)));
                            continue 'parse;
                        }
                        WhileState::ExpectDo => {
                            self.remaining = parse_optional_whitespace(self.remaining);
                            let Some(after_do) = self.remaining.strip_prefix("do") else {
                                return Err(diagnostic_here(
                                    self.source,
                                    self.remaining,
                                    2,
                                    "Expected do after while condition",
                                ));
                            };
                            self.remaining = parse_optional_whitespace(after_do);
                            if !self.remaining.starts_with('(') {
                                return Err(diagnostic_here(
                                    self.source,
                                    self.remaining,
                                    1,
                                    "Expected while body expression",
                                ));
                            }
                            while_frame.state = WhileState::Body;
                            frames.push(Frame::While(while_frame));
                            frames.push(Frame::Grouping(GroupingFrame::new()));
                            continue 'parse;
                        }
                        WhileState::Body => {
                            return Err(diagnostic_at_eof(
                                self.source,
                                "Expected while body expression",
                            ));
                        }
                        WhileState::Condition => {
                            return Err(diagnostic_at_eof(
                                self.source,
                                "Expected while condition expression",
                            ));
                        }
                    }
                }
                Frame::Return(mut return_frame) => {
                    if let Some(expr) = completed_expr.take() {
                        let span =
                            consumed_span(self.source, return_frame.start_slice, self.remaining);
                        completed_expr = Some(
                            ExpressionKind::Diverge {
                                value: Box::new(expr),
                                divergance_type: DivergeExpressionType::Return,
                            }
                            .with_span(span),
                        );
                        continue 'parse;
                    }

                    match return_frame.state {
                        DivergeState::Start => {
                            if !is_keyword_start(self.remaining, "return") {
                                return Err(diagnostic_here(
                                    self.source,
                                    self.remaining,
                                    6,
                                    "Expected return",
                                ));
                            }
                            let after_keyword = &self.remaining[6..];
                            self.remaining = parse_optional_whitespace(after_keyword);
                            let value_start = self.remaining.chars().next();
                            if self.remaining.is_empty()
                                || matches!(
                                    value_start,
                                    Some(';') | Some(')') | Some(']') | Some('}') | Some(',')
                                )
                            {
                                let span = consumed_span(
                                    self.source,
                                    return_frame.start_slice,
                                    self.remaining,
                                );
                                let empty = Expression::new(ExpressionKind::Struct(vec![]), span);
                                completed_expr = Some(
                                    ExpressionKind::Diverge {
                                        value: Box::new(empty),
                                        divergance_type: DivergeExpressionType::Return,
                                    }
                                    .with_span(span),
                                );
                                continue 'parse;
                            }
                            return_frame.state = DivergeState::AwaitValue;
                            frames.push(Frame::Return(return_frame));
                            frames.push(Frame::Expr(ExprFrame::new(0)));
                            continue 'parse;
                        }
                        DivergeState::AwaitValue => {
                            return Err(diagnostic_at_eof(
                                self.source,
                                "Expected return expression",
                            ));
                        }
                    }
                }
                Frame::Break(mut break_frame) => {
                    if let Some(expr) = completed_expr.take() {
                        let span =
                            consumed_span(self.source, break_frame.start_slice, self.remaining);
                        completed_expr = Some(
                            ExpressionKind::Diverge {
                                value: Box::new(expr),
                                divergance_type: DivergeExpressionType::Break,
                            }
                            .with_span(span),
                        );
                        continue 'parse;
                    }

                    match break_frame.state {
                        DivergeState::Start => {
                            if !is_keyword_start(self.remaining, "break") {
                                return Err(diagnostic_here(
                                    self.source,
                                    self.remaining,
                                    5,
                                    "Expected break",
                                ));
                            }
                            let after_keyword = &self.remaining[5..];
                            self.remaining = parse_optional_whitespace(after_keyword);
                            let value_start = self.remaining.chars().next();
                            if self.remaining.is_empty()
                                || matches!(
                                    value_start,
                                    Some(';') | Some(')') | Some(']') | Some('}') | Some(',')
                                )
                            {
                                let span = consumed_span(
                                    self.source,
                                    break_frame.start_slice,
                                    self.remaining,
                                );
                                let empty = Expression::new(ExpressionKind::Struct(vec![]), span);
                                completed_expr = Some(
                                    ExpressionKind::Diverge {
                                        value: Box::new(empty),
                                        divergance_type: DivergeExpressionType::Break,
                                    }
                                    .with_span(span),
                                );
                                continue 'parse;
                            }
                            break_frame.state = DivergeState::AwaitValue;
                            frames.push(Frame::Break(break_frame));
                            frames.push(Frame::Expr(ExprFrame::new(0)));
                            continue 'parse;
                        }
                        DivergeState::AwaitValue => {
                            return Err(diagnostic_at_eof(
                                self.source,
                                "Expected break expression",
                            ));
                        }
                    }
                }
            }
        }
    }
}

fn finish_block(block_frame: BlockFrame<'_>) -> Expression {
    let mut expressions = block_frame.expressions;
    if block_frame.ended_with_semicolon {
        if let Some(last_span) = expressions.last().map(|expr| expr.span()) {
            expressions.push(
                ExpressionKind::Struct(vec![]).with_span(SourceSpan::new(last_span.end(), 0)),
            );
        }
    }

    if expressions.len() == 1 {
        expressions.into_iter().next().unwrap()
    } else {
        let span = expressions
            .iter()
            .skip(1)
            .fold(expressions[0].span(), |acc, expr| acc.merge(&expr.span()));
        Expression::new(ExpressionKind::Block(expressions), span)
    }
}

fn build_while_expression(condition: Expression, body: Expression, span: SourceSpan) -> Expression {
    let condition_span = condition.span();
    let break_expr = ExpressionKind::Diverge {
        value: Box::new(Expression::new(ExpressionKind::Struct(vec![]), span)),
        divergance_type: DivergeExpressionType::Break,
    }
    .with_span(condition_span);
    let body_if = ExpressionKind::If {
        condition: Box::new(condition),
        then_branch: Box::new(body),
        else_branch: Box::new(break_expr),
    }
    .with_span(condition_span);
    ExpressionKind::Loop {
        body: Box::new(body_if),
    }
    .with_span(span)
}

fn reduce_stacks(
    operand_stack: &mut Vec<Expression>,
    operator_stack: &mut Vec<String>,
    source: &str,
) -> Result<(), Diagnostic> {
    let operator = operator_stack
        .pop()
        .ok_or_else(|| diagnostic_at_eof(source, "Expected operator when reducing operation"))?;
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
        "@" => ExpressionKind::AttachImplementation {
            type_expr: Box::new(left),
            implementation: Box::new(right),
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

#[cfg(test)]
fn parse_if_expression<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(Expression, &'a str), Diagnostic>> {
    if !is_keyword_start(file, "if") {
        return None;
    }

    Some(parse_if_expression_with_source(source, file))
}

#[cfg(test)]
fn parse_if_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(Expression, &'a str), Diagnostic> {
    let mut parser = Parser::new(source, file);
    let expr = parser.parse_with_frame(Frame::If(IfFrame::new(file)))?;
    Ok((expr, parser.remaining))
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
    if let Some(rest) = file.strip_prefix('\'') {
        return parse_char_literal(rest)
            .map(|(value, remaining)| (ExpressionLiteral::Char(value), remaining));
    }

    if let Some(rest) = file.strip_prefix('"') {
        return parse_string_literal(rest)
            .map(|(bytes, remaining)| (ExpressionLiteral::String(bytes), remaining));
    }

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

fn parse_char_literal(file: &str) -> Option<(u8, &str)> {
    let mut chars = file.chars();
    let (value, consumed) = match chars.next()? {
        '\\' => {
            let next = chars.next()?;
            let value = match next {
                'n' => b'\n',
                'r' => b'\r',
                't' => b'\t',
                '0' => b'\0',
                '\'' => b'\'',
                '"' => b'"',
                '\\' => b'\\',
                _ => return None,
            };
            (value, 1 + next.len_utf8())
        }
        ch if ch.is_ascii() => (ch as u8, ch.len_utf8()),
        _ => return None,
    };

    let remaining = &file[consumed..];
    let Some(after_quote) = remaining.strip_prefix('\'') else {
        return None;
    };
    Some((value, after_quote))
}

fn parse_string_literal(file: &str) -> Option<(Vec<u8>, &str)> {
    let mut bytes = Vec::new();
    let mut idx = 0;
    while idx < file.len() {
        let ch = file[idx..].chars().next()?;
        if ch == '"' {
            let remaining = &file[idx + ch.len_utf8()..];
            return Some((bytes, remaining));
        }
        if ch == '\\' {
            let next_idx = idx + ch.len_utf8();
            let next = file[next_idx..].chars().next()?;
            let value = match next {
                'n' => b'\n',
                'r' => b'\r',
                't' => b'\t',
                '0' => b'\0',
                '\'' => b'\'',
                '"' => b'"',
                '\\' => b'\\',
                _ => return None,
            };
            bytes.push(value);
            idx = next_idx + next.len_utf8();
            continue;
        }
        let mut buf = [0u8; 4];
        let encoded = ch.encode_utf8(&mut buf);
        bytes.extend_from_slice(encoded.as_bytes());
        idx += ch.len_utf8();
    }
    None
}

fn escape_literal_byte(byte: u8, quote: char) -> String {
    match byte {
        b'\n' => "\\n".to_string(),
        b'\r' => "\\r".to_string(),
        b'\t' => "\\t".to_string(),
        b'\0' => "\\0".to_string(),
        b'\\' => "\\\\".to_string(),
        b'\'' if quote == '\'' => "\\'".to_string(),
        b'"' if quote == '"' => "\\\"".to_string(),
        byte if byte.is_ascii_graphic() || byte == b' ' => (byte as char).to_string(),
        other => format!("\\x{:02x}", other),
    }
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
    let mut properties: Vec<(String, SourceSpan)> = Vec::new();
    let mut current = expression;

    loop {
        let Expression { kind, span } = current;
        match kind {
            ExpressionKind::Identifier(identifier) => {
                let mut lvalue = LValue::Identifier(identifier, span);
                for (property, access_span) in properties.into_iter().rev() {
                    lvalue = LValue::PropertyAccess {
                        object: Box::new(lvalue),
                        property,
                        span: access_span,
                    };
                }
                return Ok(lvalue);
            }
            ExpressionKind::PropertyAccess { object, property } => {
                properties.push((property, span));
                current = *object;
            }
            ExpressionKind::FunctionCall { function, argument } => {
                let array = expression_to_lvalue(*function)?;
                return Ok(LValue::ArrayIndex {
                    array: Box::new(array),
                    index: argument,
                    span,
                });
            }
            _ => return Err(Diagnostic::new("Invalid binding pattern expression").with_span(span)),
        }
    }
}

fn pattern_expression_to_binding_pattern(
    pattern_expression: Expression,
) -> Result<BindingPattern, Diagnostic> {
    enum PatternBuildFrame {
        Expr(Expression),
        StructFinish {
            span: SourceSpan,
            field_ids: Vec<Identifier>,
            field_count: usize,
        },
        TypeHintFinish {
            span: SourceSpan,
            type_expr: Expression,
        },
        EnumVariantFinish {
            span: SourceSpan,
            enum_type: Expression,
            variant: Identifier,
            has_payload: bool,
        },
        AnnotatedFinish {
            span: SourceSpan,
            annotations: Vec<BindingAnnotation>,
        },
    }

    let root_span = pattern_expression.span();
    let mut stack = vec![PatternBuildFrame::Expr(pattern_expression)];
    let mut pattern_stack: Vec<BindingPattern> = Vec::new();

    while let Some(frame) = stack.pop() {
        match frame {
            PatternBuildFrame::Expr(expression) => {
                let span = expression.span;
                match expression.kind {
                    ExpressionKind::Literal(expression_literal) => {
                        pattern_stack.push(BindingPattern::Literal(expression_literal, span));
                    }
                    ExpressionKind::Identifier(identifier) => {
                        pattern_stack.push(BindingPattern::Identifier(identifier, span));
                    }
                    ExpressionKind::Struct(items) => {
                        let mut field_ids = Vec::with_capacity(items.len());
                        let mut field_exprs = Vec::with_capacity(items.len());
                        for (identifier, expr) in items {
                            field_ids.push(identifier);
                            field_exprs.push(expr);
                        }
                        stack.push(PatternBuildFrame::StructFinish {
                            span,
                            field_ids,
                            field_count: field_exprs.len(),
                        });
                        for expr in field_exprs.into_iter().rev() {
                            stack.push(PatternBuildFrame::Expr(expr));
                        }
                    }
                    ExpressionKind::Operation {
                        operator,
                        left,
                        right,
                    } if operator == ":" => {
                        stack.push(PatternBuildFrame::TypeHintFinish {
                            span,
                            type_expr: *right,
                        });
                        stack.push(PatternBuildFrame::Expr(*left));
                    }
                    ExpressionKind::FunctionCall { function, argument } => match *function {
                        Expression {
                            kind:
                                ExpressionKind::EnumAccess {
                                    enum_expr, variant, ..
                                },
                            ..
                        } => {
                            stack.push(PatternBuildFrame::EnumVariantFinish {
                                span,
                                enum_type: *enum_expr,
                                variant,
                                has_payload: true,
                            });
                            stack.push(PatternBuildFrame::Expr(*argument));
                        }
                        other => {
                            let annotations = extract_binding_annotations_from_expression(other)?;
                            stack.push(PatternBuildFrame::AnnotatedFinish { span, annotations });
                            stack.push(PatternBuildFrame::Expr(*argument));
                        }
                    },
                    ExpressionKind::EnumAccess {
                        enum_expr, variant, ..
                    } => {
                        pattern_stack.push(BindingPattern::EnumVariant {
                            enum_type: enum_expr,
                            variant,
                            payload: None,
                            span,
                        });
                    }
                    _ => {
                        return Err(
                            Diagnostic::new("Invalid binding pattern expression").with_span(span)
                        );
                    }
                }
            }
            PatternBuildFrame::StructFinish {
                span,
                field_ids,
                field_count,
            } => {
                let mut field_patterns = Vec::with_capacity(field_count);
                for _ in 0..field_count {
                    field_patterns.push(pattern_stack.pop().ok_or_else(|| {
                        Diagnostic::new("Invalid binding pattern expression").with_span(span)
                    })?);
                }
                field_patterns.reverse();
                let fields = field_ids
                    .into_iter()
                    .zip(field_patterns)
                    .collect::<Vec<_>>();
                pattern_stack.push(BindingPattern::Struct(fields, span));
            }
            PatternBuildFrame::TypeHintFinish { span, type_expr } => {
                let inner = pattern_stack.pop().ok_or_else(|| {
                    Diagnostic::new("Invalid binding pattern expression").with_span(span)
                })?;
                pattern_stack.push(BindingPattern::TypeHint(
                    Box::new(inner),
                    Box::new(type_expr),
                    span,
                ));
            }
            PatternBuildFrame::EnumVariantFinish {
                span,
                enum_type,
                variant,
                has_payload,
            } => {
                let payload = if has_payload {
                    Some(Box::new(pattern_stack.pop().ok_or_else(|| {
                        Diagnostic::new("Invalid binding pattern expression").with_span(span)
                    })?))
                } else {
                    None
                };
                pattern_stack.push(BindingPattern::EnumVariant {
                    enum_type: Box::new(enum_type),
                    variant,
                    payload,
                    span,
                });
            }
            PatternBuildFrame::AnnotatedFinish { span, annotations } => {
                let inner = pattern_stack.pop().ok_or_else(|| {
                    Diagnostic::new("Invalid binding pattern expression").with_span(span)
                })?;
                pattern_stack.push(BindingPattern::Annotated {
                    annotations,
                    pattern: Box::new(inner),
                    span,
                });
            }
        }
    }

    pattern_stack
        .pop()
        .ok_or_else(|| Diagnostic::new("Invalid binding pattern expression").with_span(root_span))
}

fn extract_binding_annotations_from_expression(
    expression: Expression,
) -> Result<Vec<BindingAnnotation>, Diagnostic> {
    let mut annotations = Vec::new();
    let mut stack = vec![expression];

    while let Some(expr) = stack.pop() {
        let span = expr.span();
        match expr.kind {
            ExpressionKind::Identifier(Identifier { name: id, .. }) if id == "mut" => {
                annotations.push(BindingAnnotation::Mutable(span));
            }
            ExpressionKind::FunctionCall { function, argument } => match *function {
                Expression {
                    kind: ExpressionKind::Identifier(Identifier { name: id, .. }),
                    span: ann_span,
                } if id == "export" => {
                    annotations.push(BindingAnnotation::Export(*argument, ann_span));
                }
                other => {
                    stack.push(*argument);
                    stack.push(other);
                }
            },
            _ => return Err(Diagnostic::new("Invalid binding annotation").with_span(span)),
        }
    }

    Ok(annotations)
}

#[cfg(test)]
fn parse_grouping_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(Expression, &'a str), Diagnostic>> {
    if !file.starts_with('(') {
        return None;
    }

    let mut parser = Parser::new(source, file);
    let expr = match parser.parse_with_frame(Frame::Grouping(GroupingFrame::new())) {
        Ok(expr) => expr,
        Err(err) => return Some(Err(err)),
    };
    Some(Ok((expr, parser.remaining)))
}

pub fn parse_operator(file: &str) -> Option<(String, &str)> {
    let operator_chars: Vec<char> = vec![
        '+', '-', '*', '/', '=', '!', '<', '>', '&', '|', '^', ':', '.', '@',
    ];

    let stop_sequences = vec![";", ")", ",", "}", "]"];
    for stop_sequence in stop_sequences {
        if file.starts_with(stop_sequence) {
            return None;
        }
    }

    let stop_words = [
        "if", "then", "else", "match", "fn", "let", "return", "break", "loop", "while", "do",
        "for", "in",
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
        "" | "::" | "." | "@" => 8,
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

#[cfg(test)]
fn parse_struct_expression<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(Expression, &'a str), Diagnostic>> {
    if !file.starts_with('{') {
        return None;
    }

    let mut parser = Parser::new(source, file);
    let expr = match parser.parse_with_frame(Frame::Struct(StructFrame::new(file))) {
        Ok(expr) => expr,
        Err(err) => return Some(Err(err)),
    };
    Some(Ok((expr, parser.remaining)))
}

#[cfg(test)]
fn parse_return_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(Expression, &'a str), Diagnostic>> {
    if !is_keyword_start(file, "return") {
        return None;
    }

    let mut parser = Parser::new(source, file);
    let expr = match parser.parse_with_frame(Frame::Return(ReturnFrame::new(file))) {
        Ok(expr) => expr,
        Err(err) => return Some(Err(err)),
    };
    Some(Ok((expr, parser.remaining)))
}

#[cfg(test)]
fn parse_break_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(Expression, &'a str), Diagnostic>> {
    if !is_keyword_start(file, "break") {
        return None;
    }

    let mut parser = Parser::new(source, file);
    let expr = match parser.parse_with_frame(Frame::Break(BreakFrame::new(file))) {
        Ok(expr) => expr,
        Err(err) => return Some(Err(err)),
    };
    Some(Ok((expr, parser.remaining)))
}

#[cfg(test)]
fn parse_loop_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(Expression, &'a str), Diagnostic>> {
    if !is_keyword_start(file, "loop") {
        return None;
    }

    let mut parser = Parser::new(source, file);
    let expr = match parser.parse_with_frame(Frame::Loop(LoopFrame::new(file))) {
        Ok(expr) => expr,
        Err(err) => return Some(Err(err)),
    };
    Some(Ok((expr, parser.remaining)))
}

#[cfg(test)]
fn parse_for_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(Expression, &'a str), Diagnostic>> {
    if !is_keyword_start(file, "for") {
        return None;
    }

    let mut parser = Parser::new(source, file);
    let expr = match parser.parse_with_frame(Frame::For(ForFrame::new(file))) {
        Ok(expr) => expr,
        Err(err) => return Some(Err(err)),
    };
    Some(Ok((expr, parser.remaining)))
}

#[cfg(test)]
fn parse_while_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Option<Result<(Expression, &'a str), Diagnostic>> {
    if !is_keyword_start(file, "while") {
        return None;
    }

    let mut parser = Parser::new(source, file);
    let expr = match parser.parse_with_frame(Frame::While(WhileFrame::new(file))) {
        Ok(expr) => expr,
        Err(err) => return Some(Err(err)),
    };
    Some(Ok((expr, parser.remaining)))
}

#[cfg(test)]
pub fn parse_isolated_expression(file: &str) -> Result<(Expression, &str), Diagnostic> {
    parse_isolated_expression_with_source(file, file)
}

#[cfg(test)]
fn parse_isolated_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(Expression, &'a str), Diagnostic> {
    parse_isolated_expression_with_source_with_guard(source, file)
}

#[cfg(test)]
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
    if let Some(for_parse) = parse_for_expression_with_source(source, file) {
        return for_parse;
    }
    if let Some(while_parse) = parse_while_expression_with_source(source, file) {
        return while_parse;
    }
    if let Some(loop_parse) = parse_loop_expression_with_source(source, file) {
        return loop_parse;
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

#[cfg(test)]
fn parse_operation_expression_with_source<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(Expression, &'a str), Diagnostic> {
    parse_operation_expression_with_guard(source, file)
}

#[cfg(test)]
fn parse_operation_expression_with_guard<'a>(
    source: &'a str,
    file: &'a str,
) -> Result<(Expression, &'a str), Diagnostic> {
    parse_operation_expression_with_min_precedence(source, file, 0)
}

#[cfg(test)]
fn parse_operation_expression_with_min_precedence<'a>(
    source: &'a str,
    file: &'a str,
    min_precedence: u8,
) -> Result<(Expression, &'a str), Diagnostic> {
    let mut parser = Parser::new(source, file);
    let expr = parser.parse_expression_with_min_precedence(min_precedence)?;
    Ok((expr, parser.remaining))
}

pub fn parse_block(file: &str) -> Result<(Expression, &str), Diagnostic> {
    parse_block_with_terminators(file, file, &[])
}

fn parse_block_with_terminators<'a>(
    source: &'a str,
    file: &'a str,
    terminators: &'a [char],
) -> Result<(Expression, &'a str), Diagnostic> {
    let mut parser = Parser::new(source, file);
    let expr = parser.parse_block_with_terminators(terminators)?;
    Ok((expr, parser.remaining))
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
fn parse_for_loop_desugars_to_iterator_binding() {
    let (expr, remaining) = parse_block("for foo in bar do (foo)").unwrap();
    assert!(remaining.trim().is_empty());

    let ExpressionKind::Block(items) = expr.kind else {
        panic!("expected for loop to desugar to block");
    };
    assert_eq!(items.len(), 2);

    let ExpressionKind::Binding(binding) = &items[0].kind else {
        panic!("expected iterator binding");
    };
    let BindingPattern::Annotated {
        annotations,
        pattern,
        ..
    } = &binding.pattern
    else {
        panic!("expected annotated binding pattern");
    };
    assert!(annotations
        .iter()
        .any(|ann| matches!(ann, BindingAnnotation::Mutable(_))));
    assert!(matches!(
        pattern.as_ref(),
        BindingPattern::Identifier(Identifier { name, .. }, _) if name == "bar"
    ));

    let ExpressionKind::Loop { body } = &items[1].kind else {
        panic!("expected loop in for desugaring");
    };
    let ExpressionKind::If { condition, .. } = &body.kind else {
        panic!("expected if inside loop");
    };
    let ExpressionKind::Binding(condition_binding) = &condition.kind else {
        panic!("expected binding condition");
    };
    let BindingPattern::EnumVariant {
        enum_type,
        variant,
        payload,
        ..
    } = &condition_binding.pattern
    else {
        panic!("expected enum variant pattern");
    };
    assert_eq!(variant.name, "Some");
    assert!(matches!(
        payload.as_deref(),
        Some(BindingPattern::Identifier(Identifier { name, .. }, _)) if name == "foo"
    ));
    let ExpressionKind::FunctionCall { function, argument } = &enum_type.kind else {
        panic!("expected Option call");
    };
    assert!(matches!(
        &function.kind,
        ExpressionKind::Identifier(Identifier { name, .. }) if name == "Option"
    ));
    let ExpressionKind::PropertyAccess { object, property } = &argument.kind else {
        panic!("expected iter_ty property access");
    };
    assert_eq!(property, "iter_ty");
    assert!(matches!(
        &object.kind,
        ExpressionKind::Identifier(Identifier { name, .. }) if name == "bar"
    ));

    let ExpressionKind::PropertyAccess {
        object: next_obj,
        property: next_prop,
    } = &condition_binding.expr.kind
    else {
        panic!("expected next property access");
    };
    assert_eq!(next_prop, "next");
    assert!(matches!(
        &next_obj.kind,
        ExpressionKind::Identifier(Identifier { name, .. }) if name == "bar"
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
