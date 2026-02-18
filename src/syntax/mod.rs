use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use crate::diagnostics::SourceSpan;

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
    TypePropertyAccess {
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
            LValue::TypePropertyAccess { span, .. } => *span,
            LValue::ArrayIndex { span, .. } => *span,
        }
    }

    pub fn pretty_print(&self) -> String {
        match self {
            LValue::Identifier(id, _) => id.name.clone(),
            LValue::TypePropertyAccess {
                object, property, ..
            } => {
                format!("{}::{}", object.pretty_print(), property)
            }
            LValue::ArrayIndex { array, index, .. } => {
                format!("{}({})", array.pretty_print(), index.pretty_print())
            }
        }
    }

    pub fn get_used_identifiers(&self) -> HashSet<Identifier> {
        let mut used = HashSet::new();
        collect_identifiers_from_lvalue(self, &mut used);
        used
    }
}

fn collect_identifiers_from_lvalue(lvalue: &LValue, out: &mut HashSet<Identifier>) {
    match lvalue {
        LValue::Identifier(identifier, ..) => {
            out.insert(identifier.clone());
        }
        LValue::TypePropertyAccess { object, .. } => {
            collect_identifiers_from_lvalue(object, out);
        }
        LValue::ArrayIndex { array, index, .. } => {
            collect_identifiers_from_lvalue(array, out);
            collect_identifiers_from_expression(index, out);
        }
    }
}

fn collect_identifiers_from_expression(expr: &Expression, out: &mut HashSet<Identifier>) {
    let mut stack = vec![expr];
    while let Some(expr) = stack.pop() {
        match &expr.kind {
            ExpressionKind::Identifier(identifier) => {
                out.insert(identifier.clone());
            }
            ExpressionKind::IntrinsicType(_) | ExpressionKind::Literal(_) => {}
            ExpressionKind::BoxType(inner) => {
                stack.push(inner.as_ref());
            }
            ExpressionKind::EnumType(items) => {
                for (_, field_expr) in items.iter() {
                    stack.push(field_expr);
                }
            }
            ExpressionKind::EnumValue {
                enum_type, payload, ..
            } => {
                stack.push(payload.as_ref());
                stack.push(enum_type.as_ref());
            }
            ExpressionKind::EnumConstructor {
                enum_type,
                payload_type,
                ..
            } => {
                stack.push(payload_type.as_ref());
                stack.push(enum_type.as_ref());
            }
            ExpressionKind::FunctionType { .. } => {}
            ExpressionKind::Function { .. } => {}
            ExpressionKind::AttachImplementation {
                type_expr,
                implementation,
            } => {
                stack.push(implementation.as_ref());
                stack.push(type_expr.as_ref());
            }
            ExpressionKind::IntrinsicOperation(intrinsic_operation) => match intrinsic_operation {
                IntrinsicOperation::Binary(left, right, _) => {
                    stack.push(right.as_ref());
                    stack.push(left.as_ref());
                }
                IntrinsicOperation::Unary(operand, _) => {
                    stack.push(operand.as_ref());
                }
                IntrinsicOperation::InlineAssembly { code, .. } => {
                    stack.push(code.as_ref());
                }
            },
            ExpressionKind::Struct(items) => {
                for (_, value_expr) in items.iter() {
                    stack.push(value_expr);
                }
            }
            ExpressionKind::ArrayRepeat { value, count } => {
                stack.push(count.as_ref());
                stack.push(value.as_ref());
            }
            ExpressionKind::Operation { left, right, .. } => {
                stack.push(right.as_ref());
                stack.push(left.as_ref());
            }
            ExpressionKind::Assignment { expr, .. } => {
                stack.push(expr.as_ref());
            }
            ExpressionKind::FunctionCall { function, argument } => {
                stack.push(argument.as_ref());
                stack.push(function.as_ref());
            }
            ExpressionKind::ArrayIndex { array, index } => {
                stack.push(index.as_ref());
                stack.push(array.as_ref());
            }
            ExpressionKind::TypePropertyAccess { object, .. } => {
                stack.push(object.as_ref());
            }
            ExpressionKind::Binding(binding) => {
                stack.push(&binding.expr);
            }
            ExpressionKind::Block(expressions) => {
                for expr in expressions.iter() {
                    stack.push(expr);
                }
            }
            ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                stack.push(else_branch.as_ref());
                stack.push(then_branch.as_ref());
                stack.push(condition.as_ref());
            }
            ExpressionKind::Match { value, branches } => {
                for (_, branch) in branches.iter() {
                    stack.push(branch);
                }
                stack.push(value.as_ref());
            }
            ExpressionKind::Diverge { value, .. } => {
                stack.push(value.as_ref());
            }
            ExpressionKind::Loop { body } => {
                stack.push(body.as_ref());
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
        annotations: Vec<Expression>,
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
    WgslTarget,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BindingAnnotationLiteral {
    Mut,
    Export(TargetLiteral),
    Target(TargetLiteral),
    Wrap(TargetLiteral),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpressionLiteral {
    Number(i32),
    Boolean(bool),
    Char(u8),
    String(Vec<u8>),
    Target(TargetLiteral),
    BindingAnnotation(BindingAnnotationLiteral),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntrinsicType {
    I32,
    U8,
    Boolean,
    Type,
    Target,
    BindingAnnotation,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryIntrinsicOperator {
    BooleanNot,
    EnumFromStruct,
    MatchFromStruct,
    UseFromString,
    BoxFromType,
    BindingAnnotationExportFromTarget,
    BindingAnnotationTargetFromTarget,
    BindingAnnotationWrapFromTarget,
    AssemblyFromTarget,
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
    Binary(Rc<Expression>, Rc<Expression>, BinaryIntrinsicOperator),
    Unary(Rc<Expression>, UnaryIntrinsicOperator),
    InlineAssembly {
        target: TargetLiteral,
        code: Rc<Expression>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DivergeExpressionType {
    Return,
    Break,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpressionKind {
    IntrinsicType(IntrinsicType),
    BoxType(Rc<Expression>),
    IntrinsicOperation(IntrinsicOperation),
    EnumType(Vec<(Identifier, Expression)>),
    Match {
        value: Rc<Expression>,
        branches: Vec<(BindingPattern, Expression)>,
    },
    EnumValue {
        enum_type: Rc<Expression>,
        variant: Identifier,
        variant_index: usize,
        payload: Rc<Expression>,
    },
    EnumConstructor {
        enum_type: Rc<Expression>,
        variant: Identifier,
        variant_index: usize,
        payload_type: Rc<Expression>,
    },
    If {
        condition: Rc<Expression>,
        then_branch: Rc<Expression>,
        else_branch: Rc<Expression>,
    },
    AttachImplementation {
        type_expr: Rc<Expression>,
        implementation: Rc<Expression>,
    },
    Function {
        parameter: BindingPattern,
        return_type: Option<Rc<Expression>>,
        body: Rc<Expression>,
    },
    FunctionType {
        parameter: Rc<Expression>,
        return_type: Rc<Expression>,
    },
    Struct(Vec<(Identifier, Expression)>),
    ArrayRepeat {
        value: Rc<Expression>,
        count: Rc<Expression>,
    },
    Literal(ExpressionLiteral),
    Identifier(Identifier),
    Operation {
        operator: String,
        left: Rc<Expression>,
        right: Rc<Expression>,
    },
    Assignment {
        target: LValue,
        expr: Rc<Expression>,
    },
    FunctionCall {
        function: Rc<Expression>,
        argument: Rc<Expression>,
    },
    ArrayIndex {
        array: Rc<Expression>,
        index: Rc<Expression>,
    },
    TypePropertyAccess {
        object: Rc<Expression>,
        property: String,
    },
    Binding(Rc<Binding>),
    Block(Vec<Expression>),
    Diverge {
        value: Rc<Expression>,
        divergance_type: DivergeExpressionType,
    },
    Loop {
        body: Rc<Expression>,
    },
}

impl ExpressionKind {
    pub fn with_span(self, span: SourceSpan) -> Expression {
        Expression {
            kind: self,
            span,
            type_cache: RefCell::new(None),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: SourceSpan,
    pub type_cache: RefCell<Option<Rc<Expression>>>,
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.span == other.span
    }
}

impl Eq for Expression {}

impl Expression {
    pub fn new(kind: ExpressionKind, span: SourceSpan) -> Self {
        Self {
            kind,
            span,
            type_cache: RefCell::new(None),
        }
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
    Target(TargetLiteral, SourceSpan),
    Wrap(TargetLiteral, SourceSpan),
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
                TargetLiteral::WgslTarget => "wgsl".to_string(),
            },
            ExpressionLiteral::BindingAnnotation(binding_annotation) => match binding_annotation {
                BindingAnnotationLiteral::Mut => "mut".to_string(),
                BindingAnnotationLiteral::Export(target) => {
                    let target_str = match target {
                        TargetLiteral::JSTarget => "js",
                        TargetLiteral::WasmTarget => "wasm",
                        TargetLiteral::WgslTarget => "wgsl",
                    };
                    format!("export {}", target_str)
                }
                BindingAnnotationLiteral::Target(target) => {
                    let target_str = match target {
                        TargetLiteral::JSTarget => "js",
                        TargetLiteral::WasmTarget => "wasm",
                        TargetLiteral::WgslTarget => "wgsl",
                    };
                    format!("target {}", target_str)
                }
                BindingAnnotationLiteral::Wrap(target) => {
                    let target_str = match target {
                        TargetLiteral::JSTarget => "js",
                        TargetLiteral::WasmTarget => "wasm",
                        TargetLiteral::WgslTarget => "wgsl",
                    };
                    format!("wrap {}", target_str)
                }
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
                        IntrinsicType::BindingAnnotation => "binding_annotation",
                    };
                    context.tasks.push(PrettyTask::WriteStatic(ty_str));
                }
                ExpressionKind::BoxType(inner) => {
                    context.tasks.push(PrettyTask::WriteStatic(")"));
                    context.tasks.push(PrettyTask::Expr(inner));
                    context.tasks.push(PrettyTask::WriteStatic("Box("));
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
                            UnaryIntrinsicOperator::BoxFromType => "Box",
                            UnaryIntrinsicOperator::BindingAnnotationExportFromTarget => "export",
                            UnaryIntrinsicOperator::BindingAnnotationTargetFromTarget => "target",
                            UnaryIntrinsicOperator::BindingAnnotationWrapFromTarget => "wrap",
                            UnaryIntrinsicOperator::AssemblyFromTarget => "asm",
                        };
                        context.tasks.push(PrettyTask::WriteStatic(")"));
                        context.tasks.push(PrettyTask::Expr(operand));
                        context.tasks.push(PrettyTask::WriteStatic("("));
                        context.tasks.push(PrettyTask::WriteStatic(op_str));
                    }
                    IntrinsicOperation::InlineAssembly { target, code } => {
                        let target_str = match target {
                            TargetLiteral::JSTarget => "js",
                            TargetLiteral::WasmTarget => "wasm",
                            TargetLiteral::WgslTarget => "wgsl",
                        };
                        context.tasks.push(PrettyTask::WriteStatic(")"));
                        context.tasks.push(PrettyTask::Expr(code));
                        context.tasks.push(PrettyTask::WriteStatic(")("));
                        context.tasks.push(PrettyTask::WriteStatic(target_str));
                        context.tasks.push(PrettyTask::WriteStatic("asm("));
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
                ExpressionKind::ArrayRepeat { value, count } => {
                    context.tasks.push(PrettyTask::WriteStatic(" }"));
                    context.tasks.push(PrettyTask::Expr(count));
                    context.tasks.push(PrettyTask::WriteStatic("; "));
                    context.tasks.push(PrettyTask::Expr(value));
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
                ExpressionKind::TypePropertyAccess { object, property } => {
                    context.tasks.push(PrettyTask::WriteOwned(property.clone()));
                    context.tasks.push(PrettyTask::WriteStatic("::"));
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
                        context.tasks.push(PrettyTask::Expr(annotation));
                        context.tasks.push(PrettyTask::WriteStatic("("));
                    }
                }
            },
        }
    }

    String::new()
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
