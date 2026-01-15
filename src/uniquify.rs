use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use crate::diagnostics::SourceSpan;
use crate::parsing::{
    BinaryIntrinsicOperator, Binding, BindingAnnotation, BindingPattern, DivergeExpressionType,
    Expression, ExpressionKind, Identifier, IntrinsicOperation, LValue, UnaryIntrinsicOperator,
};

#[derive(Default, Clone)]
struct ScopeStack {
    scopes: Vec<HashMap<String, Identifier>>,
}

impl ScopeStack {
    fn new() -> Self {
        ScopeStack {
            scopes: vec![HashMap::new()],
        }
    }

    fn push(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn insert(&mut self, name: String, identifier: Identifier) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, identifier);
        }
    }

    fn resolve(&self, identifier: &Identifier) -> Identifier {
        for scope in self.scopes.iter().rev() {
            if let Some(mapped) = scope.get(&identifier.name) {
                return mapped.clone();
            }
        }
        identifier.clone()
    }
}

fn fresh_identifier(identifier: &Identifier) -> Identifier {
    Identifier::with_unique(identifier.name.clone(), generate_uuid_like())
}

fn generate_uuid_like() -> String {
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    let count = COUNTER.fetch_add(1, Ordering::Relaxed) as u128;
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    let bits = timestamp ^ count;
    format!(
        "{:08x}-{:04x}-{:04x}-{:04x}-{:012x}",
        (bits >> 96) as u32,
        (bits >> 80) as u16,
        (bits >> 64) as u16,
        (bits >> 48) as u16,
        bits as u64
    )
}

enum Value {
    Expr(Expression),
    Pattern {
        pattern: BindingPattern,
        scope: ScopeStack,
    },
    LValue(LValue),
    Annotations(Vec<BindingAnnotation>),
}

enum Task {
    Expr(Expression, ScopeStack),
    Pattern(BindingPattern, ScopeStack),
    LValue(LValue, ScopeStack),
    Annotations(Vec<BindingAnnotation>, ScopeStack),
    BuildBinary {
        span: SourceSpan,
        op: BinaryIntrinsicOperator,
    },
    BuildUnary {
        span: SourceSpan,
        op: UnaryIntrinsicOperator,
    },
    BuildEnumType {
        span: SourceSpan,
        ids: Vec<Identifier>,
    },
    BuildEnumValue {
        span: SourceSpan,
        variant: Identifier,
        variant_index: usize,
    },
    BuildEnumConstructor {
        span: SourceSpan,
        variant: Identifier,
        variant_index: usize,
    },
    BuildAttachImplementation {
        span: SourceSpan,
    },
    BuildFunctionType {
        span: SourceSpan,
    },
    BuildBoxType {
        span: SourceSpan,
    },
    BuildStruct {
        span: SourceSpan,
        ids: Vec<Identifier>,
    },
    BuildOperation {
        span: SourceSpan,
        operator: String,
    },
    BuildAssignment {
        span: SourceSpan,
    },
    BuildFunctionCall {
        span: SourceSpan,
    },
    BuildArrayIndex {
        span: SourceSpan,
    },
    BuildTypePropertyAccess {
        span: SourceSpan,
        property: String,
    },
    BuildDiverge {
        span: SourceSpan,
        divergance_type: DivergeExpressionType,
    },
    BuildLoop {
        span: SourceSpan,
    },
    BuildLValueProperty {
        span: SourceSpan,
        property: String,
    },
    BuildLValueArrayIndex {
        span: SourceSpan,
    },
    ContinueFunctionParam {
        span: SourceSpan,
        return_type: Option<Box<Expression>>,
        body: Box<Expression>,
    },
    ContinueFunctionReturnType {
        span: SourceSpan,
        body: Box<Expression>,
        parameter: BindingPattern,
        scope: ScopeStack,
    },
    ContinueFunctionBody {
        span: SourceSpan,
        parameter: BindingPattern,
        return_type: Option<Expression>,
    },
    ContinueBindingPattern {
        span: SourceSpan,
        expr: Expression,
    },
    ContinueBindingExpr {
        span: SourceSpan,
        pattern: BindingPattern,
    },
    ContinueMatchValue {
        span: SourceSpan,
        outer_scope: ScopeStack,
        branches: std::vec::IntoIter<(BindingPattern, Expression)>,
    },
    ContinueMatchBranchPattern {
        span: SourceSpan,
        outer_scope: ScopeStack,
        branches: std::vec::IntoIter<(BindingPattern, Expression)>,
        value: Expression,
        acc: Vec<(BindingPattern, Expression)>,
        branch_expr: Expression,
    },
    ContinueMatchBranchExpr {
        span: SourceSpan,
        outer_scope: ScopeStack,
        branches: std::vec::IntoIter<(BindingPattern, Expression)>,
        value: Expression,
        acc: Vec<(BindingPattern, Expression)>,
        pattern: BindingPattern,
    },
    ContinueIfCondition {
        span: SourceSpan,
        then_branch: Box<Expression>,
        else_branch: Box<Expression>,
        then_scope: ScopeStack,
        else_scope: ScopeStack,
    },
    ContinueIfThen {
        span: SourceSpan,
        condition: Expression,
        else_branch: Box<Expression>,
        else_scope: ScopeStack,
    },
    ContinueIfElse {
        span: SourceSpan,
        condition: Expression,
        then_expr: Expression,
    },
    ContinuePatternStructField {
        span: SourceSpan,
        iter: std::vec::IntoIter<(Identifier, BindingPattern)>,
        acc: Vec<(Identifier, BindingPattern)>,
        field_id: Identifier,
    },
    ContinuePatternEnumVariant {
        span: SourceSpan,
        variant: Identifier,
        payload: Option<Box<BindingPattern>>,
        scope: ScopeStack,
    },
    ContinuePatternEnumVariantPayload {
        span: SourceSpan,
        variant: Identifier,
        enum_type: Expression,
    },
    ContinuePatternTypeHint {
        span: SourceSpan,
        ty: Box<Expression>,
    },
    ContinuePatternTypeHintExpr {
        span: SourceSpan,
        inner: BindingPattern,
        scope: ScopeStack,
    },
    ContinuePatternAnnotated {
        span: SourceSpan,
        pattern: Box<BindingPattern>,
        scope: ScopeStack,
    },
    ContinuePatternAnnotatedPattern {
        span: SourceSpan,
        annotations: Vec<BindingAnnotation>,
    },
    ContinueAnnotationsItem {
        iter: std::vec::IntoIter<BindingAnnotation>,
        scope: ScopeStack,
        acc: Vec<BindingAnnotation>,
        annotation: BindingAnnotation,
    },
    ContinueAnnotationsExport {
        iter: std::vec::IntoIter<BindingAnnotation>,
        scope: ScopeStack,
        acc: Vec<BindingAnnotation>,
        span: SourceSpan,
    },
}

fn pop_expr(results: &mut Vec<Value>) -> Expression {
    match results.pop() {
        Some(Value::Expr(expr)) => expr,
        _ => panic!("expected expression result"),
    }
}

fn pop_pattern(results: &mut Vec<Value>) -> (BindingPattern, ScopeStack) {
    match results.pop() {
        Some(Value::Pattern { pattern, scope }) => (pattern, scope),
        _ => panic!("expected pattern result"),
    }
}

fn pop_lvalue(results: &mut Vec<Value>) -> LValue {
    match results.pop() {
        Some(Value::LValue(lvalue)) => lvalue,
        _ => panic!("expected lvalue result"),
    }
}

fn pop_annotations(results: &mut Vec<Value>) -> Vec<BindingAnnotation> {
    match results.pop() {
        Some(Value::Annotations(annotations)) => annotations,
        _ => panic!("expected annotations result"),
    }
}

fn uniquify_expression_iter(expr: Expression, scopes: ScopeStack) -> Expression {
    let mut tasks = Vec::new();
    let mut results = Vec::new();
    tasks.push(Task::Expr(expr, scopes));

    while let Some(task) = tasks.pop() {
        match task {
            Task::Expr(expr, scope) => {
                let span = expr.span;
                match expr.kind {
                    ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(
                        left,
                        right,
                        op,
                    )) => {
                        tasks.push(Task::BuildBinary { span, op });
                        tasks.push(Task::Expr(*right, scope.clone()));
                        tasks.push(Task::Expr(*left, scope));
                    }
                    ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(operand, op)) => {
                        tasks.push(Task::BuildUnary { span, op });
                        tasks.push(Task::Expr(*operand, scope));
                    }
                    ExpressionKind::BoxType(inner) => {
                        tasks.push(Task::BuildBoxType { span });
                        tasks.push(Task::Expr(*inner, scope));
                    }
                    ExpressionKind::EnumType(variants) => {
                        let ids = variants.iter().map(|(id, _)| id.clone()).collect();
                        tasks.push(Task::BuildEnumType { span, ids });
                        for (_, expr) in variants.into_iter().rev() {
                            tasks.push(Task::Expr(expr, scope.clone()));
                        }
                    }
                    ExpressionKind::Match { value, branches } => {
                        let outer_scope = scope.clone();
                        tasks.push(Task::ContinueMatchValue {
                            span,
                            outer_scope,
                            branches: branches.into_iter(),
                        });
                        tasks.push(Task::Expr(*value, scope));
                    }
                    ExpressionKind::EnumValue {
                        enum_type,
                        variant,
                        variant_index,
                        payload,
                    } => {
                        tasks.push(Task::BuildEnumValue {
                            span,
                            variant,
                            variant_index,
                        });
                        tasks.push(Task::Expr(*payload, scope.clone()));
                        tasks.push(Task::Expr(*enum_type, scope));
                    }
                    ExpressionKind::EnumConstructor {
                        enum_type,
                        variant,
                        variant_index,
                        payload_type,
                    } => {
                        tasks.push(Task::BuildEnumConstructor {
                            span,
                            variant,
                            variant_index,
                        });
                        tasks.push(Task::Expr(*payload_type, scope.clone()));
                        tasks.push(Task::Expr(*enum_type, scope));
                    }
                    ExpressionKind::If {
                        condition,
                        then_branch,
                        else_branch,
                    } => {
                        let mut then_scope = scope.clone();
                        then_scope.push();
                        let mut else_scope = scope.clone();
                        else_scope.push();
                        tasks.push(Task::ContinueIfCondition {
                            span,
                            then_branch,
                            else_branch,
                            then_scope,
                            else_scope,
                        });
                        tasks.push(Task::Expr(*condition, scope));
                    }
                    ExpressionKind::AttachImplementation {
                        type_expr,
                        implementation,
                    } => {
                        tasks.push(Task::BuildAttachImplementation { span });
                        tasks.push(Task::Expr(*implementation, scope.clone()));
                        tasks.push(Task::Expr(*type_expr, scope));
                    }
                    ExpressionKind::Function {
                        parameter,
                        return_type,
                        body,
                    } => {
                        let mut function_scope = scope.clone();
                        function_scope.push();
                        tasks.push(Task::ContinueFunctionParam {
                            span,
                            return_type,
                            body,
                        });
                        tasks.push(Task::Pattern(parameter, function_scope));
                    }
                    ExpressionKind::FunctionType {
                        parameter,
                        return_type,
                    } => {
                        let mut function_type_scope = scope.clone();
                        function_type_scope.push();
                        tasks.push(Task::BuildFunctionType { span });
                        tasks.push(Task::Expr(*return_type, function_type_scope.clone()));
                        tasks.push(Task::Expr(*parameter, function_type_scope));
                    }
                    ExpressionKind::Struct(fields) => {
                        let ids = fields.iter().map(|(id, _)| id.clone()).collect();
                        tasks.push(Task::BuildStruct { span, ids });
                        for (_, expr) in fields.into_iter().rev() {
                            tasks.push(Task::Expr(expr, scope.clone()));
                        }
                    }
                    ExpressionKind::IntrinsicType(ty) => {
                        results.push(Value::Expr(
                            ExpressionKind::IntrinsicType(ty).with_span(span),
                        ));
                    }
                    ExpressionKind::Literal(literal) => {
                        results.push(Value::Expr(
                            ExpressionKind::Literal(literal).with_span(span),
                        ));
                    }
                    ExpressionKind::Identifier(identifier) => {
                        let resolved = scope.resolve(&identifier);
                        results.push(Value::Expr(
                            ExpressionKind::Identifier(resolved).with_span(span),
                        ));
                    }
                    ExpressionKind::Operation {
                        operator,
                        left,
                        right,
                    } => {
                        tasks.push(Task::BuildOperation { span, operator });
                        tasks.push(Task::Expr(*right, scope.clone()));
                        tasks.push(Task::Expr(*left, scope));
                    }
                    ExpressionKind::Assignment { target, expr } => {
                        tasks.push(Task::BuildAssignment { span });
                        tasks.push(Task::Expr(*expr, scope.clone()));
                        tasks.push(Task::LValue(target, scope));
                    }
                    ExpressionKind::FunctionCall { function, argument } => {
                        tasks.push(Task::BuildFunctionCall { span });
                        tasks.push(Task::Expr(*argument, scope.clone()));
                        tasks.push(Task::Expr(*function, scope));
                    }
                    ExpressionKind::ArrayIndex { array, index } => {
                        tasks.push(Task::BuildArrayIndex { span });
                        tasks.push(Task::Expr(*index, scope.clone()));
                        tasks.push(Task::Expr(*array, scope));
                    }
                    ExpressionKind::TypePropertyAccess { object, property } => {
                        tasks.push(Task::BuildTypePropertyAccess { span, property });
                        tasks.push(Task::Expr(*object, scope));
                    }
                    ExpressionKind::Binding(binding) => {
                        let mut binding_scope = scope.clone();
                        binding_scope.push();
                        tasks.push(Task::ContinueBindingPattern {
                            span,
                            expr: binding.expr,
                        });
                        tasks.push(Task::Pattern(binding.pattern, binding_scope));
                    }
                    ExpressionKind::Block(expressions) => {
                        results.push(Value::Expr(Expression::new(
                            ExpressionKind::Block(expressions),
                            span,
                        )));
                    }
                    ExpressionKind::Diverge {
                        value,
                        divergance_type,
                    } => {
                        tasks.push(Task::BuildDiverge {
                            span,
                            divergance_type,
                        });
                        tasks.push(Task::Expr(*value, scope));
                    }
                    ExpressionKind::Loop { body } => {
                        let mut loop_scope = scope.clone();
                        loop_scope.push();
                        tasks.push(Task::BuildLoop { span });
                        tasks.push(Task::Expr(*body, loop_scope));
                    }
                }
            }
            Task::Pattern(pattern, mut scope) => match pattern {
                BindingPattern::Identifier(identifier, span) => {
                    let fresh = fresh_identifier(&identifier);
                    scope.insert(identifier.name, fresh.clone());
                    results.push(Value::Pattern {
                        pattern: BindingPattern::Identifier(fresh, span),
                        scope,
                    });
                }
                BindingPattern::Literal(_, _) => {
                    results.push(Value::Pattern { pattern, scope });
                }
                BindingPattern::Struct(fields, span) => {
                    let mut iter = fields.into_iter();
                    if let Some((field_id, sub_pattern)) = iter.next() {
                        let child_scope = scope.clone();
                        tasks.push(Task::ContinuePatternStructField {
                            span,
                            iter,
                            acc: Vec::new(),
                            field_id,
                        });
                        tasks.push(Task::Pattern(sub_pattern, child_scope));
                    } else {
                        results.push(Value::Pattern {
                            pattern: BindingPattern::Struct(Vec::new(), span),
                            scope,
                        });
                    }
                }
                BindingPattern::EnumVariant {
                    enum_type,
                    variant,
                    payload,
                    span,
                } => {
                    let expr_scope = scope.clone();
                    tasks.push(Task::ContinuePatternEnumVariant {
                        span,
                        variant,
                        payload,
                        scope,
                    });
                    tasks.push(Task::Expr(*enum_type, expr_scope));
                }
                BindingPattern::TypeHint(inner, ty, span) => {
                    let inner_scope = scope.clone();
                    tasks.push(Task::ContinuePatternTypeHint { span, ty });
                    tasks.push(Task::Pattern(*inner, inner_scope));
                }
                BindingPattern::Annotated {
                    annotations,
                    pattern,
                    span,
                } => {
                    let annotations_scope = scope.clone();
                    tasks.push(Task::ContinuePatternAnnotated {
                        span,
                        pattern,
                        scope,
                    });
                    tasks.push(Task::Annotations(annotations, annotations_scope));
                }
            },
            Task::LValue(lvalue, scope) => match lvalue {
                LValue::Identifier(identifier, span) => {
                    results.push(Value::LValue(LValue::Identifier(
                        scope.resolve(&identifier),
                        span,
                    )));
                }
                LValue::TypePropertyAccess {
                    object,
                    property,
                    span,
                } => {
                    tasks.push(Task::BuildLValueProperty { span, property });
                    tasks.push(Task::LValue(*object, scope));
                }
                LValue::ArrayIndex { array, index, span } => {
                    tasks.push(Task::BuildLValueArrayIndex { span });
                    tasks.push(Task::Expr(*index, scope.clone()));
                    tasks.push(Task::LValue(*array, scope));
                }
            },
            Task::Annotations(annotations, scope) => {
                let mut iter = annotations.into_iter();
                if let Some(annotation) = iter.next() {
                    tasks.push(Task::ContinueAnnotationsItem {
                        iter,
                        scope,
                        acc: Vec::new(),
                        annotation,
                    });
                } else {
                    results.push(Value::Annotations(Vec::new()));
                }
            }
            Task::BuildBinary { span, op } => {
                let right = pop_expr(&mut results);
                let left = pop_expr(&mut results);
                results.push(Value::Expr(
                    ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(
                        Box::new(left),
                        Box::new(right),
                        op,
                    ))
                    .with_span(span),
                ));
            }
            Task::BuildUnary { span, op } => {
                let operand = pop_expr(&mut results);
                results.push(Value::Expr(
                    ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(
                        Box::new(operand),
                        op,
                    ))
                    .with_span(span),
                ));
            }
            Task::BuildEnumType { span, ids } => {
                let mut fields = Vec::with_capacity(ids.len());
                for id in ids.iter().rev() {
                    let expr = pop_expr(&mut results);
                    fields.push((id.clone(), expr));
                }
                fields.reverse();
                results.push(Value::Expr(
                    ExpressionKind::EnumType(fields).with_span(span),
                ));
            }
            Task::BuildEnumValue {
                span,
                variant,
                variant_index,
            } => {
                let payload = pop_expr(&mut results);
                let enum_type = pop_expr(&mut results);
                results.push(Value::Expr(
                    ExpressionKind::EnumValue {
                        enum_type: Box::new(enum_type),
                        variant,
                        variant_index,
                        payload: Box::new(payload),
                    }
                    .with_span(span),
                ));
            }
            Task::BuildEnumConstructor {
                span,
                variant,
                variant_index,
            } => {
                let payload_type = pop_expr(&mut results);
                let enum_type = pop_expr(&mut results);
                results.push(Value::Expr(
                    ExpressionKind::EnumConstructor {
                        enum_type: Box::new(enum_type),
                        variant,
                        variant_index,
                        payload_type: Box::new(payload_type),
                    }
                    .with_span(span),
                ));
            }
            Task::BuildAttachImplementation { span } => {
                let implementation = pop_expr(&mut results);
                let type_expr = pop_expr(&mut results);
                results.push(Value::Expr(
                    ExpressionKind::AttachImplementation {
                        type_expr: Box::new(type_expr),
                        implementation: Box::new(implementation),
                    }
                    .with_span(span),
                ));
            }
            Task::BuildFunctionType { span } => {
                let return_type = pop_expr(&mut results);
                let parameter = pop_expr(&mut results);
                results.push(Value::Expr(
                    ExpressionKind::FunctionType {
                        parameter: Box::new(parameter),
                        return_type: Box::new(return_type),
                    }
                    .with_span(span),
                ));
            }
            Task::BuildBoxType { span } => {
                let inner = pop_expr(&mut results);
                results.push(Value::Expr(
                    ExpressionKind::BoxType(Box::new(inner)).with_span(span),
                ));
            }
            Task::BuildStruct { span, ids } => {
                let mut fields = Vec::with_capacity(ids.len());
                for id in ids.iter().rev() {
                    let expr = pop_expr(&mut results);
                    fields.push((id.clone(), expr));
                }
                fields.reverse();
                results.push(Value::Expr(ExpressionKind::Struct(fields).with_span(span)));
            }
            Task::BuildOperation { span, operator } => {
                let right = pop_expr(&mut results);
                let left = pop_expr(&mut results);
                results.push(Value::Expr(
                    ExpressionKind::Operation {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    }
                    .with_span(span),
                ));
            }
            Task::BuildAssignment { span } => {
                let expr = pop_expr(&mut results);
                let target = pop_lvalue(&mut results);
                results.push(Value::Expr(
                    ExpressionKind::Assignment {
                        target,
                        expr: Box::new(expr),
                    }
                    .with_span(span),
                ));
            }
            Task::BuildFunctionCall { span } => {
                let argument = pop_expr(&mut results);
                let function = pop_expr(&mut results);
                results.push(Value::Expr(
                    ExpressionKind::FunctionCall {
                        function: Box::new(function),
                        argument: Box::new(argument),
                    }
                    .with_span(span),
                ));
            }
            Task::BuildArrayIndex { span } => {
                let index = pop_expr(&mut results);
                let array = pop_expr(&mut results);
                results.push(Value::Expr(
                    ExpressionKind::ArrayIndex {
                        array: Box::new(array),
                        index: Box::new(index),
                    }
                    .with_span(span),
                ));
            }
            Task::BuildTypePropertyAccess { span, property } => {
                let object = pop_expr(&mut results);
                results.push(Value::Expr(
                    ExpressionKind::TypePropertyAccess {
                        object: Box::new(object),
                        property,
                    }
                    .with_span(span),
                ));
            }
            Task::BuildDiverge {
                span,
                divergance_type,
            } => {
                let value = pop_expr(&mut results);
                results.push(Value::Expr(
                    ExpressionKind::Diverge {
                        value: Box::new(value),
                        divergance_type,
                    }
                    .with_span(span),
                ));
            }
            Task::BuildLoop { span } => {
                let body = pop_expr(&mut results);
                results.push(Value::Expr(
                    ExpressionKind::Loop {
                        body: Box::new(body),
                    }
                    .with_span(span),
                ));
            }
            Task::BuildLValueProperty { span, property } => {
                let object = pop_lvalue(&mut results);
                results.push(Value::LValue(LValue::TypePropertyAccess {
                    object: Box::new(object),
                    property,
                    span,
                }));
            }
            Task::BuildLValueArrayIndex { span } => {
                let index = pop_expr(&mut results);
                let array = pop_lvalue(&mut results);
                results.push(Value::LValue(LValue::ArrayIndex {
                    array: Box::new(array),
                    index: Box::new(index),
                    span,
                }));
            }
            Task::ContinueFunctionParam {
                span,
                return_type,
                body,
            } => {
                let (parameter, scope) = pop_pattern(&mut results);
                if let Some(return_type) = return_type {
                    let return_scope = scope.clone();
                    tasks.push(Task::ContinueFunctionReturnType {
                        span,
                        body,
                        parameter,
                        scope,
                    });
                    tasks.push(Task::Expr(*return_type, return_scope));
                } else {
                    tasks.push(Task::ContinueFunctionBody {
                        span,
                        parameter,
                        return_type: None,
                    });
                    tasks.push(Task::Expr(*body, scope));
                }
            }
            Task::ContinueFunctionReturnType {
                span,
                body,
                parameter,
                scope,
            } => {
                let return_type = pop_expr(&mut results);
                tasks.push(Task::ContinueFunctionBody {
                    span,
                    parameter,
                    return_type: Some(return_type),
                });
                tasks.push(Task::Expr(*body, scope));
            }
            Task::ContinueFunctionBody {
                span,
                parameter,
                return_type,
            } => {
                let body = pop_expr(&mut results);
                let return_type = return_type.map(Box::new);
                results.push(Value::Expr(
                    ExpressionKind::Function {
                        parameter,
                        return_type,
                        body: Box::new(body),
                    }
                    .with_span(span),
                ));
            }
            Task::ContinueBindingPattern { span, expr } => {
                let (pattern, scope) = pop_pattern(&mut results);
                tasks.push(Task::ContinueBindingExpr { span, pattern });
                tasks.push(Task::Expr(expr, scope));
            }
            Task::ContinueBindingExpr { span, pattern } => {
                let expr = pop_expr(&mut results);
                results.push(Value::Expr(
                    ExpressionKind::Binding(Box::new(Binding { pattern, expr })).with_span(span),
                ));
            }
            Task::ContinueMatchValue {
                span,
                outer_scope,
                mut branches,
            } => {
                let value = pop_expr(&mut results);
                if let Some((pattern, branch_expr)) = branches.next() {
                    let mut branch_scope = outer_scope.clone();
                    branch_scope.push();
                    tasks.push(Task::ContinueMatchBranchPattern {
                        span,
                        outer_scope,
                        branches,
                        value,
                        acc: Vec::new(),
                        branch_expr,
                    });
                    tasks.push(Task::Pattern(pattern, branch_scope));
                } else {
                    results.push(Value::Expr(Expression::new(
                        ExpressionKind::Match {
                            value: Box::new(value),
                            branches: Vec::new(),
                        },
                        span,
                    )));
                }
            }
            Task::ContinueMatchBranchPattern {
                span,
                outer_scope,
                branches,
                value,
                acc,
                branch_expr,
            } => {
                let (pattern, branch_scope) = pop_pattern(&mut results);
                let expr_scope = branch_scope.clone();
                tasks.push(Task::ContinueMatchBranchExpr {
                    span,
                    outer_scope,
                    branches,
                    value,
                    acc,
                    pattern,
                });
                tasks.push(Task::Expr(branch_expr, expr_scope));
            }
            Task::ContinueMatchBranchExpr {
                span,
                outer_scope,
                mut branches,
                value,
                mut acc,
                pattern,
            } => {
                let branch_expr = pop_expr(&mut results);
                acc.push((pattern, branch_expr));
                if let Some((pattern, branch_expr)) = branches.next() {
                    let mut branch_scope = outer_scope.clone();
                    branch_scope.push();
                    tasks.push(Task::ContinueMatchBranchPattern {
                        span,
                        outer_scope,
                        branches,
                        value,
                        acc,
                        branch_expr,
                    });
                    tasks.push(Task::Pattern(pattern, branch_scope));
                } else {
                    results.push(Value::Expr(Expression::new(
                        ExpressionKind::Match {
                            value: Box::new(value),
                            branches: acc,
                        },
                        span,
                    )));
                }
            }
            Task::ContinueIfCondition {
                span,
                then_branch,
                else_branch,
                then_scope,
                else_scope,
            } => {
                let condition = pop_expr(&mut results);
                tasks.push(Task::ContinueIfThen {
                    span,
                    condition,
                    else_branch,
                    else_scope,
                });
                tasks.push(Task::Expr(*then_branch, then_scope));
            }
            Task::ContinueIfThen {
                span,
                condition,
                else_branch,
                else_scope,
            } => {
                let then_expr = pop_expr(&mut results);
                tasks.push(Task::ContinueIfElse {
                    span,
                    condition,
                    then_expr,
                });
                tasks.push(Task::Expr(*else_branch, else_scope));
            }
            Task::ContinueIfElse {
                span,
                condition,
                then_expr,
            } => {
                let else_expr = pop_expr(&mut results);
                results.push(Value::Expr(
                    ExpressionKind::If {
                        condition: Box::new(condition),
                        then_branch: Box::new(then_expr),
                        else_branch: Box::new(else_expr),
                    }
                    .with_span(span),
                ));
            }
            Task::ContinuePatternStructField {
                span,
                mut iter,
                mut acc,
                field_id,
            } => {
                let (pattern, scope) = pop_pattern(&mut results);
                acc.push((field_id, pattern));
                if let Some((field_id, sub_pattern)) = iter.next() {
                    let child_scope = scope.clone();
                    tasks.push(Task::ContinuePatternStructField {
                        span,
                        iter,
                        acc,
                        field_id,
                    });
                    tasks.push(Task::Pattern(sub_pattern, child_scope));
                } else {
                    results.push(Value::Pattern {
                        pattern: BindingPattern::Struct(acc, span),
                        scope,
                    });
                }
            }
            Task::ContinuePatternEnumVariant {
                span,
                variant,
                payload,
                scope,
            } => {
                let enum_type = pop_expr(&mut results);
                if let Some(payload) = payload {
                    let payload_scope = scope.clone();
                    tasks.push(Task::ContinuePatternEnumVariantPayload {
                        span,
                        variant,
                        enum_type,
                    });
                    tasks.push(Task::Pattern(*payload, payload_scope));
                } else {
                    results.push(Value::Pattern {
                        pattern: BindingPattern::EnumVariant {
                            enum_type: Box::new(enum_type),
                            variant,
                            payload: None,
                            span,
                        },
                        scope,
                    });
                }
            }
            Task::ContinuePatternEnumVariantPayload {
                span,
                variant,
                enum_type,
            } => {
                let (payload, scope) = pop_pattern(&mut results);
                results.push(Value::Pattern {
                    pattern: BindingPattern::EnumVariant {
                        enum_type: Box::new(enum_type),
                        variant,
                        payload: Some(Box::new(payload)),
                        span,
                    },
                    scope,
                });
            }
            Task::ContinuePatternTypeHint { span, ty } => {
                let (inner, scope) = pop_pattern(&mut results);
                tasks.push(Task::ContinuePatternTypeHintExpr {
                    span,
                    inner,
                    scope: scope.clone(),
                });
                tasks.push(Task::Expr(*ty, scope));
            }
            Task::ContinuePatternTypeHintExpr { span, inner, scope } => {
                let ty = pop_expr(&mut results);
                results.push(Value::Pattern {
                    pattern: BindingPattern::TypeHint(Box::new(inner), Box::new(ty), span),
                    scope,
                });
            }
            Task::ContinuePatternAnnotated {
                span,
                pattern,
                scope,
            } => {
                let annotations = pop_annotations(&mut results);
                let pattern_scope = scope.clone();
                tasks.push(Task::ContinuePatternAnnotatedPattern { span, annotations });
                tasks.push(Task::Pattern(*pattern, pattern_scope));
            }
            Task::ContinuePatternAnnotatedPattern { span, annotations } => {
                let (pattern, scope) = pop_pattern(&mut results);
                results.push(Value::Pattern {
                    pattern: BindingPattern::Annotated {
                        annotations,
                        pattern: Box::new(pattern),
                        span,
                    },
                    scope,
                });
            }
            Task::ContinueAnnotationsItem {
                mut iter,
                scope,
                mut acc,
                annotation,
            } => match annotation {
                BindingAnnotation::Mutable(span) => {
                    acc.push(BindingAnnotation::Mutable(span));
                    if let Some(annotation) = iter.next() {
                        tasks.push(Task::ContinueAnnotationsItem {
                            iter,
                            scope,
                            acc,
                            annotation,
                        });
                    } else {
                        results.push(Value::Annotations(acc));
                    }
                }
                BindingAnnotation::Export(expr, span) => {
                    let expr_scope = scope.clone();
                    tasks.push(Task::ContinueAnnotationsExport {
                        iter,
                        scope,
                        acc,
                        span,
                    });
                    tasks.push(Task::Expr(expr, expr_scope));
                }
            },
            Task::ContinueAnnotationsExport {
                mut iter,
                scope,
                mut acc,
                span,
            } => {
                let expr = pop_expr(&mut results);
                acc.push(BindingAnnotation::Export(expr, span));
                if let Some(annotation) = iter.next() {
                    tasks.push(Task::ContinueAnnotationsItem {
                        iter,
                        scope,
                        acc,
                        annotation,
                    });
                } else {
                    results.push(Value::Annotations(acc));
                }
            }
        }
    }

    match results.pop() {
        Some(Value::Expr(expr)) => expr,
        _ => panic!("expected final expression result"),
    }
}

pub fn uniquify_program(expr: Expression) -> Expression {
    let scopes = ScopeStack::new();
    uniquify_expression_iter(expr, scopes)
}
