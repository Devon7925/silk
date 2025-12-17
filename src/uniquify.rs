use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use crate::parsing::{
    Binding, BindingAnnotation, BindingPattern, Expression, ExpressionKind, Identifier,
    IntrinsicOperation, LValue,
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

fn uniquify_binding_pattern(pattern: BindingPattern, scopes: &mut ScopeStack) -> BindingPattern {
    match pattern {
        BindingPattern::Identifier(identifier, span) => {
            let fresh = fresh_identifier(&identifier);
            scopes.insert(identifier.name, fresh.clone());
            BindingPattern::Identifier(fresh, span)
        }
        BindingPattern::Literal(_, _) => pattern,
        BindingPattern::Struct(fields, span) => {
            let fields = fields
                .into_iter()
                .map(|(identifier, sub_pattern)| {
                    let inner_pattern = uniquify_binding_pattern(sub_pattern, scopes);
                    (identifier, inner_pattern)
                })
                .collect();
            BindingPattern::Struct(fields, span)
        }
        BindingPattern::EnumVariant {
            enum_type,
            variant,
            payload,
            span,
        } => BindingPattern::EnumVariant {
            enum_type: Box::new(uniquify_expression(*enum_type, scopes)),
            variant,
            payload: payload.map(|p| Box::new(uniquify_binding_pattern(*p, scopes))),
            span,
        },
        BindingPattern::TypeHint(inner, ty, span) => BindingPattern::TypeHint(
            Box::new(uniquify_binding_pattern(*inner, scopes)),
            Box::new(uniquify_expression(*ty, scopes)),
            span,
        ),
        BindingPattern::Annotated {
            annotations,
            pattern,
            span,
        } => BindingPattern::Annotated {
            annotations: uniquify_annotations(annotations, scopes),
            pattern: Box::new(uniquify_binding_pattern(*pattern, scopes)),
            span,
        },
    }
}

fn uniquify_lvalue(lvalue: LValue, scopes: &mut ScopeStack) -> LValue {
    match lvalue {
        LValue::Identifier(identifier, span) => {
            LValue::Identifier(scopes.resolve(&identifier), span)
        }
        LValue::PropertyAccess {
            object,
            property,
            span,
        } => LValue::PropertyAccess {
            object: Box::new(uniquify_lvalue(*object, scopes)),
            property,
            span,
        },
    }
}

fn uniquify_annotations(
    annotations: Vec<BindingAnnotation>,
    scopes: &mut ScopeStack,
) -> Vec<BindingAnnotation> {
    annotations
        .into_iter()
        .map(|annotation| match annotation {
            BindingAnnotation::Export(expr, span) => {
                BindingAnnotation::Export(uniquify_expression(expr, scopes), span)
            }
            BindingAnnotation::Mutable(span) => BindingAnnotation::Mutable(span),
        })
        .collect()
}

fn uniquify_expression(expr: Expression, scopes: &mut ScopeStack) -> Expression {
    let span = expr.span;
    match expr.kind {
        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(left, right, op)) => {
            ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(
                Box::new(uniquify_expression(*left, scopes)),
                Box::new(uniquify_expression(*right, scopes)),
                op,
            ))
            .with_span(span)
        }
        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(operand, op)) => {
            ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(
                Box::new(uniquify_expression(*operand, scopes)),
                op,
            ))
            .with_span(span)
        }
        ExpressionKind::EnumType(variants) => ExpressionKind::EnumType(
            variants
                .into_iter()
                .map(|(id, ty)| (id, uniquify_expression(ty, scopes)))
                .collect(),
        )
        .with_span(span),
        ExpressionKind::Match { value, branches } => {
            let value = Box::new(uniquify_expression(*value, scopes));
            let branches = branches
                .into_iter()
                .map(|(pattern, branch_expr)| {
                    let mut branch_scopes = scopes.clone();
                    branch_scopes.push();
                    let pattern = uniquify_binding_pattern(pattern, &mut branch_scopes);
                    let branch_expr = uniquify_expression(branch_expr, &mut branch_scopes);
                    (pattern, branch_expr)
                })
                .collect();
            Expression::new(ExpressionKind::Match { value, branches }, span)
        }
        ExpressionKind::EnumValue {
            enum_type,
            variant,
            variant_index,
            payload,
        } => ExpressionKind::EnumValue {
            enum_type: Box::new(uniquify_expression(*enum_type, scopes)),
            variant,
            variant_index,
            payload: payload.map(|p| Box::new(uniquify_expression(*p, scopes))),
        }
        .with_span(span),
        ExpressionKind::EnumConstructor {
            enum_type,
            variant,
            variant_index,
            payload_type,
        } => ExpressionKind::EnumConstructor {
            enum_type: Box::new(uniquify_expression(*enum_type, scopes)),
            variant,
            variant_index,
            payload_type: Box::new(uniquify_expression(*payload_type, scopes)),
        }
        .with_span(span),
        ExpressionKind::EnumAccess { enum_expr, variant } => ExpressionKind::EnumAccess {
            enum_expr: Box::new(uniquify_expression(*enum_expr, scopes)),
            variant,
        }
        .with_span(span),
        ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let mut then_scopes = scopes.clone();
            then_scopes.push();
            let mut else_scopes = scopes.clone();
            else_scopes.push();
            ExpressionKind::If {
                condition: Box::new(uniquify_expression(*condition, scopes)),
                then_branch: Box::new(uniquify_expression(*then_branch, &mut then_scopes)),
                else_branch: else_branch
                    .map(|branch| Box::new(uniquify_expression(*branch, &mut else_scopes))),
            }
            .with_span(span)
        }
        ExpressionKind::AttachImplementation {
            type_expr,
            implementation,
        } => ExpressionKind::AttachImplementation {
            type_expr: Box::new(uniquify_expression(*type_expr, scopes)),
            implementation: Box::new(uniquify_expression(*implementation, scopes)),
        }
        .with_span(span),
        ExpressionKind::Function {
            parameter,
            return_type,
            body,
        } => {
            let mut function_scopes = scopes.clone();
            function_scopes.push();
            let parameter = uniquify_binding_pattern(parameter, &mut function_scopes);
            let return_type =
                return_type.map(|rt| Box::new(uniquify_expression(*rt, &mut function_scopes)));
            let body = uniquify_expression(*body, &mut function_scopes);
            ExpressionKind::Function {
                parameter,
                return_type,
                body: Box::new(body),
            }
            .with_span(span)
        }
        ExpressionKind::FunctionType {
            parameter,
            return_type,
        } => {
            let mut function_type_scopes = scopes.clone();
            function_type_scopes.push();
            ExpressionKind::FunctionType {
                parameter: Box::new(uniquify_expression(*parameter, &mut function_type_scopes)),
                return_type: Box::new(uniquify_expression(*return_type, &mut function_type_scopes)),
            }
            .with_span(span)
        }
        ExpressionKind::Struct(fields) => ExpressionKind::Struct(
            fields
                .into_iter()
                .map(|(id, expr)| (id, uniquify_expression(expr, scopes)))
                .collect(),
        )
        .with_span(span),
        ExpressionKind::IntrinsicType(_) | ExpressionKind::Literal(_) => expr,
        ExpressionKind::Identifier(identifier) => {
            ExpressionKind::Identifier(scopes.resolve(&identifier)).with_span(span)
        }
        ExpressionKind::Operation {
            operator,
            left,
            right,
        } => ExpressionKind::Operation {
            operator,
            left: Box::new(uniquify_expression(*left, scopes)),
            right: Box::new(uniquify_expression(*right, scopes)),
        }
        .with_span(span),
        ExpressionKind::Assignment { target, expr } => ExpressionKind::Assignment {
            target: uniquify_lvalue(target, scopes),
            expr: Box::new(uniquify_expression(*expr, scopes)),
        }
        .with_span(span),
        ExpressionKind::FunctionCall { function, argument } => ExpressionKind::FunctionCall {
            function: Box::new(uniquify_expression(*function, scopes)),
            argument: Box::new(uniquify_expression(*argument, scopes)),
        }
        .with_span(span),
        ExpressionKind::PropertyAccess { object, property } => ExpressionKind::PropertyAccess {
            object: Box::new(uniquify_expression(*object, scopes)),
            property,
        }
        .with_span(span),
        ExpressionKind::Binding(binding) => {
            let mut binding_scopes = scopes.clone();
            binding_scopes.push();
            let pattern = uniquify_binding_pattern(binding.pattern, &mut binding_scopes);
            let expr = uniquify_expression(binding.expr, &mut binding_scopes);
            ExpressionKind::Binding(Box::new(Binding { pattern, expr })).with_span(span)
        }
        ExpressionKind::Block(expressions) => {
            let mut block_scopes = scopes.clone();
            block_scopes.push();
            Expression::new(ExpressionKind::Block(expressions), span)
        }
        ExpressionKind::Diverge {
            value,
            divergance_type,
        } => ExpressionKind::Diverge {
            value: value.map(|v| Box::new(uniquify_expression(*v, scopes))),
            divergance_type,
        }
        .with_span(span),
        ExpressionKind::Loop { body } => {
            let mut loop_scopes = scopes.clone();
            loop_scopes.push();
            ExpressionKind::Loop {
                body: Box::new(uniquify_expression(*body, &mut loop_scopes)),
            }
            .with_span(span)
        }
    }
}

pub fn uniquify_program(expr: Expression) -> Expression {
    let mut scopes = ScopeStack::new();
    uniquify_expression(expr, &mut scopes)
}
