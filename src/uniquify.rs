use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use crate::parsing::{
    Binding, BindingAnnotation, BindingPattern, Expression, Identifier, IntrinsicOperation, LValue,
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
    match expr {
        Expression::IntrinsicOperation(IntrinsicOperation::Binary(left, right, op), span) => {
            Expression::IntrinsicOperation(
                IntrinsicOperation::Binary(
                    Box::new(uniquify_expression(*left, scopes)),
                    Box::new(uniquify_expression(*right, scopes)),
                    op,
                ),
                span,
            )
        }
        Expression::IntrinsicOperation(IntrinsicOperation::Unary(operand, op), span) => {
            Expression::IntrinsicOperation(
                IntrinsicOperation::Unary(Box::new(uniquify_expression(*operand, scopes)), op),
                span,
            )
        }
        Expression::EnumType(variants, span) => Expression::EnumType(
            variants
                .into_iter()
                .map(|(id, ty)| (id, uniquify_expression(ty, scopes)))
                .collect(),
            span,
        ),
        Expression::Match {
            value,
            branches,
            span,
        } => {
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
            Expression::Match {
                value,
                branches,
                span,
            }
        }
        Expression::EnumValue {
            enum_type,
            variant,
            variant_index,
            payload,
            span,
        } => Expression::EnumValue {
            enum_type: Box::new(uniquify_expression(*enum_type, scopes)),
            variant,
            variant_index,
            payload: payload.map(|p| Box::new(uniquify_expression(*p, scopes))),
            span,
        },
        Expression::EnumConstructor {
            enum_type,
            variant,
            variant_index,
            payload_type,
            span,
        } => Expression::EnumConstructor {
            enum_type: Box::new(uniquify_expression(*enum_type, scopes)),
            variant,
            variant_index,
            payload_type: Box::new(uniquify_expression(*payload_type, scopes)),
            span,
        },
        Expression::EnumAccess {
            enum_expr,
            variant,
            span,
        } => Expression::EnumAccess {
            enum_expr: Box::new(uniquify_expression(*enum_expr, scopes)),
            variant,
            span,
        },
        Expression::If {
            condition,
            then_branch,
            else_branch,
            span,
        } => {
            let condition = Box::new(uniquify_expression(*condition, scopes));
            let mut then_scopes = scopes.clone();
            then_scopes.push();
            let then_branch = Box::new(uniquify_expression(*then_branch, &mut then_scopes));
            let else_branch = else_branch.map(|branch| {
                let mut else_scopes = scopes.clone();
                else_scopes.push();
                Box::new(uniquify_expression(*branch, &mut else_scopes))
            });
            Expression::If {
                condition,
                then_branch,
                else_branch,
                span,
            }
        }
        Expression::AttachImplementation {
            type_expr,
            implementation,
            span,
        } => Expression::AttachImplementation {
            type_expr: Box::new(uniquify_expression(*type_expr, scopes)),
            implementation: Box::new(uniquify_expression(*implementation, scopes)),
            span,
        },
        Expression::Function {
            parameter,
            return_type,
            body,
            span,
        } => {
            let mut inner_scopes = scopes.clone();
            inner_scopes.push();
            let parameter = uniquify_binding_pattern(parameter, &mut inner_scopes);
            let return_type =
                return_type.map(|ret| Box::new(uniquify_expression(*ret, &mut inner_scopes)));
            let body = Box::new(uniquify_expression(*body, &mut inner_scopes));
            Expression::Function {
                parameter,
                return_type,
                body,
                span,
            }
        }
        Expression::FunctionType {
            parameter,
            return_type,
            span,
        } => Expression::FunctionType {
            parameter: Box::new(uniquify_expression(*parameter, scopes)),
            return_type: Box::new(uniquify_expression(*return_type, scopes)),
            span,
        },
        Expression::Struct(fields, span) => Expression::Struct(
            fields
                .into_iter()
                .map(|(id, value)| (id, uniquify_expression(value, scopes)))
                .collect(),
            span,
        ),
        Expression::IntrinsicType(_, _) => expr,
        Expression::Literal(_, _) => expr,
        Expression::Identifier(identifier, span) => {
            Expression::Identifier(scopes.resolve(&identifier), span)
        }
        Expression::Operation {
            operator,
            left,
            right,
            span,
        } => Expression::Operation {
            operator,
            left: Box::new(uniquify_expression(*left, scopes)),
            right: Box::new(uniquify_expression(*right, scopes)),
            span,
        },
        Expression::Assignment { target, expr, span } => Expression::Assignment {
            target: uniquify_lvalue(target, scopes),
            expr: Box::new(uniquify_expression(*expr, scopes)),
            span,
        },
        Expression::FunctionCall {
            function,
            argument,
            span,
        } => Expression::FunctionCall {
            function: Box::new(uniquify_expression(*function, scopes)),
            argument: Box::new(uniquify_expression(*argument, scopes)),
            span,
        },
        Expression::PropertyAccess {
            object,
            property,
            span,
        } => Expression::PropertyAccess {
            object: Box::new(uniquify_expression(*object, scopes)),
            property,
            span,
        },
        Expression::Binding(binding, span) => {
            let expr = uniquify_expression(binding.expr, scopes);
            let pattern = uniquify_binding_pattern(binding.pattern, scopes);
            Expression::Binding(Box::new(Binding { pattern, expr }), span)
        }
        Expression::Block(expressions, span) => {
            let mut inner_scopes = scopes.clone();
            inner_scopes.push();
            let expressions = expressions
                .into_iter()
                .map(|expr| uniquify_expression(expr, &mut inner_scopes))
                .collect();
            Expression::Block(expressions, span)
        }
        Expression::Diverge {
            value,
            divergance_type,
            span,
        } => Expression::Diverge {
            value: value.map(|v| Box::new(uniquify_expression(*v, scopes))),
            divergance_type,
            span,
        },
        Expression::Loop { body, span } => {
            let mut loop_scopes = scopes.clone();
            loop_scopes.push();
            Expression::Loop {
                body: Box::new(uniquify_expression(*body, &mut loop_scopes)),
                span,
            }
        }
    }
}

pub fn uniquify_program(expr: Expression) -> Expression {
    let mut scopes = ScopeStack::new();
    uniquify_expression(expr, &mut scopes)
}
