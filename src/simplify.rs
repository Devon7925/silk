use crate::interpret::Context;
use crate::{
    Diagnostic,
    enum_normalization::normalize_enum_application,
    interpret::BindingContext,
    parsing::{BinaryIntrinsicOperator, Binding, BindingPattern, Expression, IntrinsicOperation},
};
use std::collections::{HashMap, HashSet, VecDeque};

#[cfg(test)]
use crate::parsing::{BindingAnnotation, ExpressionLiteral, TargetLiteral};

pub fn simplify_expression(expr: Expression) -> Result<Expression, Diagnostic> {
    match normalize_enum_application(expr) {
        Expression::IntrinsicOperation(
            IntrinsicOperation::Binary(left, right, op),
            source_span,
        ) => Ok(Expression::IntrinsicOperation(
            IntrinsicOperation::Binary(
                Box::new(simplify_expression(*left)?),
                Box::new(simplify_expression(*right)?),
                op,
            ),
            source_span,
        )),
        Expression::IntrinsicOperation(IntrinsicOperation::EnumFromStruct, source_span) => Ok(
            Expression::IntrinsicOperation(IntrinsicOperation::EnumFromStruct, source_span),
        ),
        Expression::AttachImplementation { type_expr, .. } => simplify_expression(*type_expr),
        Expression::If {
            condition,
            then_branch,
            else_branch,
            span,
        } => Ok(Expression::If {
            condition: Box::new(simplify_expression(*condition)?),
            then_branch: Box::new(simplify_expression(*then_branch)?),
            else_branch: match else_branch {
                Some(branch) => Some(Box::new(simplify_expression(*branch)?)),
                None => None,
            },
            span,
        }),
        Expression::Match {
            value,
            branches,
            span,
        } => Ok(Expression::Match {
            value: Box::new(simplify_expression(*value)?),
            branches: branches
                .into_iter()
                .map(|(pattern, expr)| {
                    Ok((
                        simplify_binding_pattern(pattern)?,
                        simplify_expression(expr)?,
                    ))
                })
                .collect::<Result<Vec<_>, Diagnostic>>()?,
            span,
        }),
        Expression::Function {
            parameter,
            return_type,
            body,
            span,
        } => Ok(Expression::Function {
            parameter: simplify_binding_pattern(parameter)?,
            return_type: Some(Box::new(simplify_expression(*return_type.unwrap())?)),
            body: Box::new(simplify_expression(*body)?),
            span,
        }),
        Expression::FunctionType {
            parameter,
            return_type,
            span,
        } => Ok(Expression::FunctionType {
            parameter: Box::new(simplify_expression(*parameter)?),
            return_type: Box::new(simplify_expression(*return_type)?),
            span,
        }),
        Expression::While {
            condition,
            body,
            span,
        } => Ok(Expression::Loop {
            body: Box::new(Expression::If {
                condition: Box::new(simplify_expression(*condition)?),
                then_branch: Box::new(simplify_expression(*body)?),
                else_branch: Some(Box::new(Expression::Diverge {
                    value: None,
                    divergance_type: crate::parsing::DivergeExpressionType::Break,
                    span,
                })),
                span,
            }),
            span,
        }),
        Expression::Loop { body, span } => Ok(Expression::Loop {
            body: Box::new(simplify_expression(*body)?),
            span,
        }),
        Expression::Struct(items, source_span) => {
            let simplified_items = items
                .into_iter()
                .map(|(id, expr)| Ok((id, simplify_expression(expr)?)))
                .collect::<Result<_, Diagnostic>>()?;
            Ok(Expression::Struct(simplified_items, source_span))
        }
        Expression::Operation {
            operator,
            left,
            right,
            span,
        } => {
            if let Some(intrinsic) = intrinsic_operator(&operator) {
                Ok(Expression::IntrinsicOperation(
                    IntrinsicOperation::Binary(
                        Box::new(simplify_expression(*left)?),
                        Box::new(simplify_expression(*right)?),
                        intrinsic,
                    ),
                    span,
                ))
            } else {
                Ok(Expression::FunctionCall {
                    function: Box::new(Expression::PropertyAccess {
                        object: Box::new(simplify_expression(*left)?),
                        property: operator,
                        span,
                    }),
                    argument: Box::new(simplify_expression(*right)?),
                    span,
                })
            }
        }
        Expression::FunctionCall {
            function,
            argument,
            span,
        } => Ok(Expression::FunctionCall {
            function: Box::new(simplify_expression(*function)?),
            argument: Box::new(simplify_expression(*argument)?),
            span,
        }),
        Expression::PropertyAccess {
            object,
            property,
            span,
        } => Ok(Expression::PropertyAccess {
            object: Box::new(simplify_expression(*object)?),
            property,
            span,
        }),
        Expression::Diverge {
            value,
            divergance_type,
            span,
        } => Ok(Expression::Diverge {
            value: match value {
                Some(expr) => Some(Box::new(simplify_expression(*expr)?)),
                None => None,
            },
            divergance_type,
            span,
        }),
        Expression::EnumType(variants, span) => Ok(Expression::EnumType(
            variants
                .into_iter()
                .map(|(id, ty)| Ok((id, simplify_expression(ty)?)))
                .collect::<Result<_, Diagnostic>>()?,
            span,
        )),
        Expression::EnumAccess {
            enum_expr,
            variant,
            span,
        } => {
            let simplified_enum = simplify_expression(*enum_expr)?;
            if let Expression::EnumType(variants, _) = &simplified_enum
                && let Some((variant_index, (_id, payload_type))) = variants
                    .iter()
                    .enumerate()
                    .find(|(_, (id, _))| id.0 == variant.0)
            {
                if let Expression::Struct(fields, _) = payload_type
                    && fields.is_empty()
                {
                    return Ok(Expression::EnumValue {
                        enum_type: Box::new(simplified_enum.clone()),
                        variant,
                        variant_index,
                        payload: None,
                        span,
                    });
                }

                return Ok(Expression::EnumConstructor {
                    enum_type: Box::new(simplified_enum.clone()),
                    variant,
                    variant_index,
                    payload_type: Box::new(payload_type.clone()),
                    span,
                });
            }

            Ok(Expression::EnumAccess {
                enum_expr: Box::new(simplified_enum),
                variant,
                span,
            })
        }
        Expression::EnumConstructor {
            enum_type,
            variant,
            variant_index,
            payload_type,
            span,
        } => Ok(Expression::EnumConstructor {
            enum_type: Box::new(simplify_expression(*enum_type)?),
            variant,
            variant_index,
            payload_type: Box::new(simplify_expression(*payload_type)?),
            span,
        }),
        Expression::EnumValue {
            enum_type,
            variant,
            variant_index,
            payload,
            span,
        } => Ok(Expression::EnumValue {
            enum_type: Box::new(simplify_expression(*enum_type)?),
            variant,
            variant_index,
            payload: match payload {
                Some(payload) => Some(Box::new(simplify_expression(*payload)?)),
                None => None,
            },
            span,
        }),
        Expression::Assignment { target, expr, span } => Ok(Expression::Assignment {
            target,
            expr: Box::new(simplify_expression(*expr)?),
            span,
        }),
        Expression::Binding(binding, source_span) => {
            let binding = Binding {
                pattern: simplify_binding_pattern(binding.pattern)?,
                ..*binding
            };
            Ok(Expression::Binding(Box::new(binding), source_span))
        }
        Expression::Block(expressions, source_span) => {
            let simplified_exprs = expressions
                .into_iter()
                .map(simplify_expression)
                .collect::<Result<_, Diagnostic>>()?;
            Ok(Expression::Block(simplified_exprs, source_span))
        }
        expr @ (Expression::Identifier(..)
        | Expression::IntrinsicType(..)
        | Expression::Literal(..)) => Ok(expr),
    }
}

fn intrinsic_operator(operator: &str) -> Option<BinaryIntrinsicOperator> {
    match operator {
        "+" => Some(BinaryIntrinsicOperator::I32Add),
        "-" => Some(BinaryIntrinsicOperator::I32Subtract),
        "*" => Some(BinaryIntrinsicOperator::I32Multiply),
        "/" => Some(BinaryIntrinsicOperator::I32Divide),
        "==" => Some(BinaryIntrinsicOperator::I32Equal),
        "!=" => Some(BinaryIntrinsicOperator::I32NotEqual),
        "<" => Some(BinaryIntrinsicOperator::I32LessThan),
        ">" => Some(BinaryIntrinsicOperator::I32GreaterThan),
        "<=" => Some(BinaryIntrinsicOperator::I32LessThanOrEqual),
        ">=" => Some(BinaryIntrinsicOperator::I32GreaterThanOrEqual),
        "&&" => Some(BinaryIntrinsicOperator::BooleanAnd),
        "||" => Some(BinaryIntrinsicOperator::BooleanOr),
        "^" => Some(BinaryIntrinsicOperator::BooleanXor),
        _ => None,
    }
}

fn simplify_binding_pattern(pattern: BindingPattern) -> Result<BindingPattern, Diagnostic> {
    match pattern {
        pat @ BindingPattern::Identifier(..) => Ok(pat),
        pat @ BindingPattern::Literal(..) => Ok(pat),
        BindingPattern::Struct(items, source_span) => {
            let simplified_items = items
                .into_iter()
                .map(|(id, pat)| Ok((id, simplify_binding_pattern(pat)?)))
                .collect::<Result<_, Diagnostic>>()?;
            Ok(BindingPattern::Struct(simplified_items, source_span))
        }
        BindingPattern::TypeHint(binding_pattern, expression, source_span) => {
            Ok(BindingPattern::TypeHint(
                Box::new(simplify_binding_pattern(*binding_pattern)?),
                Box::new(simplify_expression(*expression)?),
                source_span,
            ))
        }
        BindingPattern::EnumVariant {
            enum_type,
            variant,
            payload,
            span,
        } => Ok(BindingPattern::EnumVariant {
            enum_type: Box::new(simplify_expression(*enum_type)?),
            variant,
            payload: match payload {
                Some(payload) => Some(Box::new(simplify_binding_pattern(*payload)?)),
                None => None,
            },
            span,
        }),
        BindingPattern::Annotated {
            annotations,
            pattern,
            span,
        } => Ok(BindingPattern::Annotated {
            annotations,
            pattern: Box::new(simplify_binding_pattern(*pattern)?),
            span,
        }),
    }
}

fn simplify_binding_context(binding_context: BindingContext) -> Result<BindingContext, Diagnostic> {
    match binding_context {
        BindingContext::Bound(expression, preserve_behavior) => Ok(BindingContext::Bound(
            simplify_expression(expression)?,
            preserve_behavior,
        )),
        BindingContext::UnboundWithType(expression) => Ok(BindingContext::UnboundWithType(
            simplify_expression(expression)?,
        )),
        BindingContext::UnboundWithoutType => Ok(BindingContext::UnboundWithoutType),
    }
}

fn simplify_binding_annotation(
    annotation: crate::parsing::BindingAnnotation,
) -> Result<crate::parsing::BindingAnnotation, Diagnostic> {
    match annotation {
        crate::parsing::BindingAnnotation::Export(expr, span) => Ok(
            crate::parsing::BindingAnnotation::Export(simplify_expression(expr)?, span),
        ),
        crate::parsing::BindingAnnotation::Mutable(span) => {
            Ok(crate::parsing::BindingAnnotation::Mutable(span))
        }
    }
}

fn collect_pattern_bindings(pattern: &BindingPattern, bindings: &mut HashSet<String>) {
    match pattern {
        BindingPattern::Identifier(identifier, _) => {
            bindings.insert(identifier.0.clone());
        }
        BindingPattern::Struct(items, _) => {
            for (_id, pat) in items {
                collect_pattern_bindings(pat, bindings);
            }
        }
        BindingPattern::EnumVariant { payload, .. } => {
            if let Some(payload) = payload {
                collect_pattern_bindings(payload, bindings);
            }
        }
        BindingPattern::TypeHint(inner, _, _) => collect_pattern_bindings(inner, bindings),
        BindingPattern::Annotated { pattern, .. } => collect_pattern_bindings(pattern, bindings),
        BindingPattern::Literal(..) => {}
    }
}

fn collect_pattern_dependencies(
    pattern: &BindingPattern,
    bound: &HashSet<String>,
    free: &mut HashSet<String>,
) {
    match pattern {
        BindingPattern::TypeHint(inner, ty_expr, _) => {
            collect_free_identifiers(ty_expr, bound, free);
            collect_pattern_dependencies(inner, bound, free);
        }
        BindingPattern::Annotated {
            annotations,
            pattern,
            ..
        } => {
            for annotation in annotations {
                if let crate::parsing::BindingAnnotation::Export(expr, _) = annotation {
                    collect_free_identifiers(expr, bound, free);
                }
            }
            collect_pattern_dependencies(pattern, bound, free);
        }
        BindingPattern::Struct(items, _) => {
            for (_id, pat) in items {
                collect_pattern_dependencies(pat, bound, free);
            }
        }
        BindingPattern::EnumVariant {
            enum_type, payload, ..
        } => {
            collect_free_identifiers(enum_type, bound, free);
            if let Some(payload) = payload {
                collect_pattern_dependencies(payload, bound, free);
            }
        }
        BindingPattern::Identifier(..) | BindingPattern::Literal(..) => {}
    }
}

fn collect_lvalue_dependencies(
    lvalue: &crate::parsing::LValue,
    bound: &HashSet<String>,
    free: &mut HashSet<String>,
) {
    match lvalue {
        crate::parsing::LValue::Identifier(identifier, _) => {
            if !bound.contains(&identifier.0) {
                free.insert(identifier.0.clone());
            }
        }
        crate::parsing::LValue::PropertyAccess { object, .. } => {
            collect_lvalue_dependencies(object, bound, free);
        }
    }
}

fn collect_free_identifiers(
    expr: &Expression,
    bound: &HashSet<String>,
    free: &mut HashSet<String>,
) {
    match expr {
        Expression::Identifier(identifier, _) => {
            if !bound.contains(&identifier.0) {
                free.insert(identifier.0.clone());
            }
        }
        Expression::Function {
            parameter,
            return_type,
            body,
            ..
        } => {
            collect_pattern_dependencies(parameter, bound, free);
            let mut inner_bound = bound.clone();
            collect_pattern_bindings(parameter, &mut inner_bound);
            if let Some(return_type) = return_type {
                collect_free_identifiers(return_type, &inner_bound, free);
            }
            collect_free_identifiers(body, &inner_bound, free);
        }
        Expression::FunctionType {
            parameter,
            return_type,
            ..
        } => {
            collect_free_identifiers(parameter, bound, free);
            collect_free_identifiers(return_type, bound, free);
        }
        Expression::Struct(fields, _) => {
            for (_id, value) in fields {
                collect_free_identifiers(value, bound, free);
            }
        }
        Expression::IntrinsicOperation(intrinsic, _) => match intrinsic {
            IntrinsicOperation::Binary(left, right, _) => {
                collect_free_identifiers(left, bound, free);
                collect_free_identifiers(right, bound, free);
            }
            IntrinsicOperation::EnumFromStruct => {}
        },
        Expression::EnumType(variants, _) => {
            for (_id, ty) in variants {
                collect_free_identifiers(ty, bound, free);
            }
        }
        Expression::EnumAccess { enum_expr, .. } => {
            collect_free_identifiers(enum_expr, bound, free)
        }
        Expression::EnumConstructor {
            enum_type,
            payload_type,
            ..
        } => {
            collect_free_identifiers(enum_type, bound, free);
            collect_free_identifiers(payload_type, bound, free);
        }
        Expression::EnumValue {
            enum_type, payload, ..
        } => {
            collect_free_identifiers(enum_type, bound, free);
            if let Some(payload) = payload {
                collect_free_identifiers(payload, bound, free);
            }
        }
        Expression::Match {
            value, branches, ..
        } => {
            collect_free_identifiers(value, bound, free);
            for (pattern, branch_expr) in branches {
                collect_pattern_dependencies(pattern, bound, free);
                let mut inner_bound = bound.clone();
                collect_pattern_bindings(pattern, &mut inner_bound);
                collect_free_identifiers(branch_expr, &inner_bound, free);
            }
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_free_identifiers(condition, bound, free);
            collect_free_identifiers(then_branch, bound, free);
            if let Some(branch) = else_branch {
                collect_free_identifiers(branch, bound, free);
            }
        }
        Expression::Operation { left, right, .. } => {
            collect_free_identifiers(left, bound, free);
            collect_free_identifiers(right, bound, free);
        }
        Expression::FunctionCall {
            function, argument, ..
        } => {
            collect_free_identifiers(function, bound, free);
            collect_free_identifiers(argument, bound, free);
        }
        Expression::PropertyAccess { object, .. } => {
            collect_free_identifiers(object, bound, free);
        }
        Expression::Binding(binding, _) => {
            collect_pattern_dependencies(&binding.pattern, bound, free);
            collect_free_identifiers(&binding.expr, bound, free);
        }
        Expression::Block(expressions, _) => {
            for expr in expressions {
                collect_free_identifiers(expr, bound, free);
            }
        }
        Expression::Diverge { value, .. } => {
            if let Some(value) = value {
                collect_free_identifiers(value, bound, free);
            }
        }
        Expression::Loop { body, .. } => collect_free_identifiers(body, bound, free),
        Expression::While {
            condition, body, ..
        } => {
            collect_free_identifiers(condition, bound, free);
            collect_free_identifiers(body, bound, free);
        }
        Expression::Assignment { target, expr, .. } => {
            collect_lvalue_dependencies(target, bound, free);
            collect_free_identifiers(expr, bound, free);
        }
        Expression::AttachImplementation {
            type_expr,
            implementation,
            ..
        } => {
            collect_free_identifiers(type_expr, bound, free);
            collect_free_identifiers(implementation, bound, free);
        }
        Expression::IntrinsicType(..) | Expression::Literal(..) => {}
    }
}

fn binding_dependencies(binding: &BindingContext) -> HashSet<String> {
    let mut free = HashSet::new();
    match binding {
        BindingContext::Bound(expr, _) => {
            collect_free_identifiers(expr, &HashSet::new(), &mut free)
        }
        BindingContext::UnboundWithType(expr) => {
            collect_free_identifiers(expr, &HashSet::new(), &mut free)
        }
        BindingContext::UnboundWithoutType => {}
    }
    free
}

pub fn simplify_context(context: Context) -> Result<Context, Diagnostic> {
    let mut simplified_bindings: HashMap<
        String,
        (BindingContext, Vec<crate::parsing::BindingAnnotation>),
    > = context
        .bindings
        .into_iter()
        .map(|(bind_name, (binding, annotations))| {
            Ok((
                bind_name,
                (
                    simplify_binding_context(binding)?,
                    annotations
                        .into_iter()
                        .map(simplify_binding_annotation)
                        .collect::<Result<_, Diagnostic>>()?,
                ),
            ))
        })
        .collect::<Result<_, Diagnostic>>()?;

    let dependencies: HashMap<String, HashSet<String>> = simplified_bindings
        .iter()
        .map(|(name, (binding_ctx, _))| (name.clone(), binding_dependencies(binding_ctx)))
        .collect();

    let mut used_bindings: HashSet<String> = simplified_bindings
        .iter()
        .filter_map(|(name, (_, annotations))| {
            annotations
                .iter()
                .any(|ann| matches!(ann, crate::parsing::BindingAnnotation::Export(..)))
                .then(|| name.clone())
        })
        .collect();

    let mut queue: VecDeque<String> = used_bindings.iter().cloned().collect();

    while let Some(current) = queue.pop_front() {
        if let Some(deps) = dependencies.get(&current) {
            for dep in deps {
                if used_bindings.insert(dep.clone()) {
                    queue.push_back(dep.clone());
                }
            }
        }
    }

    simplified_bindings.retain(|name, (ctx, annotations)| {
        if let BindingContext::Bound(_, crate::interpret::PreserveBehavior::Inline) = ctx {
            // Inline bindings may still be required to resolve type-level constructs
            // (e.g. enum aliases) even though their values are substituted at use sites.
            // Keep them when they are part of the dependency graph, and only drop
            // non-exported, truly unused inline bindings.
            return !annotations.is_empty() || used_bindings.contains(name);
        }

        used_bindings.contains(name)
    });

    Ok(Context {
        bindings: simplified_bindings,
    })
}

#[cfg(test)]
fn evaluate_text_to_simplified_expression(
    program: &str,
) -> Result<(Expression, Context), Diagnostic> {
    use crate::interpret::{interpret_program, intrinsic_context};

    let (expression, remaining) =
        crate::parsing::parse_block(program).expect("Failed to parse program text");
    assert!(
        remaining.trim().is_empty(),
        "Parser did not consume entire input, remaining: {remaining:?}"
    );

    let mut context = intrinsic_context();
    let (result, context) = interpret_program(expression, &mut context)?;
    let simplified_expression = simplify_expression(result)?;
    let simplified_context = simplify_context(context)?;
    Ok((simplified_expression, simplified_context))
}

#[test]
fn interpret_exported_function() {
    let program = "
(export js) add_one := (x: i32) => (
    x + 1
);
{}
    ";
    let (_result, context) =
        evaluate_text_to_simplified_expression(&program).expect("interpretation should succeed");
    let annotated_bindings = context.annotated_bindings();
    assert_eq!(annotated_bindings.len(), 1);
    let exported_binding = &annotated_bindings[0];
    assert_eq!(exported_binding.name, "add_one");
    assert!(exported_binding.annotations.len() == 1);
    let target_expr = exported_binding
        .annotations
        .iter()
        .find_map(|ann| match ann {
            BindingAnnotation::Export(expr, _) => Some(expr),
            _ => None,
        })
        .expect("expected export annotation");
    if let Expression::Literal(ExpressionLiteral::Target(TargetLiteral::JSTarget), _) = target_expr
    {
    } else {
        panic!("expected js target in export annotation");
    }
    println!("Exported binding value: {:?}", exported_binding.value);
}

#[test]
fn interpret_exported_function_w_binding() {
    let program = include_str!("../fixtures/binding_in_function.silk");
    let (_result, context) =
        evaluate_text_to_simplified_expression(&program).expect("interpretation should succeed");
    let annotated_bindings = context.annotated_bindings();
    assert_eq!(annotated_bindings.len(), 1);
    let exported_binding = &annotated_bindings[0];
    assert_eq!(exported_binding.name, "add_one_squared");
    assert!(exported_binding.annotations.len() == 1);
    let target_expr = exported_binding
        .annotations
        .iter()
        .find_map(|ann| match ann {
            BindingAnnotation::Export(expr, _) => Some(expr),
            _ => None,
        })
        .expect("expected export annotation");
    if let Expression::Literal(ExpressionLiteral::Target(TargetLiteral::WasmTarget), _) =
        target_expr
    {
    } else {
        panic!("expected wasm target in export annotation");
    }
    println!("Exported binding value: {:?}", exported_binding.value);
}
