use std::collections::{HashMap, HashSet};

use crate::{
    diagnostics::{Diagnostic, SourceSpan},
    loader,
    parsing::{
        BinaryIntrinsicOperator, Binding, BindingAnnotation, BindingPattern, DivergeExpressionType,
        Expression, ExpressionKind, ExpressionLiteral, Identifier, IntrinsicOperation,
        IntrinsicType, LValue, TargetLiteral, UnaryIntrinsicOperator,
    },
    uniquify,
};
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PreserveBehavior {
    PreserveUsage,
    PreserveUsageInLoops,
    PreserveBinding,
    Inline,
}

impl Ord for PreserveBehavior {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use PreserveBehavior::*;
        fn get_rank(behavior: &PreserveBehavior) -> u8 {
            match behavior {
                PreserveUsage => 3,
                PreserveUsageInLoops => 2,
                PreserveBinding => 1,
                Inline => 0,
            }
        }

        get_rank(self).cmp(&get_rank(other))
    }
}

impl PartialOrd for PreserveBehavior {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BindingContext {
    Bound(Expression, PreserveBehavior, Option<Expression>),
    UnboundWithType(Expression),
    UnboundWithoutType,
}

impl BindingContext {
    fn get_bound_type(&self, context: &Context) -> Result<Option<Expression>, Diagnostic> {
        match self {
            BindingContext::Bound(expression, _, bound_type) => {
                if let Some(bound_type) = bound_type {
                    Ok(Some(bound_type.clone()))
                } else {
                    Ok(Some(get_type_of_expression(expression, context)?))
                }
            }
            BindingContext::UnboundWithType(expression) => Ok(Some(expression.clone())),
            BindingContext::UnboundWithoutType => Ok(None),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    pub bindings: Vec<HashMap<Identifier, (BindingContext, Vec<BindingAnnotation>)>>,
    pub in_loop: bool,
    pub files: HashMap<String, Expression>,
    pub import_cache: HashMap<String, Expression>,
}

#[derive(Clone, Debug)]
pub struct AnnotatedBinding {
    pub identifier: Identifier,
    pub annotations: Vec<BindingAnnotation>,
    pub value: Expression,
}

impl Context {
    pub fn annotated_bindings(&self) -> Vec<AnnotatedBinding> {
        self.bindings
            .iter()
            .last()
            .unwrap()
            .iter()
            .filter(|(_, (_, annotations))| !annotations.is_empty())
            .filter_map(|(name, (binding, annotations))| match binding {
                BindingContext::Bound(value, _, _) => Some(AnnotatedBinding {
                    identifier: name.clone(),
                    annotations: annotations.clone(),
                    value: value.clone(),
                }),
                _ => None,
            })
            .collect()
    }

    fn empty() -> Context {
        Context {
            bindings: vec![],
            in_loop: false,
            files: HashMap::new(),
            import_cache: HashMap::new(),
        }
    }

    pub fn get_identifier(
        &self,
        identifier: &Identifier,
    ) -> Option<&(BindingContext, Vec<BindingAnnotation>)> {
        self.bindings
            .iter()
            .rev()
            .find_map(|binding_context| binding_context.get(identifier))
    }

    pub fn get_mut_identifier(
        &mut self,
        identifier: &Identifier,
    ) -> Option<&mut (BindingContext, Vec<BindingAnnotation>)> {
        self.bindings
            .iter_mut()
            .rev()
            .find_map(|binding_context| binding_context.get_mut(identifier))
    }

    fn contains_identifier(&self, identifier: &Identifier) -> bool {
        self.bindings
            .iter()
            .rev()
            .any(|binding_context| binding_context.contains_key(identifier))
    }
}

fn diagnostic(message: impl Into<String>, span: SourceSpan) -> Diagnostic {
    Diagnostic::new(message).with_span(span)
}

fn dummy_span() -> SourceSpan {
    SourceSpan::default()
}

fn identifier_expr(name: &str) -> Expression {
    ExpressionKind::Identifier(Identifier::new(name.to_string())).with_span(dummy_span())
}

fn intrinsic_type_expr(ty: IntrinsicType) -> Expression {
    Expression::new(ExpressionKind::IntrinsicType(ty), dummy_span())
}

fn ensure_boolean_condition(
    condition: &Expression,
    span: SourceSpan,
    context: &Context,
    construct_name: &str,
) -> Result<(), Diagnostic> {
    let condition_type = get_type_of_expression(condition, &context.clone())?;
    let expected_bool = ExpressionKind::IntrinsicType(IntrinsicType::Boolean);

    if !types_equivalent(&condition_type.kind, &expected_bool) {
        return Err(diagnostic(
            format!(
                "{} condition did not resolve to a boolean value",
                construct_name
            ),
            span,
        ));
    }

    Ok(())
}

fn types_equivalent(left: &ExpressionKind, right: &ExpressionKind) -> bool {
    let mut stack = vec![(left, right)];
    while let Some((left, right)) = stack.pop() {
        match (left, right) {
            (ExpressionKind::IntrinsicType(a), ExpressionKind::IntrinsicType(b)) => {
                if a == b {
                    continue;
                }
                if matches!(
                    (a, b),
                    (IntrinsicType::I32, IntrinsicType::U8)
                        | (IntrinsicType::U8, IntrinsicType::I32)
                ) {
                    continue;
                }
                if a != b {
                    return false;
                }
            }
            (ExpressionKind::Identifier(a), ExpressionKind::Identifier(b)) => {
                if a.name != b.name {
                    return false;
                }
            }
            (ExpressionKind::Struct(a_items), ExpressionKind::Struct(b_items)) => {
                if a_items.len() != b_items.len() {
                    return false;
                }
                for ((a_id, a_expr), (b_id, b_expr)) in a_items.iter().zip(b_items.iter()) {
                    if a_id.name != b_id.name {
                        return false;
                    }
                    stack.push((&a_expr.kind, &b_expr.kind));
                }
            }
            (
                ExpressionKind::FunctionType {
                    parameter: a_param,
                    return_type: a_ret,
                },
                ExpressionKind::FunctionType {
                    parameter: b_param,
                    return_type: b_ret,
                },
            ) => {
                stack.push((&a_param.kind, &b_param.kind));
                stack.push((&a_ret.kind, &b_ret.kind));
            }
            (
                ExpressionKind::AttachImplementation {
                    type_expr: a_type, ..
                },
                ExpressionKind::AttachImplementation {
                    type_expr: b_type, ..
                },
            ) => {
                stack.push((&a_type.kind, &b_type.kind));
            }
            (ExpressionKind::AttachImplementation { type_expr, .. }, other) => {
                stack.push((&type_expr.kind, other));
            }
            (other, ExpressionKind::AttachImplementation { type_expr, .. }) => {
                stack.push((other, &type_expr.kind));
            }
            (ExpressionKind::EnumType(a_variants), ExpressionKind::EnumType(b_variants)) => {
                if a_variants.len() != b_variants.len() {
                    return false;
                }
                for ((a_id, a_ty), (b_id, b_ty)) in a_variants.iter().zip(b_variants.iter()) {
                    if a_id.name != b_id.name {
                        return false;
                    }
                    stack.push((&a_ty.kind, &b_ty.kind));
                }
            }
            _ => return false,
        }
    }
    true
}

fn homogeneous_struct_element_type(ty: &Expression, context: &Context) -> Option<Expression> {
    let ExpressionKind::Struct(fields) = &ty.kind else {
        return None;
    };
    let (_, first_type) = fields.first()?;
    let mut base_type = resolve_type_alias_expression(first_type, context);
    if let ExpressionKind::IntrinsicType(intrinsic) = &base_type.kind {
        base_type = intrinsic_type_expr(intrinsic.clone());
    }

    for (_, field_type) in fields.iter().skip(1) {
        let mut resolved = resolve_type_alias_expression(field_type, context);
        if let ExpressionKind::IntrinsicType(intrinsic) = &resolved.kind {
            resolved = intrinsic_type_expr(intrinsic.clone());
        }
        if !types_equivalent(&base_type.kind, &resolved.kind) {
            return None;
        }
    }
    Some(base_type)
}

fn collect_type_bindings(
    pattern_type: &Expression,
    value_type: &Expression,
    bindings: &mut HashMap<String, Expression>,
) -> bool {
    let mut stack = vec![(pattern_type, value_type)];
    while let Some((pattern, value)) = stack.pop() {
        match (&pattern.kind, &value.kind) {
            (ExpressionKind::Identifier(identifier), _) => {
                if let Some(existing) = bindings.get(&identifier.name) {
                    if !types_equivalent(&existing.kind, &value.kind) {
                        return false;
                    }
                } else {
                    bindings.insert(identifier.name.clone(), value.clone());
                }
            }
            (ExpressionKind::Struct(pattern_items), ExpressionKind::Struct(value_items)) => {
                if pattern_items.len() != value_items.len() {
                    return false;
                }
                for ((pattern_id, pattern_expr), (value_id, value_expr)) in
                    pattern_items.iter().zip(value_items.iter())
                {
                    if pattern_id.name != value_id.name {
                        return false;
                    }
                    stack.push((pattern_expr, value_expr));
                }
            }
            (
                ExpressionKind::FunctionType {
                    parameter: pattern_param,
                    return_type: pattern_ret,
                },
                ExpressionKind::FunctionType {
                    parameter: value_param,
                    return_type: value_ret,
                },
            ) => {
                stack.push((pattern_param, value_param));
                stack.push((pattern_ret, value_ret));
            }
            (
                ExpressionKind::EnumType(pattern_variants),
                ExpressionKind::EnumType(value_variants),
            ) => {
                if pattern_variants.len() != value_variants.len() {
                    return false;
                }
                for ((pattern_id, pattern_expr), (value_id, value_expr)) in
                    pattern_variants.iter().zip(value_variants.iter())
                {
                    if pattern_id.name != value_id.name {
                        return false;
                    }
                    stack.push((pattern_expr, value_expr));
                }
            }
            (ExpressionKind::AttachImplementation { type_expr, .. }, _) => {
                stack.push((type_expr, value));
            }
            (_, ExpressionKind::AttachImplementation { type_expr, .. }) => {
                stack.push((pattern, type_expr));
            }
            (ExpressionKind::IntrinsicType(a), ExpressionKind::IntrinsicType(b)) => {
                if a != b {
                    return false;
                }
            }
            _ => {
                if !types_equivalent(&pattern.kind, &value.kind) {
                    return false;
                }
            }
        }
    }
    true
}

fn apply_type_bindings(expr: &Expression, bindings: &HashMap<String, Expression>) -> Expression {
    enum Frame {
        Enter(Expression),
        Struct {
            span: SourceSpan,
            identifiers: Vec<Identifier>,
        },
        EnumType {
            span: SourceSpan,
            identifiers: Vec<Identifier>,
        },
        FunctionType {
            span: SourceSpan,
        },
        ArrayIndex {
            span: SourceSpan,
        },
        AttachImplementation {
            span: SourceSpan,
        },
    }

    let mut stack = vec![Frame::Enter(expr.clone())];
    let mut results: Vec<Expression> = Vec::new();

    while let Some(frame) = stack.pop() {
        match frame {
            Frame::Enter(expr) => match expr.kind {
                ExpressionKind::Identifier(identifier) => {
                    if let Some(replacement) = bindings.get(&identifier.name) {
                        results.push(replacement.clone());
                    } else {
                        results.push(Expression::new(
                            ExpressionKind::Identifier(identifier),
                            expr.span,
                        ));
                    }
                }
                ExpressionKind::Struct(items) => {
                    let identifiers = items.iter().map(|(id, _)| id.clone()).collect();
                    stack.push(Frame::Struct {
                        span: expr.span,
                        identifiers,
                    });
                    for (_, value) in items.into_iter().rev() {
                        stack.push(Frame::Enter(value));
                    }
                }
                ExpressionKind::EnumType(variants) => {
                    let identifiers = variants.iter().map(|(id, _)| id.clone()).collect();
                    stack.push(Frame::EnumType {
                        span: expr.span,
                        identifiers,
                    });
                    for (_, value) in variants.into_iter().rev() {
                        stack.push(Frame::Enter(value));
                    }
                }
                ExpressionKind::FunctionType {
                    parameter,
                    return_type,
                } => {
                    stack.push(Frame::FunctionType { span: expr.span });
                    stack.push(Frame::Enter(*return_type));
                    stack.push(Frame::Enter(*parameter));
                }
                ExpressionKind::ArrayIndex { array, index } => {
                    stack.push(Frame::ArrayIndex { span: expr.span });
                    stack.push(Frame::Enter(*index));
                    stack.push(Frame::Enter(*array));
                }
                ExpressionKind::AttachImplementation {
                    type_expr,
                    implementation,
                } => {
                    stack.push(Frame::AttachImplementation { span: expr.span });
                    stack.push(Frame::Enter(*implementation));
                    stack.push(Frame::Enter(*type_expr));
                }
                other => {
                    results.push(Expression::new(other, expr.span));
                }
            },
            Frame::Struct { span, identifiers } => {
                let mut evaluated = Vec::with_capacity(identifiers.len());
                for _ in 0..identifiers.len() {
                    evaluated.push(results.pop().unwrap());
                }
                evaluated.reverse();
                let items = identifiers.into_iter().zip(evaluated.into_iter()).collect();
                results.push(Expression::new(ExpressionKind::Struct(items), span));
            }
            Frame::EnumType { span, identifiers } => {
                let mut evaluated = Vec::with_capacity(identifiers.len());
                for _ in 0..identifiers.len() {
                    evaluated.push(results.pop().unwrap());
                }
                evaluated.reverse();
                let variants = identifiers.into_iter().zip(evaluated.into_iter()).collect();
                results.push(Expression::new(ExpressionKind::EnumType(variants), span));
            }
            Frame::FunctionType { span } => {
                let return_type = results.pop().unwrap();
                let parameter = results.pop().unwrap();
                results.push(
                    ExpressionKind::FunctionType {
                        parameter: Box::new(parameter),
                        return_type: Box::new(return_type),
                    }
                    .with_span(span),
                );
            }
            Frame::ArrayIndex { span } => {
                let index = results.pop().unwrap();
                let array = results.pop().unwrap();
                results.push(
                    ExpressionKind::ArrayIndex {
                        array: Box::new(array),
                        index: Box::new(index),
                    }
                    .with_span(span),
                );
            }
            Frame::AttachImplementation { span } => {
                let implementation = results.pop().unwrap();
                let type_expr = results.pop().unwrap();
                results.push(
                    ExpressionKind::AttachImplementation {
                        type_expr: Box::new(type_expr),
                        implementation: Box::new(implementation),
                    }
                    .with_span(span),
                );
            }
        }
    }

    results
        .pop()
        .unwrap_or_else(|| Expression::new(expr.kind.clone(), expr.span))
}

#[cfg(test)]
fn literal_number_expr(value: i32) -> Expression {
    ExpressionKind::Literal(ExpressionLiteral::Number(value)).with_span(dummy_span())
}

fn is_type_expression(expr: &ExpressionKind) -> bool {
    let mut stack = vec![expr];
    while let Some(expr) = stack.pop() {
        match expr {
            ExpressionKind::IntrinsicType(_)
            | ExpressionKind::AttachImplementation { .. }
            | ExpressionKind::EnumType(_)
            | ExpressionKind::FunctionType { .. }
            | ExpressionKind::Identifier(_) => {}
            ExpressionKind::Struct(items) => {
                for (_, ty) in items.iter() {
                    stack.push(&ty.kind);
                }
            }
            _ => return false,
        }
    }
    true
}

fn enum_variant_info(enum_type: &Expression, variant: &Identifier) -> Option<(usize, Expression)> {
    if let ExpressionKind::EnumType(variants) = &enum_type.kind {
        variants
            .iter()
            .enumerate()
            .find(|(_, (id, _))| id.name == variant.name)
            .map(|(idx, (_, ty))| (idx, ty.clone()))
    } else {
        None
    }
}

pub fn resolve_enum_type_expression(
    enum_expr: &Expression,
    context: &mut Context,
) -> Option<Expression> {
    match &enum_expr.kind {
        ExpressionKind::EnumType(_) => Some(enum_expr.clone()),
        ExpressionKind::AttachImplementation { type_expr, .. } => {
            resolve_enum_type_expression(type_expr, context)
        }
        ExpressionKind::Identifier(identifier) => {
            context
                .get_identifier(identifier)
                .and_then(|(binding, _)| match binding {
                    BindingContext::Bound(value, _, _) => Some(value.clone()),
                    BindingContext::UnboundWithType(type_expr) => Some(type_expr.clone()),
                    BindingContext::UnboundWithoutType => None,
                })
        }
        _ => None,
    }
}

fn collect_bindings(expr: &Expression, context: &mut Context) -> Result<(), Diagnostic> {
    let mut stack = vec![expr];
    while let Some(expr) = stack.pop() {
        match &expr.kind {
            ExpressionKind::Binding(binding) => {
                let value_type = get_type_of_expression(&binding.expr, context).ok();

                bind_pattern_blanks(binding.pattern.clone(), context, Vec::new(), value_type)?;
            }
            ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(
                left,
                right,
                BinaryIntrinsicOperator::BooleanAnd,
            )) => {
                stack.push(right);
                stack.push(left);
            }
            _ => {}
        }
    }
    Ok(())
}

pub fn interpret_expression(
    expr: Expression,
    context: &mut Context,
) -> Result<Expression, Diagnostic> {
    enum Frame {
        Eval(Expression),
        PatternStart(BindingPattern),
        PatternStruct {
            span: SourceSpan,
            identifiers: Vec<Identifier>,
        },
        PatternEnumVariant {
            variant: Identifier,
            span: SourceSpan,
            has_payload: bool,
        },
        PatternTypeHint {
            span: SourceSpan,
        },
        PatternAnnotated {
            annotations: Vec<BindingAnnotation>,
            span: SourceSpan,
            export_indices: Vec<usize>,
        },
        BindingFinish,
        FunctionStart {
            span: SourceSpan,
            body: Expression,
        },
        BlockAfterExpr {
            span: SourceSpan,
            expressions: Vec<Expression>,
            outer_context: Context,
            index: usize,
            interpreted_expressions: Vec<Expression>,
            preserved_indices: HashSet<usize>,
        },
        EnumType {
            span: SourceSpan,
            identifiers: Vec<Identifier>,
        },
        Struct {
            span: SourceSpan,
            identifiers: Vec<Identifier>,
        },
        FunctionType {
            span: SourceSpan,
        },
        EnumValue {
            span: SourceSpan,
            variant: Identifier,
            variant_index: usize,
        },
        EnumConstructor {
            span: SourceSpan,
            variant: Identifier,
            variant_index: usize,
        },
        AttachImplementation {
            span: SourceSpan,
        },
        IntrinsicBinary {
            span: SourceSpan,
            operator: BinaryIntrinsicOperator,
        },
        IntrinsicUnary {
            span: SourceSpan,
            operator: UnaryIntrinsicOperator,
        },
        Diverge {
            span: SourceSpan,
            divergance_type: DivergeExpressionType,
        },
        Assignment {
            span: SourceSpan,
            target: LValue,
        },
        PropertyAccess {
            span: SourceSpan,
            property: String,
            original_object: Expression,
        },
        ArrayIndex {
            span: SourceSpan,
        },
        Match {
            span: SourceSpan,
            branches: Vec<(BindingPattern, Expression)>,
        },
        IfCondition {
            span: SourceSpan,
            condition_for_pattern: Expression,
            then_branch: Expression,
            else_branch: Expression,
        },
        IfThenDone {
            span: SourceSpan,
            interpreted_condition: Expression,
            then_branch: Expression,
            else_branch: Expression,
            saved_context: Context,
        },
        IfElseDone {
            span: SourceSpan,
            interpreted_condition: Expression,
            then_branch: Expression,
            interpreted_then: Expression,
            then_type: Expression,
            then_diverges: bool,
            saved_context: Context,
        },
        LoopStart {
            span: SourceSpan,
            body: Expression,
            initial_context: Context,
            iteration_count: usize,
        },
        LoopIteration {
            span: SourceSpan,
            body: Expression,
            initial_context: Context,
            iteration_count: usize,
            prev_context: Context,
        },
        LoopFinalize {
            span: SourceSpan,
            was_in_loop_before: bool,
        },
        FunctionCall {
            span: SourceSpan,
        },
        InlineCall {
            saved_context: Context,
        },
        FunctionBody {
            span: SourceSpan,
            parameter: BindingPattern,
            saved_context: Context,
        },
        EnumAccess {
            span: SourceSpan,
            variant: Identifier,
        },
    }

    let mut stack = vec![Frame::Eval(expr)];
    let mut values: Vec<Expression> = Vec::new();
    let mut pattern_stack: Vec<BindingPattern> = Vec::new();

    while let Some(frame) = stack.pop() {
        match frame {
            Frame::Eval(expr) => {
                let span = expr.span;
                match expr.kind {
                    ExpressionKind::Block(expressions) => {
                        if expressions.is_empty() {
                            return Err(diagnostic("Cannot interpret empty block", span));
                        }
                        let outer_context = context.clone();
                        context.bindings.push(HashMap::new());
                        let first_expr = expressions[0].clone();
                        stack.push(Frame::BlockAfterExpr {
                            span,
                            expressions,
                            outer_context,
                            index: 0,
                            interpreted_expressions: Vec::new(),
                            preserved_indices: HashSet::new(),
                        });
                        stack.push(Frame::Eval(first_expr));
                    }
                    ExpressionKind::EnumType(variants) => {
                        let identifiers = variants.iter().map(|(id, _)| id.clone()).collect();
                        stack.push(Frame::EnumType { span, identifiers });
                        for (_, ty_expr) in variants.into_iter().rev() {
                            stack.push(Frame::Eval(ty_expr));
                        }
                    }
                    ExpressionKind::Literal(lit) => {
                        values.push(Expression::new(ExpressionKind::Literal(lit), span));
                    }
                    ExpressionKind::IntrinsicType(ty) => {
                        values.push(Expression::new(ExpressionKind::IntrinsicType(ty), span));
                    }
                    ExpressionKind::Match { value, branches } => {
                        stack.push(Frame::Match { span, branches });
                        stack.push(Frame::Eval(*value));
                    }
                    ExpressionKind::If {
                        condition,
                        then_branch,
                        else_branch,
                    } => {
                        let condition_expr = *condition;
                        let condition_for_pattern = condition_expr.clone();
                        stack.push(Frame::IfCondition {
                            span,
                            condition_for_pattern,
                            then_branch: *then_branch,
                            else_branch: *else_branch,
                        });
                        stack.push(Frame::Eval(condition_expr));
                    }
                    ExpressionKind::Identifier(identifier) => {
                        if let Some((binding, _)) = context.get_identifier(&identifier) {
                            match &binding {
                                BindingContext::Bound(
                                    value,
                                    PreserveBehavior::PreserveUsageInLoops,
                                    _,
                                ) => {
                                    if context.in_loop {
                                        values.push(Expression::new(
                                            ExpressionKind::Identifier(identifier),
                                            span,
                                        ));
                                    } else {
                                        values.push(value.clone());
                                    }
                                }
                                BindingContext::Bound(
                                    expr,
                                    PreserveBehavior::Inline | PreserveBehavior::PreserveBinding,
                                    _,
                                ) => values.push(expr.clone()),
                                BindingContext::UnboundWithType(_)
                                | BindingContext::UnboundWithoutType
                                | BindingContext::Bound(_, PreserveBehavior::PreserveUsage, _) => {
                                    values.push(Expression::new(
                                        ExpressionKind::Identifier(identifier),
                                        span,
                                    ));
                                }
                            }
                        } else {
                            return Err(diagnostic(
                                format!("Unbound identifier: {}", identifier.name),
                                span,
                            ));
                        }
                    }
                    ExpressionKind::Operation {
                        operator,
                        left,
                        right,
                    } => {
                        stack.push(Frame::Eval(
                            ExpressionKind::FunctionCall {
                                function: Box::new(
                                    ExpressionKind::PropertyAccess {
                                        object: left,
                                        property: operator.clone(),
                                    }
                                    .with_span(span),
                                ),
                                argument: right,
                            }
                            .with_span(span),
                        ));
                    }
                    ExpressionKind::Binding(binding) => {
                        stack.push(Frame::BindingFinish);
                        stack.push(Frame::Eval(binding.expr));
                        stack.push(Frame::PatternStart(binding.pattern));
                    }
                    ExpressionKind::Diverge {
                        value,
                        divergance_type,
                    } => {
                        stack.push(Frame::Diverge {
                            span,
                            divergance_type,
                        });
                        stack.push(Frame::Eval(*value));
                    }
                    ExpressionKind::Loop { body } => {
                        let initial_context = context.clone();
                        stack.push(Frame::LoopStart {
                            span,
                            body: *body,
                            initial_context,
                            iteration_count: 0,
                        });
                    }
                    ExpressionKind::Assignment { target, expr } => {
                        stack.push(Frame::Assignment { span, target });
                        stack.push(Frame::Eval(*expr));
                    }
                    ExpressionKind::FunctionCall { function, argument } => {
                        stack.push(Frame::FunctionCall { span });
                        stack.push(Frame::Eval(*argument));
                        stack.push(Frame::Eval(*function));
                    }
                    ExpressionKind::ArrayIndex { array, index } => {
                        stack.push(Frame::ArrayIndex { span });
                        stack.push(Frame::Eval(*index));
                        stack.push(Frame::Eval(*array));
                    }
                    ExpressionKind::AttachImplementation {
                        type_expr,
                        implementation,
                    } => {
                        stack.push(Frame::AttachImplementation { span });
                        stack.push(Frame::Eval(*implementation));
                        stack.push(Frame::Eval(*type_expr));
                    }
                    ExpressionKind::Function {
                        parameter,
                        return_type: _,
                        body,
                    } => {
                        stack.push(Frame::FunctionStart { span, body: *body });
                        stack.push(Frame::PatternStart(parameter));
                    }
                    ExpressionKind::Struct(items) => {
                        let identifiers = items.iter().map(|(id, _)| id.clone()).collect();
                        stack.push(Frame::Struct { span, identifiers });
                        for (_, value_expr) in items.into_iter().rev() {
                            stack.push(Frame::Eval(value_expr));
                        }
                    }
                    ExpressionKind::FunctionType {
                        parameter,
                        return_type,
                    } => {
                        stack.push(Frame::FunctionType { span });
                        stack.push(Frame::Eval(*return_type));
                        stack.push(Frame::Eval(*parameter));
                    }
                    ExpressionKind::EnumAccess { enum_expr, variant } => {
                        stack.push(Frame::EnumAccess { span, variant });
                        stack.push(Frame::Eval(*enum_expr));
                    }
                    ExpressionKind::EnumValue {
                        enum_type,
                        variant,
                        variant_index,
                        payload,
                    } => {
                        stack.push(Frame::EnumValue {
                            span,
                            variant,
                            variant_index,
                        });
                        stack.push(Frame::Eval(*payload));
                        stack.push(Frame::Eval(*enum_type));
                    }
                    ExpressionKind::EnumConstructor {
                        enum_type,
                        variant,
                        variant_index,
                        payload_type,
                    } => {
                        stack.push(Frame::EnumConstructor {
                            span,
                            variant,
                            variant_index,
                        });
                        stack.push(Frame::Eval(*payload_type));
                        stack.push(Frame::Eval(*enum_type));
                    }
                    ExpressionKind::IntrinsicOperation(intrinsic_operation) => {
                        match intrinsic_operation {
                            IntrinsicOperation::Binary(left, right, operator) => {
                                stack.push(Frame::IntrinsicBinary { span, operator });
                                stack.push(Frame::Eval(*right));
                                stack.push(Frame::Eval(*left));
                            }
                            IntrinsicOperation::Unary(operand, operator) => {
                                stack.push(Frame::IntrinsicUnary { span, operator });
                                stack.push(Frame::Eval(*operand));
                            }
                        }
                    }
                    ExpressionKind::PropertyAccess { object, property } => {
                        let original_object = *object;
                        stack.push(Frame::PropertyAccess {
                            span,
                            property,
                            original_object: original_object.clone(),
                        });
                        stack.push(Frame::Eval(original_object));
                    }
                }
            }
            Frame::PatternStart(pattern) => match pattern {
                pat @ BindingPattern::Identifier(..) => pattern_stack.push(pat),
                pat @ BindingPattern::Literal(..) => pattern_stack.push(pat),
                BindingPattern::Struct(items, span) => {
                    let identifiers = items.iter().map(|(id, _)| id.clone()).collect();
                    stack.push(Frame::PatternStruct { span, identifiers });
                    for (_, field_pattern) in items.into_iter().rev() {
                        stack.push(Frame::PatternStart(field_pattern));
                    }
                }
                BindingPattern::EnumVariant {
                    enum_type,
                    variant,
                    payload,
                    span,
                } => {
                    let has_payload = payload.is_some();
                    stack.push(Frame::PatternEnumVariant {
                        variant,
                        span,
                        has_payload,
                    });
                    if let Some(payload) = payload {
                        stack.push(Frame::PatternStart(*payload));
                    }
                    stack.push(Frame::Eval(*enum_type));
                }
                BindingPattern::TypeHint(inner, type_expr, span) => {
                    stack.push(Frame::PatternTypeHint { span });
                    stack.push(Frame::PatternStart(*inner));
                    stack.push(Frame::Eval(*type_expr));
                }
                BindingPattern::Annotated {
                    annotations,
                    pattern,
                    span,
                } => {
                    let mut export_indices = Vec::new();
                    let mut export_exprs = Vec::new();
                    for (idx, ann) in annotations.iter().enumerate() {
                        if let BindingAnnotation::Export(expr, _) = ann {
                            export_indices.push(idx);
                            export_exprs.push(expr.clone());
                        }
                    }
                    stack.push(Frame::PatternAnnotated {
                        annotations,
                        span,
                        export_indices,
                    });
                    stack.push(Frame::PatternStart(*pattern));
                    for expr in export_exprs.into_iter().rev() {
                        stack.push(Frame::Eval(expr));
                    }
                }
            },
            Frame::PatternStruct { span, identifiers } => {
                let mut interpreted = Vec::with_capacity(identifiers.len());
                for _ in 0..identifiers.len() {
                    interpreted.push(pattern_stack.pop().unwrap());
                }
                interpreted.reverse();
                let items = identifiers
                    .into_iter()
                    .zip(interpreted.into_iter())
                    .collect();
                pattern_stack.push(BindingPattern::Struct(items, span));
            }
            Frame::PatternEnumVariant {
                variant,
                span,
                has_payload,
            } => {
                let enum_type = values.pop().unwrap();
                let payload = if has_payload {
                    Some(Box::new(pattern_stack.pop().unwrap()))
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
            Frame::PatternTypeHint { span } => {
                let type_expr = values.pop().unwrap();
                let inner = pattern_stack.pop().unwrap();
                pattern_stack.push(BindingPattern::TypeHint(
                    Box::new(inner),
                    Box::new(type_expr),
                    span,
                ));
            }
            Frame::PatternAnnotated {
                mut annotations,
                span,
                export_indices,
            } => {
                let inner = pattern_stack.pop().unwrap();
                let mut export_values = Vec::with_capacity(export_indices.len());
                for _ in 0..export_indices.len() {
                    export_values.push(values.pop().unwrap());
                }
                export_values.reverse();
                for (idx, expr) in export_indices.into_iter().zip(export_values.into_iter()) {
                    if let BindingAnnotation::Export(_, ann_span) = annotations[idx] {
                        annotations[idx] = BindingAnnotation::Export(expr, ann_span);
                    }
                }
                pattern_stack.push(BindingPattern::Annotated {
                    pattern: Box::new(inner),
                    annotations,
                    span,
                });
            }
            Frame::BindingFinish => {
                let value = values.pop().unwrap();
                let interpreted_pattern = pattern_stack.pop().unwrap();
                if let Ok(value_type) = get_type_of_expression(&value, context) {
                    bind_pattern_blanks(
                        interpreted_pattern.clone(),
                        context,
                        Vec::new(),
                        Some(value_type),
                    )?;
                }
                let value_is_constant =
                    is_resolved_constant(&value) || function_contains_compile_time_data(&value);
                let (bound_success, preserve_behavior) = bind_pattern_from_value(
                    interpreted_pattern.clone(),
                    &value,
                    context,
                    Vec::new(),
                    if value_is_constant {
                        PreserveBehavior::Inline
                    } else {
                        PreserveBehavior::PreserveUsage
                    },
                    None,
                )?;
                let binding_expr = ExpressionKind::Binding(Box::new(Binding {
                    pattern: interpreted_pattern,
                    expr: value,
                }))
                .with_span(dummy_span());
                values.push(
                    if preserve_behavior != PreserveBehavior::Inline
                        || (!value_is_constant && !bound_success)
                    {
                        binding_expr
                    } else {
                        ExpressionKind::Literal(ExpressionLiteral::Boolean(bound_success))
                            .with_span(dummy_span())
                    },
                );
            }
            Frame::FunctionStart { span, body } => {
                let parameter = pattern_stack.pop().unwrap();
                let mut type_context = context.clone();
                bind_pattern_blanks(parameter.clone(), &mut type_context, Vec::new(), None)?;
                let saved_context = context.clone();
                *context = type_context;
                stack.push(Frame::FunctionBody {
                    span,
                    parameter,
                    saved_context,
                });
                stack.push(Frame::Eval(body));
            }
            Frame::BlockAfterExpr {
                span,
                expressions,
                outer_context,
                index,
                mut interpreted_expressions,
                mut preserved_indices,
            } => {
                let value = values.pop().unwrap();
                if expression_contains_external_mutation(&value, &outer_context)
                    || expression_does_diverge(&value, true, false)
                    || expression_exports(&value)
                {
                    preserved_indices.insert(index);
                }

                if matches!(
                    value.kind,
                    ExpressionKind::Diverge {
                        divergance_type: DivergeExpressionType::Break
                            | DivergeExpressionType::Return,
                        ..
                    }
                ) {
                    context.bindings.pop();
                    values.push(value);
                    continue;
                }

                interpreted_expressions.push(value);

                let next_index = index + 1;
                if next_index < expressions.len() {
                    let next_expr = expressions[next_index].clone();
                    stack.push(Frame::BlockAfterExpr {
                        span,
                        expressions,
                        outer_context,
                        index: next_index,
                        interpreted_expressions,
                        preserved_indices,
                    });
                    stack.push(Frame::Eval(next_expr));
                } else {
                    preserved_indices.insert(interpreted_expressions.len() - 1);

                    let expression_usage: Vec<HashSet<Identifier>> = interpreted_expressions
                        .iter()
                        .map(identifiers_used)
                        .collect();

                    let expression_modifications: Vec<HashSet<Identifier>> =
                        interpreted_expressions
                            .iter()
                            .map(identifiers_created_or_modified)
                            .collect();

                    let mut needed_identifiers: HashSet<Identifier> = HashSet::new();
                    for idx in (0..interpreted_expressions.len()).rev() {
                        let mut preserve_current = preserved_indices.contains(&idx);
                        if !preserve_current
                            && expression_modifications[idx]
                                .iter()
                                .any(|identifier| needed_identifiers.contains(identifier))
                        {
                            preserved_indices.insert(idx);
                            preserve_current = true;
                        }

                        if preserve_current {
                            for identifier in &expression_usage[idx] {
                                needed_identifiers.insert(identifier.clone());
                            }
                        }
                    }

                    context.bindings.pop();
                    if preserved_indices.len() == 1 {
                        values.push(interpreted_expressions.into_iter().last().unwrap());
                    } else {
                        let preserved_expressions = interpreted_expressions
                            .into_iter()
                            .enumerate()
                            .filter(|(idx, _)| preserved_indices.contains(idx))
                            .map(|(_, expr)| expr)
                            .collect();
                        values.push(Expression::new(
                            ExpressionKind::Block(preserved_expressions),
                            span,
                        ));
                    }
                }
            }
            Frame::EnumType { span, identifiers } => {
                let mut evaluated = Vec::with_capacity(identifiers.len());
                for _ in 0..identifiers.len() {
                    evaluated.push(values.pop().unwrap());
                }
                evaluated.reverse();
                let variants = identifiers.into_iter().zip(evaluated.into_iter()).collect();
                values.push(Expression::new(ExpressionKind::EnumType(variants), span));
            }
            Frame::Struct { span, identifiers } => {
                let mut evaluated = Vec::with_capacity(identifiers.len());
                for _ in 0..identifiers.len() {
                    evaluated.push(values.pop().unwrap());
                }
                evaluated.reverse();
                let items = identifiers.into_iter().zip(evaluated.into_iter()).collect();
                values.push(Expression::new(ExpressionKind::Struct(items), span));
            }
            Frame::FunctionType { span } => {
                let return_type = values.pop().unwrap();
                let parameter = values.pop().unwrap();
                values.push(
                    ExpressionKind::FunctionType {
                        parameter: Box::new(parameter),
                        return_type: Box::new(return_type),
                    }
                    .with_span(span),
                );
            }
            Frame::EnumValue {
                span,
                variant,
                variant_index,
            } => {
                let payload = values.pop().unwrap();
                let enum_type = values.pop().unwrap();
                values.push(
                    ExpressionKind::EnumValue {
                        enum_type: Box::new(enum_type),
                        variant,
                        variant_index,
                        payload: Box::new(payload),
                    }
                    .with_span(span),
                );
            }
            Frame::EnumConstructor {
                span,
                variant,
                variant_index,
            } => {
                let payload_type = values.pop().unwrap();
                let enum_type = values.pop().unwrap();
                values.push(
                    ExpressionKind::EnumConstructor {
                        enum_type: Box::new(enum_type),
                        variant,
                        variant_index,
                        payload_type: Box::new(payload_type),
                    }
                    .with_span(span),
                );
            }
            Frame::AttachImplementation { span } => {
                let implementation = values.pop().unwrap();
                let type_expr = values.pop().unwrap();
                values.push(
                    ExpressionKind::AttachImplementation {
                        type_expr: Box::new(type_expr),
                        implementation: Box::new(implementation),
                    }
                    .with_span(span),
                );
            }
            Frame::IntrinsicBinary { span, operator } => {
                let evaluated_right = values.pop().unwrap();
                let evaluated_left = values.pop().unwrap();
                if !is_resolved_constant(&evaluated_left) || !is_resolved_constant(&evaluated_right)
                {
                    values.push(Expression::new(
                        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(
                            Box::new(evaluated_left),
                            Box::new(evaluated_right),
                            operator,
                        )),
                        span,
                    ));
                    continue;
                }
                let result = match operator {
                    BinaryIntrinsicOperator::I32Add
                    | BinaryIntrinsicOperator::I32Subtract
                    | BinaryIntrinsicOperator::I32Multiply => interpret_numeric_intrinsic(
                        evaluated_left,
                        evaluated_right,
                        span,
                        match operator {
                            BinaryIntrinsicOperator::I32Add => |l, r| l + r,
                            BinaryIntrinsicOperator::I32Subtract => |l, r| l - r,
                            BinaryIntrinsicOperator::I32Multiply => |l, r| l * r,
                            _ => unreachable!(),
                        },
                    )?,
                    BinaryIntrinsicOperator::I32Divide => {
                        interpret_divide_intrinsic(evaluated_left, evaluated_right, span)?
                    }
                    BinaryIntrinsicOperator::I32Equal
                    | BinaryIntrinsicOperator::I32NotEqual
                    | BinaryIntrinsicOperator::I32LessThan
                    | BinaryIntrinsicOperator::I32GreaterThan
                    | BinaryIntrinsicOperator::I32LessThanOrEqual
                    | BinaryIntrinsicOperator::I32GreaterThanOrEqual => {
                        interpret_comparison_intrinsic(
                            evaluated_left,
                            evaluated_right,
                            span,
                            match operator {
                                BinaryIntrinsicOperator::I32Equal => |l, r| l == r,
                                BinaryIntrinsicOperator::I32NotEqual => |l, r| l != r,
                                BinaryIntrinsicOperator::I32LessThan => |l, r| l < r,
                                BinaryIntrinsicOperator::I32GreaterThan => |l, r| l > r,
                                BinaryIntrinsicOperator::I32LessThanOrEqual => |l, r| l <= r,
                                BinaryIntrinsicOperator::I32GreaterThanOrEqual => |l, r| l >= r,
                                _ => unreachable!(),
                            },
                        )?
                    }
                    BinaryIntrinsicOperator::BooleanAnd
                    | BinaryIntrinsicOperator::BooleanOr
                    | BinaryIntrinsicOperator::BooleanXor => interpret_boolean_intrinsic(
                        evaluated_left,
                        evaluated_right,
                        span,
                        match operator {
                            BinaryIntrinsicOperator::BooleanAnd => |l, r| l && r,
                            BinaryIntrinsicOperator::BooleanOr => |l, r| l || r,
                            BinaryIntrinsicOperator::BooleanXor => |l, r| l ^ r,
                            _ => unreachable!(),
                        },
                    )?,
                };
                values.push(result);
            }
            Frame::IntrinsicUnary { span, operator } => {
                let evaluated_operand = values.pop().unwrap();
                let op_requires_const_argument = match operator {
                    UnaryIntrinsicOperator::BooleanNot => true,
                    UnaryIntrinsicOperator::EnumFromStruct => true,
                    UnaryIntrinsicOperator::MatchFromStruct => false,
                    UnaryIntrinsicOperator::UseFromString => true,
                };
                if !is_resolved_constant(&evaluated_operand) && op_requires_const_argument {
                    values.push(Expression::new(
                        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(
                            Box::new(evaluated_operand),
                            operator,
                        )),
                        span,
                    ));
                    continue;
                }
                let result = match operator {
                    UnaryIntrinsicOperator::MatchFromStruct => {
                        let ExpressionKind::Struct(branches) = evaluated_operand.kind else {
                            return Err(diagnostic(
                                format!(
                                    "match expects a struct of branch functions, got {:?}",
                                    evaluated_operand
                                ),
                                span,
                            ));
                        };

                        let mut match_branches = Vec::with_capacity(branches.len());
                        for (_, branch_expr) in branches {
                            let ExpressionKind::Function {
                                parameter, body, ..
                            } = branch_expr.kind
                            else {
                                return Err(diagnostic(
                                    "match expects each branch to be a function",
                                    branch_expr.span(),
                                ));
                            };
                            match_branches.push((parameter, *body));
                        }

                        let match_value = Identifier::new("match_value");
                        let parameter = BindingPattern::Identifier(match_value.clone(), span);
                        let match_expr = ExpressionKind::Match {
                            value: Box::new(
                                ExpressionKind::Identifier(match_value).with_span(span),
                            ),
                            branches: match_branches,
                        }
                        .with_span(span);

                        let mut type_context = context.clone();
                        bind_pattern_blanks(
                            parameter.clone(),
                            &mut type_context,
                            Vec::new(),
                            None,
                        )?;
                        let return_type = get_type_of_expression(&match_expr, &type_context)?;

                        ExpressionKind::Function {
                            parameter,
                            return_type: Some(Box::new(return_type)),
                            body: Box::new(match_expr),
                        }
                        .with_span(span)
                    }
                    UnaryIntrinsicOperator::BooleanNot => match evaluated_operand.kind {
                        ExpressionKind::Literal(ExpressionLiteral::Boolean(b)) => {
                            ExpressionKind::Literal(ExpressionLiteral::Boolean(!b))
                                .with_span(evaluated_operand.span)
                        }
                        _ => {
                            return Err(Diagnostic::new(
                                "Cannot perform boolean not on non boolean type",
                            )
                            .with_span(span));
                        }
                    },
                    UnaryIntrinsicOperator::EnumFromStruct => {
                        let ExpressionKind::Struct(variants) = evaluated_operand.kind else {
                            return Err(diagnostic(
                                format!(
                                    "enum expects a struct of variants, got {:?}",
                                    evaluated_operand
                                ),
                                span,
                            ));
                        };

                        let mut evaluated_variants = Vec::with_capacity(variants.len());
                        for (variant_name, variant_type) in variants {
                            if !is_type_expression(&variant_type.kind) {
                                return Err(diagnostic(
                                    "Enum variant payload must be a type",
                                    variant_type.span(),
                                ));
                            }
                            evaluated_variants.push((variant_name, variant_type));
                        }

                        ExpressionKind::EnumType(evaluated_variants).with_span(span)
                    }
                    UnaryIntrinsicOperator::UseFromString => {
                        let ExpressionKind::Literal(ExpressionLiteral::String(bytes)) =
                            evaluated_operand.kind
                        else {
                            return Err(diagnostic(
                                "use expects a string literal path",
                                evaluated_operand.span(),
                            ));
                        };
                        let path = String::from_utf8(bytes)
                            .map_err(|_| diagnostic("use path must be valid UTF-8", span))?;
                        let normalized = loader::normalize_path(&path);
                        if let Some(cached) = context.import_cache.get(&normalized) {
                            return Ok(cached.clone());
                        }
                        let expression = context.files.get(&normalized).ok_or_else(|| {
                            diagnostic(format!("Unknown file path: {path}"), span)
                        })?;
                        let mut import_context =
                            intrinsic_context_with_files(context.files.clone());
                        let (value, _) = interpret_program(expression.clone(), &mut import_context)?;
                        context.import_cache.insert(normalized, value.clone());
                        value
                    }
                };
                values.push(result);
            }
            Frame::Diverge {
                span,
                divergance_type,
            } => {
                let evaluated_value = values.pop().unwrap();
                values.push(
                    ExpressionKind::Diverge {
                        value: Box::new(evaluated_value),
                        divergance_type,
                    }
                    .with_span(span),
                );
            }
            Frame::Assignment { span, target } => {
                let value = values.pop().unwrap();
                values.push(apply_assignment(target, value, span, context)?);
            }
            Frame::Match { span, branches } => {
                let interpreted_value = values.pop().unwrap();
                if !is_resolved_constant(&interpreted_value) {
                    values.push(
                        ExpressionKind::Match {
                            value: Box::new(interpreted_value),
                            branches,
                        }
                        .with_span(span),
                    );
                    continue;
                }

                let mut matched = false;
                for (pattern, branch) in branches {
                    let mut branch_context = context.clone();
                    let (pattern_matched, _preserve_behavior) = bind_pattern_from_value(
                        pattern.clone(),
                        &interpreted_value,
                        &mut branch_context,
                        Vec::new(),
                        PreserveBehavior::Inline,
                        None,
                    )?;

                    if pattern_matched {
                        *context = branch_context;
                        stack.push(Frame::Eval(branch));
                        matched = true;
                        break;
                    }
                }

                if !matched {
                    return Err(diagnostic("No match branches matched", span));
                }
            }
            Frame::IfCondition {
                span,
                condition_for_pattern,
                then_branch,
                else_branch,
            } => {
                let interpreted_condition = values.pop().unwrap();
                ensure_boolean_condition(&interpreted_condition, span, context, "If")?;

                let mut then_context = context.clone();
                collect_bindings(&condition_for_pattern, &mut then_context)?;
                let saved_context = context.clone();
                *context = then_context;

                let then_branch_eval = then_branch.clone();
                stack.push(Frame::IfThenDone {
                    span,
                    interpreted_condition,
                    then_branch,
                    else_branch,
                    saved_context,
                });
                stack.push(Frame::Eval(then_branch_eval));
            }
            Frame::IfThenDone {
                span,
                interpreted_condition,
                then_branch,
                else_branch,
                saved_context,
            } => {
                let interpreted_then = values.pop().unwrap();
                let then_type = get_type_of_expression(&interpreted_then, context)?;
                let then_diverges = matches!(interpreted_then.kind, ExpressionKind::Diverge { .. });

                *context = saved_context.clone();
                stack.push(Frame::IfElseDone {
                    span,
                    interpreted_condition,
                    then_branch,
                    interpreted_then,
                    then_type,
                    then_diverges,
                    saved_context,
                });
                stack.push(Frame::Eval(else_branch));
            }
            Frame::IfElseDone {
                span,
                interpreted_condition,
                then_branch,
                interpreted_then,
                then_type,
                then_diverges,
                saved_context,
            } => {
                let interpreted_else = values.pop().unwrap();
                let else_type = get_type_of_expression(&interpreted_else, context)?;
                let else_diverges = matches!(interpreted_else.kind, ExpressionKind::Diverge { .. });

                *context = saved_context;

                if !types_equivalent(&then_type.kind, &else_type.kind)
                    && !then_diverges
                    && !else_diverges
                {
                    return Err(diagnostic("Type mismatch between if branches", span));
                }

                if let ExpressionKind::Literal(ExpressionLiteral::Boolean(condition_value)) =
                    interpreted_condition.kind
                {
                    if condition_value {
                        stack.push(Frame::Eval(then_branch));
                    } else {
                        stack.push(Frame::Eval(interpreted_else));
                    }
                } else {
                    let interpreted = ExpressionKind::If {
                        condition: Box::new(interpreted_condition),
                        then_branch: Box::new(interpreted_then),
                        else_branch: Box::new(interpreted_else),
                    }
                    .with_span(span);
                    let possibly_mutated_values = get_possibly_mutated_values(&interpreted);
                    for possibly_mutated_value in possibly_mutated_values {
                        let binding = context.get_identifier(&possibly_mutated_value).unwrap();
                        if let Some(binding_ty) = binding.0.get_bound_type(context)? {
                            let binding =
                                context.get_mut_identifier(&possibly_mutated_value).unwrap();
                            binding.0 = BindingContext::UnboundWithType(binding_ty)
                        }
                    }
                    values.push(interpreted);
                }
            }
            Frame::LoopStart {
                span,
                body,
                initial_context,
                iteration_count,
            } => {
                if iteration_count > 10_000 {
                    return Err(diagnostic("Loop did not produce a return value", span));
                }

                let prev_context = context.clone();
                stack.push(Frame::LoopIteration {
                    span,
                    body: body.clone(),
                    initial_context,
                    iteration_count,
                    prev_context,
                });
                stack.push(Frame::Eval(body));
            }
            Frame::LoopIteration {
                span,
                body,
                initial_context,
                iteration_count,
                prev_context,
            } => {
                let iteration_value = values.pop().unwrap();
                if let ExpressionKind::Diverge {
                    value,
                    divergance_type,
                } = &iteration_value.kind
                {
                    match divergance_type {
                        DivergeExpressionType::Return => {
                            values.push(iteration_value);
                            continue;
                        }
                        DivergeExpressionType::Break => {
                            values.push(*value.clone());
                            continue;
                        }
                    }
                }

                let possible_divergence = expression_does_diverge(&iteration_value, true, false);
                if possible_divergence || prev_context.bindings == context.bindings {
                    *context = initial_context.clone();
                    let was_in_loop_before = context.in_loop;
                    context.in_loop = true;
                    stack.push(Frame::LoopFinalize {
                        span,
                        was_in_loop_before,
                    });
                    stack.push(Frame::Eval(body));
                } else {
                    stack.push(Frame::LoopStart {
                        span,
                        body,
                        initial_context,
                        iteration_count: iteration_count + 1,
                    });
                }
            }
            Frame::LoopFinalize {
                span,
                was_in_loop_before,
            } => {
                let interpreted_body = values.pop().unwrap();
                context.in_loop = was_in_loop_before;
                let possibly_mutated_values = get_possibly_mutated_values(&interpreted_body);
                for possibly_mutated_value in possibly_mutated_values {
                    let binding = context.get_identifier(&possibly_mutated_value).unwrap();
                    if let Some(binding_ty) = binding.0.get_bound_type(context)? {
                        let binding = context.get_mut_identifier(&possibly_mutated_value).unwrap();
                        binding.0 = BindingContext::UnboundWithType(binding_ty)
                    }
                }
                values.push(
                    ExpressionKind::Loop {
                        body: Box::new(interpreted_body),
                    }
                    .with_span(span),
                );
            }
            Frame::FunctionCall { span } => {
                let argument_value = values.pop().unwrap();
                let function_value = values.pop().unwrap();

                if let Ok(function_type) = get_type_of_expression(&function_value, context)
                    && homogeneous_struct_element_type(&function_type, context).is_some()
                {
                    let index_type = get_type_of_expression(&argument_value, context)?;
                    let expected_index = ExpressionKind::IntrinsicType(IntrinsicType::I32);
                    if !types_equivalent(&index_type.kind, &expected_index) {
                        return Err(diagnostic("Array index must be an i32 value", span));
                    }

                    if let (
                        ExpressionKind::Literal(ExpressionLiteral::String(bytes)),
                        ExpressionKind::Literal(ExpressionLiteral::Number(index)),
                    ) = (&function_value.kind, &argument_value.kind)
                    {
                        if *index < 0 {
                            return Err(diagnostic("Array index out of range", span));
                        }
                        let index = *index as usize;
                        if let Some(value) = bytes.get(index) {
                            values.push(
                                ExpressionKind::Literal(ExpressionLiteral::Char(*value))
                                    .with_span(span),
                            );
                            continue;
                        }
                        return Err(diagnostic("Array index out of range", span));
                    }

                    if let (
                        ExpressionKind::Struct(fields),
                        ExpressionKind::Literal(ExpressionLiteral::Number(index)),
                    ) = (&function_value.kind, &argument_value.kind)
                    {
                        if *index < 0 {
                            return Err(diagnostic("Array index out of range", span));
                        }
                        let index = *index as usize;
                        if let Some((_, value)) = fields.get(index) {
                            values.push(value.clone());
                            continue;
                        }
                        return Err(diagnostic("Array index out of range", span));
                    }

                    values.push(
                        ExpressionKind::ArrayIndex {
                            array: Box::new(function_value),
                            index: Box::new(argument_value),
                        }
                        .with_span(span),
                    );
                    continue;
                }

                let effective_function =
                    if let ExpressionKind::Identifier(ident) = &function_value.kind {
                        context.get_identifier(ident).and_then(|b| match &b.0 {
                            BindingContext::Bound(v, _, _) => Some(v.clone()),
                            _ => None,
                        })
                    } else {
                        Some(function_value.clone())
                    };

                if let Some(Expression {
                    kind:
                        ExpressionKind::Function {
                            parameter,
                            return_type,
                            body,
                        },
                    ..
                }) = effective_function
                {
                    let is_direct = matches!(function_value.kind, ExpressionKind::Function { .. });
                    let returns_compile_time_type = return_type
                        .as_ref()
                        .is_some_and(|ty| type_expression_contains_compile_time_data(ty.as_ref()));
                    let pattern_is_compile_time = pattern_contains_compile_time_data(&parameter);
                    let argument_is_const = is_resolved_constant(&argument_value);

                    if is_direct
                        || returns_compile_time_type
                        || pattern_is_compile_time
                        || argument_is_const
                    {
                        let mut call_context = context.clone();
                        bind_pattern_from_value(
                            parameter,
                            &argument_value,
                            &mut call_context,
                            Vec::new(),
                            PreserveBehavior::Inline,
                            None,
                        )?;
                        let saved_context = context.clone();
                        *context = call_context;
                        stack.push(Frame::InlineCall { saved_context });
                        stack.push(Frame::Eval(*body));
                        continue;
                    }
                }

                if let ExpressionKind::EnumConstructor {
                    enum_type,
                    variant,
                    variant_index,
                    payload_type,
                } = function_value.kind.clone()
                {
                    let argument_type = get_type_of_expression(&argument_value, context)?;
                    if !types_equivalent(&payload_type.kind, &argument_type.kind) {
                        return Err(diagnostic("Enum variant payload type mismatch", span));
                    }
                    values.push(
                        ExpressionKind::EnumValue {
                            enum_type,
                            variant,
                            variant_index,
                            payload: Box::new(argument_value),
                        }
                        .with_span(span.merge(&function_value.span)),
                    );
                } else if is_resolved_constant(&function_value) {
                    return Err(diagnostic("Attempted to call a non-function value", span));
                } else {
                    values.push(
                        ExpressionKind::FunctionCall {
                            function: Box::new(function_value),
                            argument: Box::new(argument_value),
                        }
                        .with_span(span),
                    );
                }
            }
            Frame::ArrayIndex { span } => {
                let index = values.pop().unwrap();
                let array = values.pop().unwrap();
                values.push(
                    ExpressionKind::ArrayIndex {
                        array: Box::new(array),
                        index: Box::new(index),
                    }
                    .with_span(span),
                );
            }
            Frame::InlineCall { saved_context } => {
                let body_value = values.pop().unwrap();
                *context = saved_context;
                if let ExpressionKind::Diverge {
                    value,
                    divergance_type: DivergeExpressionType::Return,
                } = body_value.kind
                {
                    values.push(*value);
                } else {
                    values.push(body_value);
                }
            }
            Frame::FunctionBody {
                span,
                parameter,
                saved_context,
            } => {
                let interpreted_body = values.pop().unwrap();
                let return_type = get_type_of_expression(&interpreted_body, context)?;
                *context = saved_context;
                values.push(
                    ExpressionKind::Function {
                        parameter,
                        return_type: Some(Box::new(return_type)),
                        body: Box::new(interpreted_body),
                    }
                    .with_span(span),
                );
            }
            Frame::EnumAccess { span, variant } => {
                let interpreted_enum = values.pop().unwrap();
                if let Some((variant_index, payload_type)) =
                    enum_variant_info(&interpreted_enum, &variant)
                {
                    if let ExpressionKind::Struct(fields) = &payload_type.kind
                        && fields.is_empty()
                    {
                        values.push(
                            ExpressionKind::EnumValue {
                                enum_type: Box::new(interpreted_enum),
                                variant,
                                variant_index,
                                payload: Box::new(ExpressionKind::Struct(vec![]).with_span(span)),
                            }
                            .with_span(span),
                        );
                    } else {
                        values.push(
                            ExpressionKind::EnumConstructor {
                                enum_type: Box::new(interpreted_enum),
                                variant,
                                variant_index,
                                payload_type: Box::new(payload_type),
                            }
                            .with_span(span),
                        );
                    }
                } else {
                    values.push(
                        ExpressionKind::EnumAccess {
                            enum_expr: Box::new(interpreted_enum),
                            variant,
                        }
                        .with_span(span),
                    );
                }
            }
            Frame::PropertyAccess {
                span,
                property,
                original_object,
            } => {
                let evaluated_object = values.pop().unwrap();
                if let ExpressionKind::Struct(items) = &evaluated_object.kind {
                    if let Some((_, item_expr)) =
                        items.iter().find(|(item_id, _)| item_id.name == property)
                    {
                        values.push(item_expr.clone());
                        continue;
                    }
                }

                let mut object_type = get_type_of_expression(&original_object, context)?;
                if matches!(
                    object_type.kind,
                    ExpressionKind::FunctionType { .. } | ExpressionKind::IntrinsicType(_)
                ) {
                    object_type = get_type_of_expression(&evaluated_object, context)?;
                }
                let trait_prop = get_trait_prop_of_type(&object_type, &property, span, context)?;
                match trait_prop.kind {
                    ExpressionKind::Function { .. } => {
                        stack.push(Frame::Eval(
                            ExpressionKind::FunctionCall {
                                function: Box::new(trait_prop),
                                argument: Box::new(evaluated_object),
                            }
                            .with_span(span),
                        ));
                    }
                    _other => {
                        values.push(
                            ExpressionKind::PropertyAccess {
                                object: Box::new(evaluated_object),
                                property,
                            }
                            .with_span(span),
                        );
                    }
                }
            }
        }
    }

    values
        .pop()
        .ok_or_else(|| diagnostic("Failed to interpret expression", dummy_span()))
}

fn get_possibly_mutated_values(body: &Expression) -> HashSet<Identifier> {
    fold_expression(body, HashSet::new(), &|expr, mut mutated| {
        if let ExpressionKind::Assignment { ref target, .. } = expr.kind {
            mutated.extend(target.get_used_identifiers());
        }
        mutated
    })
}

fn get_type_of_expression(expr: &Expression, context: &Context) -> Result<Expression, Diagnostic> {
    let resolve_intrinsic_type =
        |name: &str, span: SourceSpan, context: &Context| -> Result<Expression, Diagnostic> {
            let identifier = Identifier::new(name.to_string());
            let (binding, _) = context.get_identifier(&identifier).ok_or_else(|| {
                diagnostic(format!("Unbound identifier: {}", identifier.name), span)
            })?;
            match binding {
                BindingContext::Bound(value, _, _) => Ok(value.clone()),
                BindingContext::UnboundWithType(type_expr) => Ok(type_expr.clone()),
                BindingContext::UnboundWithoutType => Err(diagnostic(
                    format!(
                        "Cannot determine type of unbound identifier: {}",
                        identifier.name
                    ),
                    span,
                )),
            }
        };
    let string_literal_type =
        |bytes: &[u8], span: SourceSpan, context: &Context| -> Result<Expression, Diagnostic> {
            let element_type = resolve_intrinsic_type("u8", span, context)?;
            let items = (0..bytes.len())
                .map(|index| (Identifier::new(index.to_string()), element_type.clone()))
                .collect();
            Ok(Expression::new(ExpressionKind::Struct(items), span))
        };

    let pattern_type_expr = |pattern: &BindingPattern,
                             context: &Context|
     -> Result<Expression, Diagnostic> {
        enum Frame<'a> {
            Enter(&'a BindingPattern),
            Struct {
                span: SourceSpan,
                identifiers: Vec<Identifier>,
            },
        }

        let mut stack = vec![Frame::Enter(pattern)];
        let mut results: Vec<Expression> = Vec::new();

        while let Some(frame) = stack.pop() {
            match frame {
                Frame::Enter(pattern) => match pattern {
                    BindingPattern::Identifier(_, span) => {
                        return Err(diagnostic(
                            "Cannot determine type of untyped identifier",
                            *span,
                        ));
                    }
                    BindingPattern::Literal(lit, span) => {
                        let type_expr = match lit {
                            ExpressionLiteral::Number(_) => {
                                resolve_intrinsic_type("i32", *span, context)?
                            }
                            ExpressionLiteral::Boolean(_) => {
                                resolve_intrinsic_type("bool", *span, context)?
                            }
                            ExpressionLiteral::Char(_) => {
                                resolve_intrinsic_type("u8", *span, context)?
                            }
                            ExpressionLiteral::String(bytes) => {
                                string_literal_type(&bytes, *span, context)?
                            }
                            ExpressionLiteral::Target(_) => {
                                resolve_intrinsic_type("target", *span, context)?
                            }
                        };
                        results.push(type_expr);
                    }
                    BindingPattern::Struct(items, span) => {
                        let identifiers = items.iter().map(|(id, _)| id.clone()).collect();
                        stack.push(Frame::Struct {
                            span: *span,
                            identifiers,
                        });
                        for (_, field_pattern) in items.iter().rev() {
                            stack.push(Frame::Enter(field_pattern));
                        }
                    }
                    BindingPattern::EnumVariant { enum_type, .. } => {
                        results.push(*enum_type.clone());
                    }
                    BindingPattern::TypeHint(_, type_expr, _) => {
                        results.push(*type_expr.clone());
                    }
                    BindingPattern::Annotated { pattern, .. } => {
                        stack.push(Frame::Enter(pattern));
                    }
                },
                Frame::Struct { span, identifiers } => {
                    let mut resolved = Vec::with_capacity(identifiers.len());
                    for _ in 0..identifiers.len() {
                        resolved.push(results.pop().unwrap());
                    }
                    resolved.reverse();
                    let struct_items = identifiers.into_iter().zip(resolved.into_iter()).collect();
                    results.push(Expression::new(ExpressionKind::Struct(struct_items), span));
                }
            }
        }

        results
            .pop()
            .ok_or_else(|| diagnostic("Cannot determine type of binding pattern", pattern.span()))
    };

    enum Frame {
        Enter(Expression, Context),
        Struct {
            span: SourceSpan,
            identifiers: Vec<Identifier>,
        },
        Block {
            span: SourceSpan,
            expressions: Vec<Expression>,
            context: Context,
            index: usize,
        },
        BlockAfterExpr {
            span: SourceSpan,
            expressions: Vec<Expression>,
            context: Context,
            index: usize,
        },
        BlockAfterBinding {
            span: SourceSpan,
            expressions: Vec<Expression>,
            context: Context,
            index: usize,
            pattern: BindingPattern,
        },
        Assignment {
            span: SourceSpan,
            target: LValue,
            context: Context,
        },
        If {
            span: SourceSpan,
            then_branch: Expression,
            else_branch: Expression,
        },
        Loop {
            span: SourceSpan,
            break_values: Vec<Expression>,
            use_body: bool,
        },
        MatchStart {
            span: SourceSpan,
            branches: Vec<(BindingPattern, Expression)>,
            context: Context,
        },
        MatchBranches {
            span: SourceSpan,
            branches: Vec<(BindingPattern, Expression)>,
            context: Context,
            value_type: Option<Expression>,
        },
        MatchFinish {
            span: SourceSpan,
            branches: Vec<(BindingPattern, Expression)>,
        },
        FunctionCall {
            span: SourceSpan,
            argument_expr: Expression,
            context: Context,
        },
        ArrayIndex {
            span: SourceSpan,
            context: Context,
        },
        PropertyAccess {
            span: SourceSpan,
            property: String,
            context: Context,
        },
    }

    let mut stack = vec![Frame::Enter(expr.clone(), context.clone())];
    let mut results: Vec<Expression> = Vec::new();

    while let Some(frame) = stack.pop() {
        match frame {
            Frame::Enter(expr, context) => {
                let span = expr.span();
                match expr.kind {
                    ExpressionKind::Literal(lit) => {
                        let type_expr = match lit {
                            ExpressionLiteral::Number(_) => {
                                resolve_intrinsic_type("i32", span, &context)?
                            }
                            ExpressionLiteral::Boolean(_) => {
                                resolve_intrinsic_type("bool", span, &context)?
                            }
                            ExpressionLiteral::Char(_) => {
                                resolve_intrinsic_type("u8", span, &context)?
                            }
                            ExpressionLiteral::String(bytes) => {
                                string_literal_type(&bytes, span, &context)?
                            }
                            ExpressionLiteral::Target(_) => {
                                resolve_intrinsic_type("target", span, &context)?
                            }
                        };
                        results.push(type_expr);
                    }
                    ExpressionKind::Identifier(identifier) => {
                        let bound_value = context
                            .get_identifier(&identifier)
                            .ok_or_else(|| {
                                diagnostic(format!("Unbound identifier: {}", identifier.name), span)
                            })?
                            .clone();

                        match bound_value.0 {
                            BindingContext::Bound(_, _, Some(bound_type)) => {
                                results.push(bound_type.clone());
                            }
                            BindingContext::Bound(value, _, None) => {
                                stack.push(Frame::Enter(value, context));
                            }
                            BindingContext::UnboundWithType(type_expr) => {
                                results.push(resolve_type_alias_expression(&type_expr, &context));
                            }
                            BindingContext::UnboundWithoutType => {
                                return Err(diagnostic(
                                    format!(
                                        "Cannot determine type of unbound identifier: {}",
                                        identifier.name
                                    ),
                                    span,
                                ));
                            }
                        }
                    }
                    ExpressionKind::Assignment {
                        target,
                        expr: value_expr,
                    } => {
                        stack.push(Frame::Assignment {
                            span,
                            target,
                            context: context.clone(),
                        });
                        stack.push(Frame::Enter(*value_expr, context));
                    }
                    ExpressionKind::Diverge { value, .. } => {
                        stack.push(Frame::Enter(*value, context));
                    }
                    ExpressionKind::If {
                        condition,
                        then_branch,
                        else_branch,
                    } => {
                        let mut then_context = context.clone();
                        collect_bindings(&condition, &mut then_context)?;
                        stack.push(Frame::If {
                            span,
                            then_branch: *then_branch.clone(),
                            else_branch: *else_branch.clone(),
                        });
                        stack.push(Frame::Enter(*else_branch, context.clone()));
                        stack.push(Frame::Enter(*then_branch, then_context));
                        stack.push(Frame::Enter(*condition, context));
                    }
                    ExpressionKind::Match { value, branches } => {
                        let skip_value_type = match &value.as_ref().kind {
                            ExpressionKind::Identifier(identifier) => context
                                .get_identifier(identifier)
                                .map(|(binding, _)| {
                                    matches!(binding, BindingContext::UnboundWithoutType)
                                })
                                .unwrap_or(false),
                            _ => false,
                        };

                        if skip_value_type {
                            stack.push(Frame::MatchBranches {
                                span,
                                branches,
                                context,
                                value_type: None,
                            });
                        } else {
                            stack.push(Frame::MatchStart {
                                span,
                                branches,
                                context: context.clone(),
                            });
                            stack.push(Frame::Enter(*value, context));
                        }
                    }
                    ExpressionKind::Binding(..) => {
                        results.push(resolve_intrinsic_type("bool", span, &context)?);
                    }
                    ExpressionKind::EnumAccess { enum_expr, variant } => {
                        let enum_type = resolve_type_alias_expression(enum_expr.as_ref(), &context);
                        if let Some((_, payload_type)) = enum_variant_info(&enum_type, &variant) {
                            if let ExpressionKind::Struct(fields) = &payload_type.kind
                                && fields.is_empty()
                            {
                                results.push(enum_type);
                            } else {
                                results.push(
                                    ExpressionKind::FunctionType {
                                        parameter: Box::new(payload_type),
                                        return_type: Box::new(enum_type),
                                    }
                                    .with_span(span),
                                );
                            }
                        } else {
                            return Err(diagnostic("Unknown enum variant", span));
                        }
                    }
                    ExpressionKind::EnumConstructor {
                        enum_type,
                        payload_type,
                        ..
                    } => {
                        results.push(
                            ExpressionKind::FunctionType {
                                parameter: payload_type,
                                return_type: enum_type,
                            }
                            .with_span(span),
                        );
                    }
                    ExpressionKind::EnumValue { enum_type, .. } => {
                        results.push(*enum_type);
                    }
                    ExpressionKind::EnumType(_) => {
                        results.push(
                            ExpressionKind::IntrinsicType(IntrinsicType::Type).with_span(span),
                        );
                    }
                    ExpressionKind::Operation {
                        operator,
                        left,
                        right,
                    } => {
                        stack.push(Frame::Enter(
                            ExpressionKind::FunctionCall {
                                function: Box::new(
                                    ExpressionKind::PropertyAccess {
                                        object: left.clone(),
                                        property: operator.clone(),
                                    }
                                    .with_span(span),
                                ),
                                argument: right.clone(),
                            }
                            .with_span(span),
                            context,
                        ));
                    }
                    ExpressionKind::Block(exprs) => {
                        if exprs.is_empty() {
                            return Err(Diagnostic::new(
                                "Cannot determine type of empty block".to_string(),
                            )
                            .with_span(span));
                        }
                        let mut block_context = context.clone();
                        block_context.bindings.push(HashMap::new());
                        stack.push(Frame::Block {
                            span,
                            expressions: exprs,
                            context: block_context,
                            index: 0,
                        });
                    }
                    ExpressionKind::Loop { body } => {
                        let break_values = collect_break_values(&body);
                        if break_values.is_empty() {
                            stack.push(Frame::Loop {
                                span,
                                break_values,
                                use_body: true,
                            });
                            stack.push(Frame::Enter(*body, context));
                        } else {
                            stack.push(Frame::Loop {
                                span,
                                break_values: break_values.clone(),
                                use_body: false,
                            });
                            for value in break_values.into_iter().rev() {
                                stack.push(Frame::Enter(value, context.clone()));
                            }
                        }
                    }
                    ExpressionKind::FunctionCall { function, argument } => {
                        let call_context = context.clone();
                        stack.push(Frame::FunctionCall {
                            span,
                            argument_expr: *argument.clone(),
                            context: call_context.clone(),
                        });
                        stack.push(Frame::Enter(*argument, call_context.clone()));
                        stack.push(Frame::Enter(*function, call_context));
                    }
                    ExpressionKind::ArrayIndex { array, index } => {
                        let call_context = context.clone();
                        stack.push(Frame::ArrayIndex {
                            span,
                            context: call_context.clone(),
                        });
                        stack.push(Frame::Enter(*index, call_context.clone()));
                        stack.push(Frame::Enter(*array, call_context));
                    }
                    ExpressionKind::PropertyAccess { object, property } => {
                        stack.push(Frame::PropertyAccess {
                            span,
                            property,
                            context: context.clone(),
                        });
                        stack.push(Frame::Enter(*object, context));
                    }
                    ExpressionKind::IntrinsicType(intrinsic_type) => match intrinsic_type {
                        IntrinsicType::I32
                        | IntrinsicType::U8
                        | IntrinsicType::Boolean
                        | IntrinsicType::Target
                        | IntrinsicType::Type => {
                            results.push(
                                ExpressionKind::IntrinsicType(IntrinsicType::Type).with_span(span),
                            );
                        }
                    },
                    ExpressionKind::AttachImplementation { type_expr, .. } => {
                        stack.push(Frame::Enter(*type_expr, context));
                    }
                    ExpressionKind::Function {
                        parameter,
                        return_type,
                        ..
                    } => {
                        let parameter_type = pattern_type_expr(&parameter, &context)?;
                        let parameter_type =
                            resolve_type_alias_expression(&parameter_type, &context);
                        let resolved_return =
                            resolve_type_alias_expression(return_type.as_ref().unwrap(), &context);
                        results.push(
                            ExpressionKind::FunctionType {
                                parameter: Box::new(parameter_type),
                                return_type: Box::new(resolved_return),
                            }
                            .with_span(span),
                        );
                    }
                    ExpressionKind::Struct(items) => {
                        let identifiers = items.iter().map(|(id, _)| id.clone()).collect();
                        stack.push(Frame::Struct { span, identifiers });
                        for (_, field_expr) in items.into_iter().rev() {
                            stack.push(Frame::Enter(field_expr, context.clone()));
                        }
                    }
                    ExpressionKind::FunctionType { .. } => {
                        results.push(
                            ExpressionKind::IntrinsicType(IntrinsicType::Type).with_span(span),
                        );
                    }
                    ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(
                        _,
                        _,
                        operator,
                    )) => {
                        let type_expr = match operator {
                            BinaryIntrinsicOperator::I32Add
                            | BinaryIntrinsicOperator::I32Subtract
                            | BinaryIntrinsicOperator::I32Multiply
                            | BinaryIntrinsicOperator::I32Divide => {
                                resolve_intrinsic_type("i32", span, &context)?
                            }
                            BinaryIntrinsicOperator::I32Equal
                            | BinaryIntrinsicOperator::I32NotEqual
                            | BinaryIntrinsicOperator::I32LessThan
                            | BinaryIntrinsicOperator::I32GreaterThan
                            | BinaryIntrinsicOperator::I32LessThanOrEqual
                            | BinaryIntrinsicOperator::I32GreaterThanOrEqual
                            | BinaryIntrinsicOperator::BooleanAnd
                            | BinaryIntrinsicOperator::BooleanOr
                            | BinaryIntrinsicOperator::BooleanXor => {
                                resolve_intrinsic_type("bool", span, &context)?
                            }
                        };
                        results.push(type_expr);
                    }
                    ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(
                        _,
                        UnaryIntrinsicOperator::BooleanNot,
                    )) => {
                        results.push(resolve_intrinsic_type("bool", span, &context)?);
                    }
                    ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(
                        _,
                        UnaryIntrinsicOperator::EnumFromStruct,
                    )) => {
                        results.push(resolve_intrinsic_type("type", span, &context)?);
                    }
                    ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(
                        _,
                        UnaryIntrinsicOperator::MatchFromStruct,
                    )) => {
                        return Err(diagnostic(
                            "match intrinsic should be resolved before type checking",
                            span,
                        ));
                    }
                    ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(
                        _,
                        UnaryIntrinsicOperator::UseFromString,
                    )) => {
                        return Err(diagnostic(
                            "use intrinsic should be resolved before type checking",
                            span,
                        ));
                    }
                }
            }
            Frame::Struct { span, identifiers } => {
                let mut struct_items = Vec::with_capacity(identifiers.len());
                for _ in 0..identifiers.len() {
                    struct_items.push(results.pop().unwrap());
                }
                struct_items.reverse();
                let struct_items = identifiers
                    .into_iter()
                    .zip(struct_items.into_iter())
                    .collect();
                results.push(Expression::new(ExpressionKind::Struct(struct_items), span));
            }
            Frame::Block {
                span,
                expressions,
                context,
                index,
            } => {
                let expr = expressions[index].clone();
                match expr.kind {
                    ExpressionKind::Binding(binding) => {
                        let expr_context = context.clone();
                        stack.push(Frame::BlockAfterBinding {
                            span,
                            expressions,
                            context,
                            index,
                            pattern: binding.pattern,
                        });
                        stack.push(Frame::Enter(binding.expr, expr_context));
                    }
                    _ => {
                        let expr_context = context.clone();
                        stack.push(Frame::BlockAfterExpr {
                            span,
                            expressions,
                            context,
                            index,
                        });
                        stack.push(Frame::Enter(expr, expr_context));
                    }
                }
            }
            Frame::BlockAfterExpr {
                span: _,
                expressions,
                context,
                index,
            } => {
                let expr_type = results.pop().unwrap();
                let next_index = index + 1;
                if next_index >= expressions.len() {
                    results.push(expr_type);
                } else {
                    stack.push(Frame::Block {
                        span: expressions[next_index].span(),
                        expressions,
                        context,
                        index: next_index,
                    });
                }
            }
            Frame::BlockAfterBinding {
                span,
                expressions,
                mut context,
                index,
                pattern,
            } => {
                let value_type = results.pop().unwrap();
                bind_pattern_blanks(pattern, &mut context, Vec::new(), Some(value_type))?;
                let bool_type = resolve_intrinsic_type("bool", span, &context)?;
                let next_index = index + 1;
                if next_index >= expressions.len() {
                    results.push(bool_type);
                } else {
                    stack.push(Frame::Block {
                        span: expressions[next_index].span(),
                        expressions,
                        context,
                        index: next_index,
                    });
                }
            }
            Frame::Assignment {
                span,
                target,
                context,
            } => {
                let value_type = results.pop().unwrap();
                let target_type = get_lvalue_type(&target, &context, span)?;

                if !types_equivalent(&target_type.kind, &value_type.kind) {
                    return Err(diagnostic(
                        format!(
                            "Cannot assign value of mismatched type to {}",
                            lvalue_display_name(&target)
                        ),
                        span,
                    ));
                }

                results.push(target_type);
            }
            Frame::If {
                span,
                then_branch,
                else_branch,
            } => {
                let else_type = results.pop().unwrap();
                let then_type = results.pop().unwrap();
                let condition_type = results.pop().unwrap();
                let expected_bool = ExpressionKind::IntrinsicType(IntrinsicType::Boolean);

                if !types_equivalent(&condition_type.kind, &expected_bool) {
                    return Err(diagnostic(
                        "If condition did not resolve to a boolean value",
                        span,
                    ));
                }

                if !types_equivalent(&then_type.kind, &else_type.kind) {
                    let then_returns = expression_does_diverge(&then_branch, false, false);
                    let else_returns = expression_does_diverge(&else_branch, false, false);

                    if then_returns && !else_returns {
                        results.push(else_type);
                    } else if else_returns && !then_returns {
                        results.push(then_type);
                    } else {
                        return Err(diagnostic("Type mismatch between if branches", span));
                    }
                } else {
                    results.push(then_type);
                }
            }
            Frame::Loop {
                span,
                break_values,
                use_body,
            } => {
                if use_body {
                    let body_type = results.pop().unwrap();
                    results.push(body_type);
                } else {
                    let mut first_type: Option<Expression> = None;
                    let mut break_types = Vec::with_capacity(break_values.len());
                    for _ in 0..break_values.len() {
                        break_types.push(results.pop().unwrap());
                    }
                    break_types.reverse();

                    for (break_value, ty) in break_values.iter().zip(break_types.iter()) {
                        if let Some(ref first) = first_type {
                            if !types_equivalent(&first.kind, &ty.kind) {
                                return Err(diagnostic(
                                    "Inconsistent break types in loop",
                                    break_value.span(),
                                ));
                            }
                        } else {
                            first_type = Some(ty.clone());
                        }
                    }
                    results.push(first_type.unwrap());
                }
                let _ = span;
            }
            Frame::MatchStart {
                span,
                branches,
                context,
            } => {
                let value_type = results.pop().unwrap();
                stack.push(Frame::MatchBranches {
                    span,
                    branches,
                    context,
                    value_type: Some(value_type),
                });
            }
            Frame::MatchBranches {
                span,
                branches,
                context,
                value_type,
            } => {
                let mut branch_contexts = Vec::with_capacity(branches.len());
                for (pattern, _) in branches.iter() {
                    let mut branch_context = context.clone();
                    bind_pattern_blanks(
                        pattern.clone(),
                        &mut branch_context,
                        Vec::new(),
                        value_type.clone(),
                    )?;
                    branch_contexts.push(branch_context);
                }

                stack.push(Frame::MatchFinish {
                    span,
                    branches: branches.clone(),
                });

                for ((_, branch), branch_context) in
                    branches.iter().rev().zip(branch_contexts.into_iter().rev())
                {
                    stack.push(Frame::Enter(branch.clone(), branch_context));
                }
            }
            Frame::MatchFinish { span, branches } => {
                let mut branch_types = Vec::with_capacity(branches.len());
                for _ in 0..branches.len() {
                    branch_types.push(results.pop().unwrap());
                }
                branch_types.reverse();

                let mut branch_type: Option<Expression> = None;
                for ((_, branch), branch_ty) in branches.iter().zip(branch_types.iter()) {
                    if let Some(existing) = &branch_type {
                        if !types_equivalent(&existing.kind, &branch_ty.kind)
                            && !expression_does_diverge(branch, false, false)
                        {
                            return Err(diagnostic("Type mismatch between match branches", span));
                        }
                    } else {
                        branch_type = Some(branch_ty.clone());
                    }
                }

                results.push(branch_type.ok_or(diagnostic("Match has no branches", span))?);
            }
            Frame::FunctionCall {
                span,
                argument_expr,
                context,
            } => {
                let argument_type = results.pop().unwrap();
                let mut evaluated_function_type = results.pop().unwrap();
                if let Some(mut element_type) =
                    homogeneous_struct_element_type(&evaluated_function_type, &context)
                {
                    let expected_index = ExpressionKind::IntrinsicType(IntrinsicType::I32);
                    if !types_equivalent(&argument_type.kind, &expected_index) {
                        return Err(diagnostic("Array index must be an i32 value", span));
                    }

                    if let ExpressionKind::IntrinsicType(intrinsic) = &element_type.kind {
                        element_type = match intrinsic {
                            IntrinsicType::I32 => resolve_intrinsic_type("i32", span, &context)?,
                            IntrinsicType::U8 => resolve_intrinsic_type("u8", span, &context)?,
                            IntrinsicType::Boolean => {
                                resolve_intrinsic_type("bool", span, &context)?
                            }
                            IntrinsicType::Target => {
                                resolve_intrinsic_type("target", span, &context)?
                            }
                            IntrinsicType::Type => resolve_intrinsic_type("type", span, &context)?,
                        };
                    }
                    results.push(element_type);
                    continue;
                }
                if let ExpressionKind::Function {
                    parameter,
                    return_type,
                    ..
                } = &evaluated_function_type.kind
                {
                    let parameter_type = pattern_type_expr(parameter, &context)?;
                    let parameter_type = resolve_type_alias_expression(&parameter_type, &context);
                    let resolved_return = resolve_type_alias_expression(
                        return_type.as_ref().unwrap().as_ref(),
                        &context,
                    );
                    evaluated_function_type = ExpressionKind::FunctionType {
                        parameter: Box::new(parameter_type),
                        return_type: Box::new(resolved_return),
                    }
                    .with_span(evaluated_function_type.span);
                }

                let ExpressionKind::FunctionType {
                    parameter,
                    return_type,
                } = &evaluated_function_type.kind
                else {
                    return Err(diagnostic("Attempted to call a non-function value", span));
                };
                let mut type_bindings: HashMap<String, Expression> = HashMap::new();
                if !collect_type_bindings(parameter.as_ref(), &argument_type, &mut type_bindings) {
                    return Err(diagnostic(
                        format!(
                            "Function argument type mismatch type {:?} vs {:?}",
                            parameter, argument_type
                        ),
                        span,
                    ));
                }

                if let Some(trait_expr) = trait_requirement_from_type_hint(parameter.as_ref()) {
                    ensure_trait_requirements(&argument_expr, &trait_expr, &context, span)?;
                }

                let mut return_value =
                    resolve_type_alias_expression(return_type.as_ref(), &context);
                if !type_bindings.is_empty() {
                    return_value = apply_type_bindings(&return_value, &type_bindings);
                }
                if let ExpressionKind::IntrinsicType(intrinsic) = &return_value.kind {
                    return_value = match intrinsic {
                        IntrinsicType::I32 => resolve_intrinsic_type("i32", span, &context)?,
                        IntrinsicType::U8 => resolve_intrinsic_type("u8", span, &context)?,
                        IntrinsicType::Boolean => resolve_intrinsic_type("bool", span, &context)?,
                        IntrinsicType::Target => resolve_intrinsic_type("target", span, &context)?,
                        IntrinsicType::Type => resolve_intrinsic_type("type", span, &context)?,
                    };
                }
                results.push(return_value);
            }
            Frame::ArrayIndex { span, context } => {
                let index_type = results.pop().unwrap();
                let array_type = results.pop().unwrap();
                let expected_index = ExpressionKind::IntrinsicType(IntrinsicType::I32);
                if !types_equivalent(&index_type.kind, &expected_index) {
                    return Err(diagnostic("Array index must be an i32 value", span));
                }

                let Some(mut element_type) = homogeneous_struct_element_type(&array_type, &context)
                else {
                    return Err(diagnostic("Attempted to index a non-array value", span));
                };

                if let ExpressionKind::IntrinsicType(intrinsic) = &element_type.kind {
                    element_type = match intrinsic {
                        IntrinsicType::I32 => resolve_intrinsic_type("i32", span, &context)?,
                        IntrinsicType::U8 => resolve_intrinsic_type("u8", span, &context)?,
                        IntrinsicType::Boolean => resolve_intrinsic_type("bool", span, &context)?,
                        IntrinsicType::Target => resolve_intrinsic_type("target", span, &context)?,
                        IntrinsicType::Type => resolve_intrinsic_type("type", span, &context)?,
                    };
                }
                results.push(element_type);
            }
            Frame::PropertyAccess {
                span,
                property,
                context,
            } => {
                let object_type = results.pop().unwrap();
                let trait_prop = get_trait_prop_of_type(&object_type, &property, span, &context)?;
                if let ExpressionKind::Function { return_type, .. } = &trait_prop.kind {
                    results.push(resolve_type_alias_expression(
                        return_type.as_ref().unwrap().as_ref(),
                        &context,
                    ));
                } else {
                    results.push(trait_prop);
                }
            }
        }
    }

    results
        .pop()
        .ok_or_else(|| diagnostic("Failed to determine expression type", expr.span()))
}

pub fn collect_break_values(expr: &Expression) -> Vec<Expression> {
    fold_expression(expr, Vec::new(), &|current_expr, mut values| {
        if let ExpressionKind::Diverge {
            value,
            divergance_type: DivergeExpressionType::Break,
        } = &current_expr.kind
        {
            values.push(*value.clone());
        }
        values
    })
}

pub fn expression_does_diverge(expr: &Expression, possibility: bool, in_inner_loop: bool) -> bool {
    enum Combine {
        Any(usize),
        All(usize),
        Match { branch_count: usize },
    }

    enum Frame<'a> {
        Enter(&'a Expression, bool),
        Exit(Combine),
    }

    let mut results: Vec<bool> = Vec::new();
    let mut stack = vec![Frame::Enter(expr, in_inner_loop)];

    while let Some(frame) = stack.pop() {
        match frame {
            Frame::Enter(expr, in_inner_loop) => match &expr.kind {
                ExpressionKind::Diverge {
                    divergance_type: DivergeExpressionType::Break,
                    ..
                } => results.push(!in_inner_loop),
                ExpressionKind::Diverge {
                    divergance_type: DivergeExpressionType::Return,
                    ..
                } => results.push(true),
                ExpressionKind::Block(exprs) => {
                    stack.push(Frame::Exit(Combine::Any(exprs.len())));
                    for expr in exprs.iter().rev() {
                        stack.push(Frame::Enter(expr, in_inner_loop));
                    }
                }
                ExpressionKind::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    let combine = if possibility {
                        Combine::Any(2)
                    } else {
                        Combine::All(2)
                    };
                    stack.push(Frame::Exit(combine));
                    stack.push(Frame::Enter(else_branch, in_inner_loop));
                    stack.push(Frame::Enter(then_branch, in_inner_loop));
                }
                ExpressionKind::Binding(binding) => {
                    stack.push(Frame::Exit(Combine::Any(1)));
                    stack.push(Frame::Enter(&binding.expr, in_inner_loop));
                }
                ExpressionKind::Assignment { expr, .. } => {
                    stack.push(Frame::Exit(Combine::Any(1)));
                    stack.push(Frame::Enter(expr, in_inner_loop));
                }
                ExpressionKind::FunctionCall {
                    function, argument, ..
                } => {
                    stack.push(Frame::Exit(Combine::Any(2)));
                    stack.push(Frame::Enter(argument, in_inner_loop));
                    stack.push(Frame::Enter(function, in_inner_loop));
                }
                ExpressionKind::ArrayIndex { array, index } => {
                    stack.push(Frame::Exit(Combine::Any(2)));
                    stack.push(Frame::Enter(index, in_inner_loop));
                    stack.push(Frame::Enter(array, in_inner_loop));
                }
                ExpressionKind::Loop { body } => {
                    stack.push(Frame::Exit(Combine::Any(1)));
                    stack.push(Frame::Enter(body, true));
                }
                ExpressionKind::PropertyAccess { object, .. } => {
                    stack.push(Frame::Exit(Combine::Any(1)));
                    stack.push(Frame::Enter(object, in_inner_loop));
                }
                ExpressionKind::Operation { left, right, .. } => {
                    stack.push(Frame::Exit(Combine::Any(2)));
                    stack.push(Frame::Enter(right, in_inner_loop));
                    stack.push(Frame::Enter(left, in_inner_loop));
                }
                ExpressionKind::AttachImplementation {
                    type_expr,
                    implementation,
                } => {
                    stack.push(Frame::Exit(Combine::Any(2)));
                    stack.push(Frame::Enter(implementation, in_inner_loop));
                    stack.push(Frame::Enter(type_expr, in_inner_loop));
                }
                ExpressionKind::EnumAccess { enum_expr, .. } => {
                    stack.push(Frame::Exit(Combine::Any(1)));
                    stack.push(Frame::Enter(enum_expr, in_inner_loop));
                }
                ExpressionKind::EnumValue {
                    enum_type, payload, ..
                } => {
                    stack.push(Frame::Exit(Combine::Any(2)));
                    stack.push(Frame::Enter(payload, in_inner_loop));
                    stack.push(Frame::Enter(enum_type, in_inner_loop));
                }
                ExpressionKind::EnumConstructor {
                    enum_type,
                    payload_type,
                    ..
                } => {
                    stack.push(Frame::Exit(Combine::Any(2)));
                    stack.push(Frame::Enter(payload_type, in_inner_loop));
                    stack.push(Frame::Enter(enum_type, in_inner_loop));
                }
                ExpressionKind::Match { value, branches } => {
                    stack.push(Frame::Exit(Combine::Match {
                        branch_count: branches.len(),
                    }));
                    for (_, branch) in branches.iter().rev() {
                        stack.push(Frame::Enter(branch, in_inner_loop));
                    }
                    stack.push(Frame::Enter(value, in_inner_loop));
                }
                ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(left, right, _)) => {
                    stack.push(Frame::Exit(Combine::Any(2)));
                    stack.push(Frame::Enter(right, in_inner_loop));
                    stack.push(Frame::Enter(left, in_inner_loop));
                }
                ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(operand, _)) => {
                    stack.push(Frame::Exit(Combine::Any(1)));
                    stack.push(Frame::Enter(operand, in_inner_loop));
                }
                ExpressionKind::Literal(_)
                | ExpressionKind::Identifier(_)
                | ExpressionKind::IntrinsicType(_)
                | ExpressionKind::EnumType(_)
                | ExpressionKind::Function { .. }
                | ExpressionKind::FunctionType { .. }
                | ExpressionKind::Struct(_) => results.push(false),
            },
            Frame::Exit(combine) => {
                let result = match combine {
                    Combine::Any(count) => {
                        let mut any = false;
                        for _ in 0..count {
                            if let Some(value) = results.pop() {
                                any |= value;
                            }
                        }
                        any
                    }
                    Combine::All(count) => {
                        let mut all = true;
                        for _ in 0..count {
                            if let Some(value) = results.pop() {
                                all &= value;
                            }
                        }
                        all
                    }
                    Combine::Match { branch_count } => {
                        let mut all_branches = true;
                        for _ in 0..branch_count {
                            if let Some(value) = results.pop() {
                                all_branches &= value;
                            }
                        }
                        let value_diverges = results.pop().unwrap_or(false);
                        value_diverges || all_branches
                    }
                };
                results.push(result);
            }
        }
    }

    results.pop().unwrap_or(false)
}

pub fn expression_exports(expr: &Expression) -> bool {
    enum ExprCombine {
        Any(usize),
        Match { branch_count: usize },
    }

    enum Frame<'a> {
        EnterExpr(&'a Expression),
        ExitExpr(ExprCombine),
        EnterPattern(&'a BindingPattern),
        ExitPattern { child_count: usize, base: bool },
    }

    let mut results: Vec<bool> = Vec::new();
    let mut stack = vec![Frame::EnterExpr(expr)];

    while let Some(frame) = stack.pop() {
        match frame {
            Frame::EnterExpr(expr) => match &expr.kind {
                ExpressionKind::Diverge { value, .. } => {
                    stack.push(Frame::ExitExpr(ExprCombine::Any(1)));
                    stack.push(Frame::EnterExpr(value));
                }
                ExpressionKind::Block(exprs) => {
                    stack.push(Frame::ExitExpr(ExprCombine::Any(exprs.len())));
                    for expr in exprs.iter().rev() {
                        stack.push(Frame::EnterExpr(expr));
                    }
                }
                ExpressionKind::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    stack.push(Frame::ExitExpr(ExprCombine::Any(2)));
                    stack.push(Frame::EnterExpr(else_branch));
                    stack.push(Frame::EnterExpr(then_branch));
                }
                ExpressionKind::Binding(binding) => {
                    stack.push(Frame::ExitExpr(ExprCombine::Any(2)));
                    stack.push(Frame::EnterPattern(&binding.pattern));
                    stack.push(Frame::EnterExpr(&binding.expr));
                }
                ExpressionKind::Assignment { expr, .. } => {
                    stack.push(Frame::ExitExpr(ExprCombine::Any(1)));
                    stack.push(Frame::EnterExpr(expr));
                }
                ExpressionKind::FunctionCall {
                    function, argument, ..
                } => {
                    stack.push(Frame::ExitExpr(ExprCombine::Any(2)));
                    stack.push(Frame::EnterExpr(argument));
                    stack.push(Frame::EnterExpr(function));
                }
                ExpressionKind::ArrayIndex { array, index } => {
                    stack.push(Frame::ExitExpr(ExprCombine::Any(2)));
                    stack.push(Frame::EnterExpr(index));
                    stack.push(Frame::EnterExpr(array));
                }
                ExpressionKind::Loop { body, .. } => {
                    stack.push(Frame::ExitExpr(ExprCombine::Any(1)));
                    stack.push(Frame::EnterExpr(body));
                }
                ExpressionKind::PropertyAccess { object, .. } => {
                    stack.push(Frame::ExitExpr(ExprCombine::Any(1)));
                    stack.push(Frame::EnterExpr(object));
                }
                ExpressionKind::Operation { left, right, .. } => {
                    stack.push(Frame::ExitExpr(ExprCombine::Any(2)));
                    stack.push(Frame::EnterExpr(right));
                    stack.push(Frame::EnterExpr(left));
                }
                ExpressionKind::AttachImplementation {
                    type_expr,
                    implementation,
                    ..
                } => {
                    stack.push(Frame::ExitExpr(ExprCombine::Any(2)));
                    stack.push(Frame::EnterExpr(implementation));
                    stack.push(Frame::EnterExpr(type_expr));
                }
                ExpressionKind::EnumAccess { enum_expr, .. } => {
                    stack.push(Frame::ExitExpr(ExprCombine::Any(1)));
                    stack.push(Frame::EnterExpr(enum_expr));
                }
                ExpressionKind::EnumValue {
                    enum_type, payload, ..
                } => {
                    stack.push(Frame::ExitExpr(ExprCombine::Any(2)));
                    stack.push(Frame::EnterExpr(payload));
                    stack.push(Frame::EnterExpr(enum_type));
                }
                ExpressionKind::EnumConstructor {
                    enum_type,
                    payload_type,
                    ..
                } => {
                    stack.push(Frame::ExitExpr(ExprCombine::Any(2)));
                    stack.push(Frame::EnterExpr(payload_type));
                    stack.push(Frame::EnterExpr(enum_type));
                }
                ExpressionKind::Match { value, branches } => {
                    stack.push(Frame::ExitExpr(ExprCombine::Match {
                        branch_count: branches.len(),
                    }));
                    for (_, branch) in branches.iter().rev() {
                        stack.push(Frame::EnterExpr(branch));
                    }
                    stack.push(Frame::EnterExpr(value));
                }
                ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(left, right, _)) => {
                    stack.push(Frame::ExitExpr(ExprCombine::Any(2)));
                    stack.push(Frame::EnterExpr(right));
                    stack.push(Frame::EnterExpr(left));
                }
                ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(operand, _)) => {
                    stack.push(Frame::ExitExpr(ExprCombine::Any(1)));
                    stack.push(Frame::EnterExpr(operand));
                }
                ExpressionKind::Literal(_)
                | ExpressionKind::Identifier(_)
                | ExpressionKind::IntrinsicType(_)
                | ExpressionKind::EnumType(_)
                | ExpressionKind::Function { .. }
                | ExpressionKind::FunctionType { .. }
                | ExpressionKind::Struct(_) => results.push(false),
            },
            Frame::ExitExpr(combine) => {
                let result = match combine {
                    ExprCombine::Any(count) => {
                        let mut any = false;
                        for _ in 0..count {
                            if let Some(value) = results.pop() {
                                any |= value;
                            }
                        }
                        any
                    }
                    ExprCombine::Match { branch_count } => {
                        let mut all_branches = true;
                        for _ in 0..branch_count {
                            if let Some(value) = results.pop() {
                                all_branches &= value;
                            }
                        }
                        let value_exports = results.pop().unwrap_or(false);
                        value_exports || all_branches
                    }
                };
                results.push(result);
            }
            Frame::EnterPattern(pattern) => match pattern {
                BindingPattern::Identifier(..) | BindingPattern::Literal(..) => {
                    results.push(false);
                }
                BindingPattern::Struct(items, ..) => {
                    stack.push(Frame::ExitPattern {
                        child_count: items.len(),
                        base: false,
                    });
                    for (_, item) in items.iter().rev() {
                        stack.push(Frame::EnterPattern(item));
                    }
                }
                BindingPattern::EnumVariant {
                    enum_type, payload, ..
                } => {
                    let child_count = 1 + payload.as_ref().map(|_| 1).unwrap_or(0);
                    stack.push(Frame::ExitPattern {
                        child_count,
                        base: false,
                    });
                    if let Some(payload) = payload {
                        stack.push(Frame::EnterPattern(payload));
                    }
                    stack.push(Frame::EnterExpr(enum_type));
                }
                BindingPattern::TypeHint(binding_pattern, expression, ..) => {
                    stack.push(Frame::ExitPattern {
                        child_count: 2,
                        base: false,
                    });
                    stack.push(Frame::EnterPattern(binding_pattern));
                    stack.push(Frame::EnterExpr(expression));
                }
                BindingPattern::Annotated {
                    annotations,
                    pattern,
                    ..
                } => {
                    let base = annotations
                        .iter()
                        .any(|ann| matches!(ann, BindingAnnotation::Export(..)));
                    stack.push(Frame::ExitPattern {
                        child_count: 1,
                        base,
                    });
                    stack.push(Frame::EnterPattern(pattern));
                }
            },
            Frame::ExitPattern { child_count, base } => {
                let mut any_child = false;
                for _ in 0..child_count {
                    if let Some(value) = results.pop() {
                        any_child |= value;
                    }
                }
                results.push(base || any_child);
            }
        }
    }

    results.pop().unwrap_or(false)
}

fn get_assigned_identifiers(expr: &Expression) -> HashSet<Identifier> {
    fold_expression(expr, HashSet::new(), &|current_expr, mut identifiers| {
        if let ExpressionKind::Assignment { target, .. } = &current_expr.kind {
            identifiers.extend(target.get_used_identifiers());
        }
        identifiers
    })
}

fn expression_contains_external_mutation(expr: &Expression, context: &Context) -> bool {
    let assigned = get_assigned_identifiers(expr);

    for identifier in assigned {
        if context.get_identifier(&identifier).is_some() {
            return true;
        }
    }
    false
}

fn pattern_contains_compile_time_data(pattern: &BindingPattern) -> bool {
    let mut stack = vec![pattern];
    while let Some(pattern) = stack.pop() {
        match pattern {
            BindingPattern::Identifier(_, _) => {}
            BindingPattern::Literal(_, _) => return true,
            BindingPattern::Struct(items, _) => {
                for (_, field_pattern) in items.iter() {
                    stack.push(field_pattern);
                }
            }
            BindingPattern::EnumVariant { payload, .. } => {
                if let Some(payload) = payload {
                    stack.push(payload);
                }
            }
            BindingPattern::TypeHint(inner, ty, _) => {
                if type_expression_contains_compile_time_data(ty) {
                    return true;
                }
                stack.push(inner);
            }
            BindingPattern::Annotated { pattern, .. } => {
                stack.push(pattern);
            }
        }
    }
    false
}

fn function_contains_compile_time_data(expr: &Expression) -> bool {
    if let ExpressionKind::Function {
        parameter,
        return_type: Some(return_type),
        ..
    } = &expr.kind
    {
        pattern_contains_compile_time_data(parameter)
            || type_expression_contains_compile_time_data(return_type)
    } else {
        false
    }
}

fn type_expression_contains_compile_time_data(expr: &Expression) -> bool {
    let mut stack = vec![expr];
    while let Some(expr) = stack.pop() {
        match &expr.kind {
            ExpressionKind::Struct(items) => {
                for (_, field_expr) in items.iter() {
                    stack.push(field_expr);
                }
            }
            ExpressionKind::FunctionType { .. } => return true,
            ExpressionKind::AttachImplementation { type_expr, .. } => {
                stack.push(type_expr);
            }
            ExpressionKind::EnumType(cases) => {
                for (_, field_expr) in cases.iter() {
                    stack.push(field_expr);
                }
            }
            ExpressionKind::IntrinsicType(IntrinsicType::Target | IntrinsicType::Type) => {
                return true;
            }
            ExpressionKind::IntrinsicType(
                IntrinsicType::Boolean | IntrinsicType::I32 | IntrinsicType::U8,
            ) => {}
            ExpressionKind::Identifier(_) => return true,
            other => panic!("Unsupported expression {:?} for resolved type", other),
        }
    }
    false
}

fn collect_pattern_value_bindings(
    pattern: &BindingPattern,
    value: &Expression,
    bindings: &mut HashMap<String, Expression>,
) -> Result<(), Diagnostic> {
    let mut stack = vec![(pattern, value)];
    while let Some((pattern, value)) = stack.pop() {
        match pattern {
            BindingPattern::Identifier(identifier, _) => {
                if let Some(existing) = bindings.get(&identifier.name) {
                    if !types_equivalent(&existing.kind, &value.kind) {
                        return Err(diagnostic(
                            format!("Conflicting binding for {}", identifier.name),
                            value.span(),
                        ));
                    }
                } else {
                    bindings.insert(identifier.name.clone(), value.clone());
                }
            }
            BindingPattern::Literal(literal, span) => {
                let matched = match (&value.kind, literal) {
                    (
                        ExpressionKind::Literal(ExpressionLiteral::Number(value)),
                        ExpressionLiteral::Number(pattern_value),
                    ) => *value == *pattern_value,
                    (
                        ExpressionKind::Literal(ExpressionLiteral::Boolean(value)),
                        ExpressionLiteral::Boolean(pattern_value),
                    ) => *value == *pattern_value,
                    (
                        ExpressionKind::Literal(ExpressionLiteral::Char(value)),
                        ExpressionLiteral::Char(pattern_value),
                    ) => *value == *pattern_value,
                    (
                        ExpressionKind::Literal(ExpressionLiteral::String(value)),
                        ExpressionLiteral::String(pattern_value),
                    ) => value == pattern_value,
                    _ => false,
                };
                if !matched {
                    return Err(diagnostic("Pattern literal does not match value", *span));
                }
            }
            BindingPattern::Struct(items, span) => {
                let ExpressionKind::Struct(value_items) = &value.kind else {
                    return Err(diagnostic("Struct pattern requires struct value", *span));
                };
                for (field_id, field_pattern) in items.iter() {
                    let field_value = value_items
                        .iter()
                        .find(|(value_id, _)| value_id.name == field_id.name)
                        .map(|(_, expr)| expr)
                        .ok_or_else(|| {
                            diagnostic(
                                format!("Missing field {}", field_id.name),
                                field_pattern.span(),
                            )
                        })?;
                    stack.push((field_pattern, field_value));
                }
            }
            BindingPattern::EnumVariant {
                enum_type,
                variant,
                payload,
                span,
            } => {
                let ExpressionKind::EnumValue {
                    enum_type: value_enum,
                    variant: value_variant,
                    payload: value_payload,
                    ..
                } = &value.kind
                else {
                    return Err(diagnostic("Enum pattern requires enum value", *span));
                };

                if value_variant.name != variant.name {
                    return Err(diagnostic("Enum variant does not match", *span));
                }
                if !types_equivalent(&enum_type.kind, &value_enum.kind) {
                    return Err(diagnostic("Enum pattern references mismatched type", *span));
                }
                if let Some(payload_pattern) = payload.as_ref() {
                    stack.push((payload_pattern.as_ref(), value_payload));
                }
            }
            BindingPattern::TypeHint(inner, _type_expr, _) => {
                stack.push((inner.as_ref(), value));
            }
            BindingPattern::Annotated { pattern, .. } => {
                stack.push((pattern.as_ref(), value));
            }
        }
    }
    Ok(())
}

fn get_trait_prop_of_type(
    value_type: &Expression,
    trait_prop: &str,
    span: SourceSpan,
    context: &Context,
) -> Result<Expression, Diagnostic> {
    fn get_struct_field(
        items: &[(Identifier, Expression)],
        trait_prop: &str,
    ) -> Option<Expression> {
        items
            .iter()
            .find(|(field_id, _)| field_id.name == trait_prop)
            .map(|(_, expr)| expr.clone())
    }

    let mut current = value_type;
    loop {
        match &current.kind {
            ExpressionKind::Struct(items) => {
                return get_struct_field(items, trait_prop).ok_or_else(|| {
                    diagnostic(format!("Missing field {} on type", trait_prop), span)
                });
            }
            ExpressionKind::AttachImplementation {
                type_expr,
                implementation,
                ..
            } => {
                if let ExpressionKind::Struct(ref items) = implementation.kind
                    && let Some(field) = get_struct_field(items, trait_prop)
                {
                    return Ok(field);
                }

                current = type_expr;
            }
            ExpressionKind::Identifier(identifier) => {
                let (binding, _) = context.get_identifier(identifier).ok_or_else(|| {
                    diagnostic(format!("Unbound identifier: {}", identifier.name), span)
                })?;
                match binding {
                    BindingContext::Bound(value, _, _) => {
                        current = value;
                    }
                    BindingContext::UnboundWithType(type_expr) => {
                        if let Some(trait_expr) = trait_requirement_from_type_hint(type_expr) {
                            let (required_fields, _type_bindings) = resolve_trait_struct(
                                &trait_expr,
                                ExpressionKind::Identifier(identifier.clone()).with_span(span),
                                context,
                                span,
                            )?;
                            return required_fields
                                .iter()
                                .find(|(field_id, _)| field_id.name == trait_prop)
                                .map(|(_, expr)| expr.clone())
                                .ok_or_else(|| {
                                    diagnostic(
                                        format!(
                                            "Type does not implement trait: missing field {}",
                                            trait_prop
                                        ),
                                        span,
                                    )
                                });
                        } else {
                            current = type_expr;
                        }
                    }
                    BindingContext::UnboundWithoutType => {
                        return Err(diagnostic(
                            format!(
                                "Cannot determine type of unbound identifier: {}",
                                identifier.name
                            ),
                            span,
                        ));
                    }
                }
            }
            ty => {
                return Err(diagnostic(
                    format!(
                        "Unsupported value type {:?} for `{}` operator lookup",
                        ty, trait_prop
                    ),
                    span,
                ));
            }
        }
    }
}

fn is_intrinsic_type_expr(expr: &Expression, intrinsic: IntrinsicType) -> bool {
    let mut current = expr;
    loop {
        match &current.kind {
            ExpressionKind::IntrinsicType(found) => return *found == intrinsic,
            ExpressionKind::AttachImplementation { type_expr, .. } => {
                current = type_expr;
            }
            _ => return false,
        }
    }
}

fn trait_requirement_from_type_hint(type_expr: &Expression) -> Option<Expression> {
    match &type_expr.kind {
        ExpressionKind::AttachImplementation {
            type_expr,
            implementation,
        } if is_intrinsic_type_expr(type_expr, IntrinsicType::Type)
            && !matches!(implementation.kind, ExpressionKind::Struct(_)) =>
        {
            Some(*implementation.clone())
        }
        _ => None,
    }
}

fn ensure_trait_requirements(
    type_value: &Expression,
    trait_expr: &Expression,
    context: &Context,
    span: SourceSpan,
) -> Result<(), Diagnostic> {
    let type_value_type = get_type_of_expression(type_value, context)?;
    if !matches!(
        type_value_type.kind,
        ExpressionKind::IntrinsicType(IntrinsicType::Type)
    ) && !is_type_expression(&type_value_type.kind)
    {
        return Err(diagnostic(
            "Trait constraints require a type argument",
            span,
        ));
    }

    let evaluated_type = resolve_type_alias_expression(type_value, context);
    let (required_fields, type_bindings) =
        resolve_trait_struct(trait_expr, evaluated_type.clone(), context, span)?;

    for (field_id, expected_field_value) in required_fields {
        let field_value = get_trait_prop_of_type(&evaluated_type, &field_id.name, span, context)
            .map_err(|_| {
                diagnostic(
                    format!(
                        "Type does not implement trait: missing field {}",
                        field_id.name
                    ),
                    span,
                )
            })?;
        let actual_field_type = get_type_of_expression(&field_value, context)?;
        let expected_field_type = apply_type_bindings(&expected_field_value, &type_bindings);
        let expected_field_type = resolve_type_alias_expression(&expected_field_type, context);
        let actual_field_type = resolve_type_alias_expression(&actual_field_type, context);

        if !types_equivalent(&actual_field_type.kind, &expected_field_type.kind) {
            return Err(diagnostic(
                format!("Trait field {} has mismatched type", field_id.name),
                span,
            ));
        }
    }

    Ok(())
}

fn resolve_trait_struct(
    trait_expr: &Expression,
    type_arg: Expression,
    context: &Context,
    span: SourceSpan,
) -> Result<(Vec<(Identifier, Expression)>, HashMap<String, Expression>), Diagnostic> {
    let evaluated_trait = if let ExpressionKind::Identifier(identifier) = &trait_expr.kind {
        context
            .get_identifier(identifier)
            .and_then(|binding| match &binding.0 {
                BindingContext::Bound(value, _, _) => Some(value.clone()),
                _ => None,
            })
            .unwrap_or_else(|| trait_expr.clone())
    } else {
        trait_expr.clone()
    };
    let ExpressionKind::Function {
        parameter, body, ..
    } = evaluated_trait.kind
    else {
        return Err(diagnostic("Trait must be a function", span));
    };

    let mut type_bindings = HashMap::new();
    collect_pattern_value_bindings(&parameter, &type_arg, &mut type_bindings)?;
    let required_struct = apply_type_bindings(&body, &type_bindings);

    let ExpressionKind::Struct(required_fields) = required_struct.kind else {
        return Err(diagnostic("Trait must return a struct type", span));
    };

    Ok((required_fields, type_bindings))
}

fn resolve_type_alias_expression(expr: &Expression, context: &Context) -> Expression {
    enum Frame {
        Enter(Expression),
        Struct {
            span: SourceSpan,
            identifiers: Vec<Identifier>,
        },
        EnumType {
            span: SourceSpan,
            identifiers: Vec<Identifier>,
        },
        FunctionType {
            span: SourceSpan,
        },
        AttachImplementation {
            span: SourceSpan,
        },
    }

    let mut stack = vec![Frame::Enter(expr.clone())];
    let mut results: Vec<Expression> = Vec::new();

    while let Some(frame) = stack.pop() {
        match frame {
            Frame::Enter(expr) => match expr.kind {
                ExpressionKind::Identifier(identifier) => {
                    if let Some((BindingContext::Bound(value, _, _), _)) =
                        context.get_identifier(&identifier)
                        && is_type_expression(&value.kind)
                    {
                        results.push(value.clone());
                    } else {
                        results.push(Expression::new(
                            ExpressionKind::Identifier(identifier),
                            expr.span,
                        ));
                    }
                }
                ExpressionKind::Struct(items) => {
                    let identifiers = items.iter().map(|(id, _)| id.clone()).collect();
                    stack.push(Frame::Struct {
                        span: expr.span,
                        identifiers,
                    });
                    for (_, value) in items.into_iter().rev() {
                        stack.push(Frame::Enter(value));
                    }
                }
                ExpressionKind::FunctionType {
                    parameter,
                    return_type,
                } => {
                    stack.push(Frame::FunctionType { span: expr.span });
                    stack.push(Frame::Enter(*return_type));
                    stack.push(Frame::Enter(*parameter));
                }
                ExpressionKind::EnumType(variants) => {
                    let identifiers = variants.iter().map(|(id, _)| id.clone()).collect();
                    stack.push(Frame::EnumType {
                        span: expr.span,
                        identifiers,
                    });
                    for (_, value) in variants.into_iter().rev() {
                        stack.push(Frame::Enter(value));
                    }
                }
                ExpressionKind::AttachImplementation {
                    type_expr,
                    implementation,
                } => {
                    stack.push(Frame::AttachImplementation { span: expr.span });
                    stack.push(Frame::Enter(*implementation));
                    stack.push(Frame::Enter(*type_expr));
                }
                other => {
                    results.push(Expression::new(other, expr.span));
                }
            },
            Frame::Struct { span, identifiers } => {
                let mut resolved = Vec::with_capacity(identifiers.len());
                for _ in 0..identifiers.len() {
                    resolved.push(results.pop().unwrap());
                }
                resolved.reverse();
                let items = identifiers.into_iter().zip(resolved.into_iter()).collect();
                results.push(Expression::new(ExpressionKind::Struct(items), span));
            }
            Frame::EnumType { span, identifiers } => {
                let mut resolved = Vec::with_capacity(identifiers.len());
                for _ in 0..identifiers.len() {
                    resolved.push(results.pop().unwrap());
                }
                resolved.reverse();
                let variants = identifiers.into_iter().zip(resolved.into_iter()).collect();
                results.push(Expression::new(ExpressionKind::EnumType(variants), span));
            }
            Frame::FunctionType { span } => {
                let return_type = results.pop().unwrap();
                let parameter = results.pop().unwrap();
                results.push(
                    ExpressionKind::FunctionType {
                        parameter: Box::new(parameter),
                        return_type: Box::new(return_type),
                    }
                    .with_span(span),
                );
            }
            Frame::AttachImplementation { span } => {
                let implementation = results.pop().unwrap();
                let type_expr = results.pop().unwrap();
                results.push(
                    ExpressionKind::AttachImplementation {
                        type_expr: Box::new(type_expr),
                        implementation: Box::new(implementation),
                    }
                    .with_span(span),
                );
            }
        }
    }

    results.pop().unwrap_or_else(|| expr.clone())
}

fn collect_bound_identifiers_from_pattern(
    pattern: &BindingPattern,
    bound: &mut HashSet<Identifier>,
) {
    let mut stack = vec![pattern];
    while let Some(pattern) = stack.pop() {
        match pattern {
            BindingPattern::Identifier(identifier, _) => {
                bound.insert(identifier.clone());
            }
            BindingPattern::Struct(items, _) => {
                for (_, sub_pattern) in items.iter() {
                    stack.push(sub_pattern);
                }
            }
            BindingPattern::EnumVariant { payload, .. } => {
                if let Some(payload) = payload {
                    stack.push(payload);
                }
            }
            BindingPattern::TypeHint(inner, _, _) => {
                stack.push(inner);
            }
            BindingPattern::Annotated { pattern, .. } => {
                stack.push(pattern);
            }
            BindingPattern::Literal(_, _) => {}
        }
    }
}

fn fold_expression<T, U: Fn(&Expression, T) -> T>(
    expr: &Expression,
    init: T,
    item_processor: &U,
) -> T {
    let mut state = init;
    let mut stack = vec![expr];
    while let Some(expr) = stack.pop() {
        state = item_processor(expr, state);
        match &expr.kind {
            ExpressionKind::IntrinsicType(..)
            | ExpressionKind::Literal(..)
            | ExpressionKind::Identifier(..) => {}
            ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(left, right, ..)) => {
                stack.push(right);
                stack.push(left);
            }
            ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(operand, ..)) => {
                stack.push(operand);
            }
            ExpressionKind::EnumType(items) => {
                for (_, field_expr) in items.iter().rev() {
                    stack.push(field_expr);
                }
            }
            ExpressionKind::Match {
                value, branches, ..
            } => {
                for (_, branch) in branches.iter().rev() {
                    stack.push(branch);
                }
                stack.push(value);
            }
            ExpressionKind::EnumValue {
                enum_type, payload, ..
            } => {
                stack.push(payload);
                stack.push(enum_type);
            }
            ExpressionKind::EnumConstructor {
                enum_type,
                payload_type,
                ..
            } => {
                stack.push(payload_type);
                stack.push(enum_type);
            }
            ExpressionKind::EnumAccess { enum_expr, .. } => {
                stack.push(enum_expr);
            }
            ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                stack.push(else_branch);
                stack.push(then_branch);
                stack.push(condition);
            }
            ExpressionKind::AttachImplementation {
                type_expr,
                implementation,
                ..
            } => {
                stack.push(implementation);
                stack.push(type_expr);
            }
            ExpressionKind::Function {
                return_type, body, ..
            } => {
                stack.push(body);
                if let Some(ret_type) = return_type {
                    stack.push(ret_type);
                }
            }
            ExpressionKind::FunctionType {
                parameter,
                return_type,
                ..
            } => {
                stack.push(return_type);
                stack.push(parameter);
            }
            ExpressionKind::Struct(items) => {
                for (_, field_expr) in items.iter().rev() {
                    stack.push(field_expr);
                }
            }
            ExpressionKind::Operation { left, right, .. } => {
                stack.push(right);
                stack.push(left);
            }
            ExpressionKind::Assignment { expr, .. } => {
                stack.push(expr);
            }
            ExpressionKind::FunctionCall {
                function, argument, ..
            } => {
                stack.push(argument);
                stack.push(function);
            }
            ExpressionKind::ArrayIndex { array, index } => {
                stack.push(index);
                stack.push(array);
            }
            ExpressionKind::PropertyAccess { object, .. } => {
                stack.push(object);
            }
            ExpressionKind::Binding(binding) => {
                stack.push(&binding.expr);
            }
            ExpressionKind::Block(expressions) => {
                for expr in expressions.iter().rev() {
                    stack.push(expr);
                }
            }
            ExpressionKind::Diverge { value, .. } => {
                stack.push(value);
            }
            ExpressionKind::Loop { body, .. } => {
                stack.push(body);
            }
        }
    }
    state
}

fn identifiers_created_or_modified(expr: &Expression) -> HashSet<Identifier> {
    fold_expression(
        expr,
        HashSet::new(),
        &|expr, mut identifiers| match &expr.kind {
            ExpressionKind::Binding(binding) => {
                collect_bound_identifiers_from_pattern(&binding.pattern, &mut identifiers);
                identifiers
            }
            ExpressionKind::Assignment { target, .. } => {
                identifiers.extend(target.get_used_identifiers());
                identifiers
            }
            _ => identifiers,
        },
    )
}

fn identifiers_used(expr: &Expression) -> HashSet<Identifier> {
    fold_expression(expr, HashSet::new(), &|current_expr, mut used| {
        match &current_expr.kind {
            ExpressionKind::Identifier(identifier) => {
                used.insert(identifier.clone());
            }
            ExpressionKind::Assignment { target, .. } => {
                used.extend(target.get_used_identifiers());
            }
            _ => {}
        }
        used
    })
}

fn interpret_block(
    expressions: Vec<Expression>,
    span: SourceSpan,
    context: &mut Context,
) -> Result<(Expression, Context), Diagnostic> {
    let outer_context = context.clone();
    context.bindings.push(HashMap::new());
    let mut interpreted_expressions = Vec::new();
    let mut preserved_expression_indicies = HashSet::new();

    for (expr_idx, expression) in expressions.into_iter().enumerate() {
        let value = interpret_expression(expression, context)?;
        if expression_contains_external_mutation(&value, &outer_context)
            || expression_does_diverge(&value, true, false)
            || expression_exports(&value)
        {
            preserved_expression_indicies.insert(expr_idx);
        }

        if matches!(
            value.kind,
            ExpressionKind::Diverge {
                divergance_type: DivergeExpressionType::Break | DivergeExpressionType::Return,
                ..
            }
        ) {
            let block_context = context.clone();
            context.bindings.pop();
            return Ok((value, block_context));
        }
        interpreted_expressions.push(value);
    }

    preserved_expression_indicies.insert(interpreted_expressions.len() - 1);

    let expression_usage: Vec<HashSet<Identifier>> = interpreted_expressions
        .iter()
        .map(identifiers_used)
        .collect();

    let expression_modifications: Vec<HashSet<Identifier>> = interpreted_expressions
        .iter()
        .map(identifiers_created_or_modified)
        .collect();

    let mut needed_identifiers: HashSet<Identifier> = HashSet::new();
    for idx in (0..interpreted_expressions.len()).rev() {
        let mut preserve_current = preserved_expression_indicies.contains(&idx);
        if !preserve_current
            && expression_modifications[idx]
                .iter()
                .any(|identifier| needed_identifiers.contains(identifier))
        {
            preserved_expression_indicies.insert(idx);
            preserve_current = true;
        }

        if preserve_current {
            // for identifier in &expression_modifications[idx] {
            //     needed_identifiers.remove(identifier);
            // }

            for identifier in &expression_usage[idx] {
                needed_identifiers.insert(identifier.clone());
            }
        }
    }

    let block_context = context.clone();
    context.bindings.pop();
    if preserved_expression_indicies.len() == 1 {
        // Only last expression preserved
        Ok((
            interpreted_expressions.into_iter().last().unwrap(),
            block_context,
        ))
    } else {
        // If we have preserved bindings, we need to return a Block that contains them
        // AND the final value.
        let preserved_expressions = interpreted_expressions
            .into_iter()
            .enumerate()
            .filter(|(idx, _)| preserved_expression_indicies.contains(idx))
            .map(|(_, expr)| expr)
            .collect();

        Ok((
            Expression::new(ExpressionKind::Block(preserved_expressions), span),
            block_context,
        ))
    }
}

pub fn interpret_program(
    expr: Expression,
    context: &mut Context,
) -> Result<(Expression, Context), Diagnostic> {
    match expr {
        Expression {
            kind: ExpressionKind::Block(expressions),
            span,
        } => interpret_block(expressions, span, context),
        other => interpret_expression(other, context).map(|value| (value, context.clone())),
    }
}

fn ensure_lvalue_mutable(
    target: &LValue,
    context: &Context,
    span: SourceSpan,
) -> Result<(), Diagnostic> {
    let mut current = target;
    loop {
        match current {
            LValue::Identifier(identifier, target_span) => {
                let (_, annotations) = context.get_identifier(identifier).ok_or_else(|| {
                    diagnostic(
                        format!("Cannot assign to unbound identifier: {}", identifier.name),
                        *target_span,
                    )
                })?;

                if !annotations
                    .iter()
                    .any(|ann| matches!(ann, BindingAnnotation::Mutable(_)))
                {
                    return Err(diagnostic(
                        format!("Cannot assign to immutable identifier: {}", identifier.name),
                        span,
                    ));
                }
                return Ok(());
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

fn lvalue_display_name(lvalue: &LValue) -> String {
    match lvalue {
        LValue::Identifier(Identifier { name, .. }, _) => name.clone(),
        LValue::PropertyAccess {
            object, property, ..
        } => format!("{}.{}", lvalue_display_name(object), property),
        LValue::ArrayIndex { array, index, .. } => {
            format!("{}({})", lvalue_display_name(array), index.pretty_print())
        }
    }
}

fn get_lvalue_type(
    target: &LValue,
    context: &Context,
    span: SourceSpan,
) -> Result<Expression, Diagnostic> {
    ensure_lvalue_mutable(target, context, span)?;
    fn resolve_identifier_type(
        identifier: &Identifier,
        target_span: SourceSpan,
        context: &Context,
    ) -> Result<Expression, Diagnostic> {
        let (binding_ctx, _) = context.get_identifier(identifier).ok_or_else(|| {
            diagnostic(
                format!("Cannot assign to unbound identifier: {}", identifier.name),
                target_span,
            )
        })?;

        match binding_ctx {
            BindingContext::Bound(_, _, Some(bound_type)) => Ok(bound_type.clone()),
            BindingContext::Bound(value, _, None) => get_type_of_expression(value, context),
            BindingContext::UnboundWithType(type_expr) => Ok(type_expr.clone()),
            BindingContext::UnboundWithoutType => Err(diagnostic(
                format!("Cannot determine type of {}", identifier.name),
                target_span,
            )),
        }
    }

    match target {
        LValue::Identifier(identifier, target_span) => {
            resolve_identifier_type(identifier, *target_span, context)
        }
        LValue::PropertyAccess {
            object,
            property,
            span: prop_span,
        } => {
            let current_type = get_lvalue_type(object, context, *prop_span)?;
            let Expression {
                kind: ExpressionKind::Struct(fields),
                ..
            } = current_type
            else {
                return Err(diagnostic("Property access on non-struct type", *prop_span));
            };

            fields
                .iter()
                .find(|(id, _)| id.name == *property)
                .map(|(_, ty)| ty.clone())
                .ok_or_else(|| {
                    diagnostic(
                        format!("Field {} not found in struct type", property),
                        *prop_span,
                    )
                })
        }
        LValue::ArrayIndex { array, index, span } => {
            let array_type = get_lvalue_type(array, context, *span)?;
            let Some(element_type) = homogeneous_struct_element_type(&array_type, context) else {
                return Err(diagnostic("Indexing requires an array type", *span));
            };
            let index_type = get_type_of_expression(index, context)?;
            let expected_index = ExpressionKind::IntrinsicType(IntrinsicType::I32);
            if !types_equivalent(&index_type.kind, &expected_index) {
                return Err(diagnostic("Array index must be an i32 value", *span));
            }
            Ok(element_type)
        }
    }
}

fn get_lvalue_value(
    target: &LValue,
    context: &mut Context,
) -> Result<Option<Expression>, Diagnostic> {
    match target {
        LValue::Identifier(identifier, target_span) => {
            let (binding_ctx, _) = context.get_identifier(identifier).ok_or_else(|| {
                diagnostic(
                    format!("Cannot assign to unbound identifier: {}", identifier.name),
                    *target_span,
                )
            })?;

            match binding_ctx {
                BindingContext::Bound(value, _, _) => {
                    if is_resolved_constant(value) {
                        Ok(Some(value.clone()))
                    } else {
                        Ok(None)
                    }
                }
                BindingContext::UnboundWithType(_) => Ok(None),
                BindingContext::UnboundWithoutType => Err(diagnostic(
                    format!(
                        "Cannot assign to uninitialized identifier: {}",
                        identifier.name
                    ),
                    *target_span,
                )),
            }
        }
        LValue::PropertyAccess {
            object,
            property,
            span: prop_span,
        } => {
            let Some(object_value) = get_lvalue_value(object, context)? else {
                return Ok(None);
            };
            let Expression {
                kind: ExpressionKind::Struct(fields),
                span: struct_span,
            } = object_value
            else {
                return Err(diagnostic(
                    "Property access on non-struct value",
                    *prop_span,
                ));
            };

            fields
                .into_iter()
                .find(|(id, _)| id.name == *property)
                .map(|(_, expr)| Some(expr))
                .ok_or_else(|| {
                    diagnostic(format!("Missing field {} in struct", property), struct_span)
                })
        }
        LValue::ArrayIndex { array, index, span } => {
            let Some(array_value) = get_lvalue_value(array, context)? else {
                return Ok(None);
            };
            let ExpressionKind::Literal(ExpressionLiteral::Number(index)) = index.kind else {
                return Ok(None);
            };
            if index < 0 {
                return Err(diagnostic("Array index out of range", *span));
            }
            let index = index as usize;
            if let ExpressionKind::Literal(ExpressionLiteral::String(bytes)) = array_value.kind {
                return bytes
                    .get(index)
                    .copied()
                    .map(|value| {
                        Some(
                            ExpressionKind::Literal(ExpressionLiteral::Char(value))
                                .with_span(array_value.span),
                        )
                    })
                    .ok_or_else(|| diagnostic("Array index out of range", array_value.span));
            }
            let Expression {
                kind: ExpressionKind::Struct(fields),
                span: struct_span,
            } = array_value
            else {
                return Err(diagnostic("Indexing requires an array value", *span));
            };
            fields
                .into_iter()
                .nth(index)
                .map(|(_, expr)| Some(expr))
                .ok_or_else(|| diagnostic("Array index out of range", struct_span))
        }
    }
}

fn apply_lvalue_update(
    target: &LValue,
    value: Expression,
    context: &mut Context,
    span: SourceSpan,
) -> Result<(), Diagnostic> {
    let update_identifier = |identifier: &Identifier,
                             value: Expression,
                             context: &mut Context,
                             span: SourceSpan|
     -> Result<(), Diagnostic> {
        let value_type = get_type_of_expression(&value, context).ok();
        let type_context = context.clone();

        let Some((binding_ctx, _annotations)) = context.get_mut_identifier(identifier) else {
            return Err(diagnostic(
                format!("Cannot assign to unbound identifier: {}", identifier.name),
                span,
            ));
        };

        let (expected_type, bound_type) = match binding_ctx {
            BindingContext::Bound(existing, _, bound_type) => (
                get_type_of_expression(existing, &type_context).ok(),
                bound_type.clone(),
            ),
            BindingContext::UnboundWithType(expected_ty) => {
                (Some(expected_ty.clone()), Some(expected_ty.clone()))
            }
            BindingContext::UnboundWithoutType => (None, None),
        };

        if let (Some(expected_ty), Some(actual_ty)) = (&expected_type, &value_type)
            && !types_equivalent(&expected_ty.kind, &actual_ty.kind)
        {
            return Err(diagnostic(
                format!(
                    "Cannot assign value of mismatched type to {}",
                    identifier.name
                ),
                span,
            ));
        }

        let binding_type = expected_type.or(value_type);

        if is_resolved_constant(&value) {
            *binding_ctx = BindingContext::Bound(value, PreserveBehavior::Inline, bound_type);
        } else if let Some(binding_ty) = binding_type {
            *binding_ctx = BindingContext::UnboundWithType(binding_ty);
        } else {
            *binding_ctx = BindingContext::UnboundWithoutType;
        }

        Ok(())
    };

    match target {
        LValue::Identifier(identifier, _) => {
            update_identifier(identifier, value, context, span)?;
            Ok(())
        }
        LValue::PropertyAccess { .. } | LValue::ArrayIndex { .. } => {
            enum AccessStep {
                Property(String, SourceSpan),
                Index(usize, SourceSpan),
                DynamicIndex(SourceSpan),
            }

            let mut steps: Vec<AccessStep> = Vec::new();
            let mut current = target;
            let (base_identifier, base_span) = loop {
                match current {
                    LValue::Identifier(identifier, span) => break (identifier.clone(), *span),
                    LValue::PropertyAccess {
                        object,
                        property,
                        span: prop_span,
                    } => {
                        steps.push(AccessStep::Property(property.clone(), *prop_span));
                        current = object;
                    }
                    LValue::ArrayIndex { array, index, span } => {
                        match index.kind {
                            ExpressionKind::Literal(ExpressionLiteral::Number(idx)) if idx >= 0 => {
                                steps.push(AccessStep::Index(idx as usize, *span));
                            }
                            _ => {
                                steps.push(AccessStep::DynamicIndex(*span));
                            }
                        }
                        current = array;
                    }
                }
            };

            if steps
                .iter()
                .any(|step| matches!(step, AccessStep::DynamicIndex(_)))
            {
                for invalidated_identifier in target.get_used_identifiers() {
                    let binding_to_invalidate =
                        context.get_identifier(&invalidated_identifier).unwrap();
                    if let Some(binding_ty) = binding_to_invalidate.0.get_bound_type(context)? {
                        let binding_to_invalidate =
                            context.get_mut_identifier(&invalidated_identifier).unwrap();
                        binding_to_invalidate.0 = BindingContext::UnboundWithType(binding_ty)
                    }
                }
                return Ok(());
            }

            let base_target = LValue::Identifier(base_identifier.clone(), base_span);
            let Some(mut current_object) = get_lvalue_value(&base_target, context)? else {
                for invalidated_identifier in target.get_used_identifiers() {
                    let binding_to_invalidate =
                        context.get_identifier(&invalidated_identifier).unwrap();
                    if let Some(binding_ty) = binding_to_invalidate.0.get_bound_type(context)? {
                        let binding_to_invalidate =
                            context.get_mut_identifier(&invalidated_identifier).unwrap();
                        binding_to_invalidate.0 = BindingContext::UnboundWithType(binding_ty)
                    }
                }
                return Ok(());
            };

            enum UpdateStep {
                Property(String),
                Index(usize),
            }

            let mut struct_stack: Vec<(Vec<(Identifier, Expression)>, SourceSpan, UpdateStep)> =
                Vec::new();
            for step in steps.into_iter().rev() {
                let Expression {
                    kind: ExpressionKind::Struct(fields),
                    span: struct_span,
                } = current_object
                else {
                    return Err(diagnostic("Property access on non-struct value", span));
                };

                let (field_value, update_step) = match step {
                    AccessStep::Property(property, _prop_span) => {
                        let mut found = None;
                        for (field_id, field_expr) in fields.iter() {
                            if field_id.name == property {
                                found = Some(field_expr.clone());
                                break;
                            }
                        }
                        let field_value = found.ok_or_else(|| {
                            diagnostic(format!("Missing field {} in struct", property), struct_span)
                        })?;
                        (field_value, UpdateStep::Property(property))
                    }
                    AccessStep::Index(index, _) => {
                        let field_value = fields
                            .get(index)
                            .map(|(_, expr)| expr.clone())
                            .ok_or_else(|| diagnostic("Array index out of range", struct_span))?;
                        (field_value, UpdateStep::Index(index))
                    }
                    AccessStep::DynamicIndex(_) => unreachable!("dynamic index handled earlier"),
                };

                struct_stack.push((fields, struct_span, update_step));
                current_object = field_value;
            }

            let mut updated_value = value;
            while let Some((mut fields, struct_span, step)) = struct_stack.pop() {
                match step {
                    UpdateStep::Property(property) => {
                        let mut found = false;
                        for (field_id, field_expr) in fields.iter_mut() {
                            if field_id.name == property {
                                *field_expr = updated_value.clone();
                                found = true;
                                break;
                            }
                        }
                        if !found {
                            return Err(diagnostic(
                                format!("Missing field {} in struct", property),
                                struct_span,
                            ));
                        }
                    }
                    UpdateStep::Index(index) => {
                        if let Some((_, field_expr)) = fields.get_mut(index) {
                            *field_expr = updated_value.clone();
                        } else {
                            return Err(diagnostic("Array index out of range", struct_span));
                        }
                    }
                }
                updated_value = Expression::new(ExpressionKind::Struct(fields), struct_span);
            }

            update_identifier(&base_identifier, updated_value, context, span)
        }
    }
}

fn apply_assignment(
    target: LValue,
    value: Expression,
    span: SourceSpan,
    context: &mut Context,
) -> Result<Expression, Diagnostic> {
    ensure_lvalue_mutable(&target, context, span)?;

    let value_type = get_type_of_expression(&value, context).ok();
    let target_type = get_lvalue_type(&target, context, span)?;

    if let Some(actual_type) = value_type
        && !types_equivalent(&target_type.kind, &actual_type.kind)
    {
        return Err(diagnostic(
            format!(
                "Cannot assign value of mismatched type to {}",
                lvalue_display_name(&target)
            ),
            span,
        ));
    }

    apply_lvalue_update(&target, value.clone(), context, span)?;

    Ok(Expression::new(
        ExpressionKind::Assignment {
            target,
            expr: Box::new(value),
        },
        span,
    ))
}

fn pattern_has_mutable_annotation(pattern: &BindingPattern) -> bool {
    let mut stack = vec![pattern];
    while let Some(pattern) = stack.pop() {
        match pattern {
            BindingPattern::Annotated {
                annotations,
                pattern,
                ..
            } => {
                if annotations
                    .iter()
                    .any(|ann| matches!(ann, BindingAnnotation::Mutable(_)))
                {
                    return true;
                }
                stack.push(pattern);
            }
            BindingPattern::Struct(items, _) => {
                for (_, pat) in items.iter() {
                    stack.push(pat);
                }
            }
            BindingPattern::EnumVariant { payload, .. } => {
                if let Some(payload) = payload.as_ref() {
                    stack.push(payload);
                }
            }
            BindingPattern::TypeHint(inner, _, _) => {
                stack.push(inner);
            }
            BindingPattern::Identifier(..) | BindingPattern::Literal(..) => {}
        }
    }
    false
}

fn bind_pattern_blanks(
    pattern: BindingPattern,
    context: &mut Context,
    passed_annotations: Vec<BindingAnnotation>,
    type_hint: Option<Expression>,
) -> Result<(), Diagnostic> {
    struct Frame {
        pattern: BindingPattern,
        passed_annotations: Vec<BindingAnnotation>,
        type_hint: Option<Expression>,
    }

    let mut stack = vec![Frame {
        pattern,
        passed_annotations,
        type_hint,
    }];

    while let Some(frame) = stack.pop() {
        match frame.pattern {
            BindingPattern::Identifier(identifier, _) => {
                if let Some(type_expr) = frame.type_hint {
                    context.bindings.last_mut().unwrap().insert(
                        identifier,
                        (
                            BindingContext::UnboundWithType(type_expr),
                            frame.passed_annotations.clone(),
                        ),
                    );
                } else {
                    context.bindings.last_mut().unwrap().insert(
                        identifier,
                        (
                            BindingContext::UnboundWithoutType,
                            frame.passed_annotations.clone(),
                        ),
                    );
                }
            }
            BindingPattern::Literal(_, _) => {}
            BindingPattern::Struct(pattern_items, _) => {
                let mut type_lookup = None;
                if let Some(Expression {
                    kind: ExpressionKind::Struct(type_fields),
                    ..
                }) = &frame.type_hint
                {
                    type_lookup = Some(type_fields.clone());
                }

                for (field_identifier, field_pattern) in pattern_items.into_iter().rev() {
                    let field_type_hint = type_lookup.as_ref().and_then(|fields| {
                        fields
                            .iter()
                            .find(|(name, _)| name.name == field_identifier.name)
                            .map(|(_, ty)| ty.clone())
                    });
                    stack.push(Frame {
                        pattern: field_pattern,
                        passed_annotations: frame.passed_annotations.clone(),
                        type_hint: field_type_hint,
                    });
                }
            }
            BindingPattern::EnumVariant {
                enum_type,
                variant,
                payload,
                ..
            } => {
                let type_hint = frame
                    .type_hint
                    .or_else(|| resolve_enum_type_expression(&enum_type, context));
                let payload_hint = type_hint
                    .as_ref()
                    .and_then(|hint| enum_variant_info(hint, &variant).map(|(_, ty)| ty));

                if let Some(payload_pattern) = payload {
                    stack.push(Frame {
                        pattern: *payload_pattern,
                        passed_annotations: frame.passed_annotations,
                        type_hint: payload_hint,
                    });
                }
            }
            BindingPattern::TypeHint(inner, type_hint, _) => {
                stack.push(Frame {
                    pattern: *inner,
                    passed_annotations: frame.passed_annotations,
                    type_hint: Some(*type_hint),
                });
            }
            BindingPattern::Annotated {
                pattern,
                annotations,
                ..
            } => {
                let combined_annotations = frame
                    .passed_annotations
                    .into_iter()
                    .chain(annotations)
                    .collect();
                stack.push(Frame {
                    pattern: *pattern,
                    passed_annotations: combined_annotations,
                    type_hint: frame.type_hint,
                });
            }
        }
    }

    Ok(())
}

fn value_preserve_behavior(
    value: &Expression,
    preserve_behavior: PreserveBehavior,
) -> PreserveBehavior {
    if let ExpressionKind::Function { parameter, .. } = &value.kind
        && pattern_has_mutable_annotation(parameter)
    {
        return PreserveBehavior::PreserveUsage;
    }
    preserve_behavior
}

fn bind_pattern_from_value(
    pattern: BindingPattern,
    value: &Expression,
    context: &mut Context,
    passed_annotations: Vec<BindingAnnotation>,
    preserve_behavior: PreserveBehavior,
    bound_type: Option<Expression>,
) -> Result<(bool, PreserveBehavior), Diagnostic> {
    enum Frame<'a> {
        Enter {
            pattern: BindingPattern,
            value: &'a Expression,
            passed_annotations: Vec<BindingAnnotation>,
            preserve_behavior: PreserveBehavior,
            bound_type: Option<Expression>,
        },
        StructContinue {
            items: Vec<(Identifier, BindingPattern)>,
            value: &'a Expression,
            index: usize,
            preserve_behavior: PreserveBehavior,
            overall_matched: bool,
            passed_annotations: Vec<BindingAnnotation>,
        },
    }

    let mut stack = vec![Frame::Enter {
        pattern,
        value,
        passed_annotations,
        preserve_behavior,
        bound_type,
    }];
    let mut results: Vec<(bool, PreserveBehavior)> = Vec::new();

    while let Some(frame) = stack.pop() {
        match frame {
            Frame::Enter {
                pattern,
                value,
                passed_annotations,
                preserve_behavior,
                bound_type,
            } => match pattern {
                BindingPattern::Identifier(identifier, _) => {
                    let new_preserve_behavior = value_preserve_behavior(value, preserve_behavior);
                    context.bindings.last_mut().unwrap().insert(
                        identifier,
                        (
                            BindingContext::Bound(value.clone(), new_preserve_behavior, bound_type),
                            passed_annotations.clone(),
                        ),
                    );
                    results.push((true, new_preserve_behavior));
                }
                BindingPattern::Literal(literal, _) => {
                    let matched = match (literal, &value.kind) {
                        (
                            ExpressionLiteral::Number(pattern_value),
                            ExpressionKind::Literal(ExpressionLiteral::Number(value)),
                        ) => pattern_value == *value,
                        (
                            ExpressionLiteral::Boolean(pattern_value),
                            ExpressionKind::Literal(ExpressionLiteral::Boolean(value)),
                        ) => pattern_value == *value,
                        (
                            ExpressionLiteral::Char(pattern_value),
                            ExpressionKind::Literal(ExpressionLiteral::Char(value)),
                        ) => pattern_value == *value,
                        (
                            ExpressionLiteral::String(pattern_value),
                            ExpressionKind::Literal(ExpressionLiteral::String(value)),
                        ) => pattern_value == *value,
                        _ => false,
                    };
                    results.push((matched, preserve_behavior));
                }
                BindingPattern::Struct(pattern_items, span) => {
                    let ExpressionKind::Struct(_) = &value.kind else {
                        return Err(diagnostic("Struct pattern requires struct value", span));
                    };
                    if pattern_items.is_empty() {
                        results.push((true, preserve_behavior));
                        continue;
                    }
                    let items = pattern_items;
                    let (field_identifier, field_pattern) = items[0].clone();
                    let ExpressionKind::Struct(struct_items) = &value.kind else {
                        return Err(diagnostic("Struct pattern requires struct value", span));
                    };
                    let field_span = field_pattern.span();
                    let field_value = struct_items
                        .iter()
                        .find(|(value_identifier, _)| {
                            value_identifier.name == field_identifier.name
                        })
                        .map(|(_, expr)| expr)
                        .ok_or_else(|| {
                            diagnostic(
                                format!("Missing field {}", field_identifier.name),
                                field_span,
                            )
                        })?;
                    stack.push(Frame::StructContinue {
                        items,
                        value,
                        index: 0,
                        preserve_behavior,
                        overall_matched: true,
                        passed_annotations: passed_annotations.clone(),
                    });
                    stack.push(Frame::Enter {
                        pattern: field_pattern,
                        value: field_value,
                        passed_annotations,
                        preserve_behavior,
                        bound_type: None,
                    });
                }
                BindingPattern::EnumVariant {
                    enum_type,
                    variant,
                    payload,
                    span,
                } => {
                    let Expression {
                        kind:
                            ExpressionKind::EnumValue {
                                enum_type: value_enum,
                                variant: value_variant,
                                payload: value_payload,
                                ..
                            },
                        ..
                    } = value
                    else {
                        results.push((false, preserve_behavior));
                        continue;
                    };

                    let enum_type_name = match enum_type.as_ref() {
                        Expression {
                            kind: ExpressionKind::Identifier(id),
                            ..
                        } => id.name.clone(),
                        _ => "<unknown>".to_string(),
                    };

                    let expected_enum_type = resolve_enum_type_expression(
                        enum_type.as_ref(),
                        context,
                    )
                    .ok_or_else(|| {
                        diagnostic(
                            format!("Enum pattern references unknown type: {}", enum_type_name),
                            span,
                        )
                    })?;

                    if !types_equivalent(&expected_enum_type.kind, &value_enum.kind) {
                        results.push((false, preserve_behavior));
                        continue;
                    }

                    if value_variant.name != variant.name {
                        results.push((false, preserve_behavior));
                        continue;
                    }

                    if let Some(payload_pattern) = payload {
                        stack.push(Frame::Enter {
                            pattern: *payload_pattern,
                            value: value_payload,
                            passed_annotations,
                            preserve_behavior,
                            bound_type: None,
                        });
                    } else {
                        results.push((true, preserve_behavior));
                    }
                }
                BindingPattern::TypeHint(inner, type_expr, _) => {
                    if let Some(trait_expr) = trait_requirement_from_type_hint(type_expr.as_ref()) {
                        ensure_trait_requirements(value, &trait_expr, context, type_expr.span())?;
                    }
                    stack.push(Frame::Enter {
                        pattern: *inner,
                        value,
                        passed_annotations,
                        preserve_behavior,
                        bound_type: Some(*type_expr),
                    });
                }
                BindingPattern::Annotated {
                    pattern,
                    annotations,
                    ..
                } => {
                    let mut new_preserve_behavior = preserve_behavior;
                    if annotations
                        .iter()
                        .any(|ann| matches!(ann, BindingAnnotation::Export(_, _)))
                    {
                        new_preserve_behavior =
                            new_preserve_behavior.max(PreserveBehavior::PreserveBinding);
                    }
                    if annotations
                        .iter()
                        .any(|ann| matches!(ann, BindingAnnotation::Mutable(_)))
                    {
                        new_preserve_behavior =
                            new_preserve_behavior.max(PreserveBehavior::PreserveUsageInLoops);
                    }
                    let combined_annotations =
                        passed_annotations.into_iter().chain(annotations).collect();
                    stack.push(Frame::Enter {
                        pattern: *pattern,
                        value,
                        passed_annotations: combined_annotations,
                        preserve_behavior: new_preserve_behavior,
                        bound_type,
                    });
                }
            },
            Frame::StructContinue {
                items,
                value,
                index,
                preserve_behavior,
                overall_matched,
                passed_annotations,
            } => {
                let (matched, child_preserve) = results.pop().unwrap();
                let new_preserve = Ord::max(preserve_behavior, child_preserve);
                let new_matched = overall_matched && matched;
                let next_index = index + 1;
                if next_index >= items.len() {
                    results.push((new_matched, new_preserve));
                    continue;
                }

                let (field_identifier, field_pattern) = items[next_index].clone();
                let ExpressionKind::Struct(struct_items) = &value.kind else {
                    return Err(diagnostic(
                        "Struct pattern requires struct value",
                        field_pattern.span(),
                    ));
                };
                let field_span = field_pattern.span();
                let field_value = struct_items
                    .iter()
                    .find(|(value_identifier, _)| value_identifier.name == field_identifier.name)
                    .map(|(_, expr)| expr)
                    .ok_or_else(|| {
                        diagnostic(
                            format!("Missing field {}", field_identifier.name),
                            field_span,
                        )
                    })?;

                stack.push(Frame::StructContinue {
                    items,
                    value,
                    index: next_index,
                    preserve_behavior: new_preserve,
                    overall_matched: new_matched,
                    passed_annotations: passed_annotations.clone(),
                });
                stack.push(Frame::Enter {
                    pattern: field_pattern,
                    value: field_value,
                    passed_annotations,
                    preserve_behavior: new_preserve,
                    bound_type: None,
                });
            }
        }
    }

    results
        .pop()
        .ok_or_else(|| diagnostic("Failed to bind pattern from value", value.span()))
}

fn interpret_numeric_intrinsic<F>(
    left: Expression,
    right: Expression,
    span: SourceSpan,
    op: F,
) -> Result<Expression, Diagnostic>
where
    F: Fn(i32, i32) -> i32,
{
    let left_value = evaluate_numeric_operand(left)?;
    let right_value = evaluate_numeric_operand(right)?;
    Ok(Expression::new(
        ExpressionKind::Literal(ExpressionLiteral::Number(op(left_value, right_value))),
        span,
    ))
}

fn interpret_divide_intrinsic(
    left: Expression,
    right: Expression,
    span: SourceSpan,
) -> Result<Expression, Diagnostic> {
    let left_value = evaluate_numeric_operand(left)?;
    let right_value = evaluate_numeric_operand(right)?;

    if right_value == 0 {
        return Err(diagnostic("Division by zero", span));
    }

    Ok(Expression::new(
        ExpressionKind::Literal(ExpressionLiteral::Number(left_value / right_value)),
        span,
    ))
}

fn interpret_comparison_intrinsic<F>(
    left: Expression,
    right: Expression,
    span: SourceSpan,
    op: F,
) -> Result<Expression, Diagnostic>
where
    F: Fn(i32, i32) -> bool,
{
    let left_value = evaluate_numeric_operand(left)?;
    let right_value = evaluate_numeric_operand(right)?;
    Ok(
        ExpressionKind::Literal(ExpressionLiteral::Boolean(op(left_value, right_value)))
            .with_span(span),
    )
}

fn evaluate_numeric_operand(expr: Expression) -> Result<i32, Diagnostic> {
    match expr {
        Expression {
            kind: ExpressionKind::Literal(ExpressionLiteral::Number(value)),
            ..
        } => Ok(value),
        Expression {
            kind: ExpressionKind::Literal(ExpressionLiteral::Char(value)),
            ..
        } => Ok(i32::from(value)),
        _ => Err(diagnostic(
            "Expected numeric literal during intrinsic operation",
            expr.span(),
        )),
    }
}

fn interpret_boolean_intrinsic<F>(
    left: Expression,
    right: Expression,
    span: SourceSpan,
    op: F,
) -> Result<Expression, Diagnostic>
where
    F: Fn(bool, bool) -> bool,
{
    let left_value = evaluate_boolean_operand(left)?;
    let right_value = evaluate_boolean_operand(right)?;
    Ok(Expression::new(
        ExpressionKind::Literal(ExpressionLiteral::Boolean(op(left_value, right_value))),
        span,
    ))
}

fn evaluate_boolean_operand(expr: Expression) -> Result<bool, Diagnostic> {
    match expr {
        Expression {
            kind: ExpressionKind::Literal(ExpressionLiteral::Boolean(value)),
            ..
        } => Ok(value),
        _ => Err(diagnostic(
            "Expected boolean literal during intrinsic operation",
            expr.span(),
        )),
    }
}

fn is_resolved_constant(expr: &Expression) -> bool {
    let mut stack = vec![expr];
    while let Some(expr) = stack.pop() {
        match &expr.kind {
            ExpressionKind::Literal(_) | ExpressionKind::IntrinsicType(_) => {}
            ExpressionKind::EnumType(variants) => {
                for (_, ty) in variants.iter() {
                    stack.push(ty);
                }
            }
            ExpressionKind::EnumValue {
                enum_type, payload, ..
            } => {
                stack.push(payload);
                stack.push(enum_type);
            }
            ExpressionKind::EnumConstructor {
                enum_type,
                payload_type,
                ..
            } => {
                stack.push(payload_type);
                stack.push(enum_type);
            }
            ExpressionKind::Struct(items) => {
                for (_, value_expr) in items.iter() {
                    stack.push(value_expr);
                }
            }
            ExpressionKind::AttachImplementation {
                type_expr,
                implementation,
                ..
            } => {
                stack.push(implementation);
                stack.push(type_expr);
            }
            ExpressionKind::Function {
                parameter,
                return_type,
                body,
                ..
            } => {
                let new_function_context = {
                    let mut ctx = Context::empty();
                    ctx.bindings.push(HashMap::new());
                    bind_pattern_blanks(parameter.clone(), &mut ctx, Vec::new(), None).unwrap();
                    ctx
                };
                if !is_resolved_const_function_expression(body, &new_function_context) {
                    return false;
                }
                stack.push(return_type.as_ref().unwrap());
            }
            ExpressionKind::FunctionType {
                parameter,
                return_type,
                ..
            } => {
                stack.push(return_type);
                stack.push(parameter);
            }
            ExpressionKind::Assignment { .. } => return false,
            _ => return false,
        }
    }
    true
}

fn is_resolved_const_function_expression(expr: &Expression, function_context: &Context) -> bool {
    struct Frame<'a> {
        expr: &'a Expression,
        context: Context,
    }

    let mut stack = vec![Frame {
        expr,
        context: function_context.clone(),
    }];

    while let Some(frame) = stack.pop() {
        match &frame.expr.kind {
            ExpressionKind::Literal(_) | ExpressionKind::IntrinsicType(_) => {}
            ExpressionKind::Struct(items) => {
                for (_, value_expr) in items.iter() {
                    stack.push(Frame {
                        expr: value_expr,
                        context: frame.context.clone(),
                    });
                }
            }
            ExpressionKind::AttachImplementation {
                type_expr,
                implementation,
                ..
            } => {
                stack.push(Frame {
                    expr: implementation,
                    context: frame.context.clone(),
                });
                stack.push(Frame {
                    expr: type_expr,
                    context: frame.context.clone(),
                });
            }
            ExpressionKind::Function {
                parameter,
                return_type,
                body,
                ..
            } => {
                let mut new_function_context = frame.context.clone();
                bind_pattern_blanks(
                    parameter.clone(),
                    &mut new_function_context,
                    Vec::new(),
                    None,
                )
                .unwrap();
                stack.push(Frame {
                    expr: body,
                    context: new_function_context.clone(),
                });
                stack.push(Frame {
                    expr: return_type.as_ref().unwrap(),
                    context: new_function_context,
                });
            }
            ExpressionKind::FunctionType {
                parameter,
                return_type,
                ..
            } => {
                stack.push(Frame {
                    expr: return_type,
                    context: frame.context.clone(),
                });
                stack.push(Frame {
                    expr: parameter,
                    context: frame.context,
                });
            }
            ExpressionKind::Identifier(ident) => {
                if !frame.context.contains_identifier(ident) {
                    return false;
                }
            }
            ExpressionKind::IntrinsicOperation(intrinsic_operation) => match intrinsic_operation {
                IntrinsicOperation::Binary(left, right, _) => {
                    stack.push(Frame {
                        expr: right,
                        context: frame.context.clone(),
                    });
                    stack.push(Frame {
                        expr: left,
                        context: frame.context,
                    });
                }
                IntrinsicOperation::Unary(operand, _) => {
                    stack.push(Frame {
                        expr: operand,
                        context: frame.context,
                    });
                }
            },
            ExpressionKind::EnumType(cases) => {
                for (_, case_expr) in cases.iter() {
                    stack.push(Frame {
                        expr: case_expr,
                        context: frame.context.clone(),
                    });
                }
            }
            ExpressionKind::EnumValue {
                enum_type, payload, ..
            } => {
                stack.push(Frame {
                    expr: payload,
                    context: frame.context.clone(),
                });
                stack.push(Frame {
                    expr: enum_type,
                    context: frame.context,
                });
            }
            ExpressionKind::EnumConstructor {
                enum_type,
                payload_type,
                ..
            } => {
                stack.push(Frame {
                    expr: payload_type,
                    context: frame.context.clone(),
                });
                stack.push(Frame {
                    expr: enum_type,
                    context: frame.context,
                });
            }
            ExpressionKind::FunctionCall {
                function, argument, ..
            } => {
                stack.push(Frame {
                    expr: argument,
                    context: frame.context.clone(),
                });
                stack.push(Frame {
                    expr: function,
                    context: frame.context,
                });
            }
            ExpressionKind::ArrayIndex { array, index } => {
                stack.push(Frame {
                    expr: index,
                    context: frame.context.clone(),
                });
                stack.push(Frame {
                    expr: array,
                    context: frame.context,
                });
            }
            _ => return false,
        }
    }

    true
}

pub fn intrinsic_context() -> Context {
    intrinsic_context_with_files(HashMap::new())
}

pub fn intrinsic_context_with_files(files: HashMap<String, Expression>) -> Context {
    let mut context = Context {
        bindings: vec![HashMap::new()],
        in_loop: false,
        files,
        import_cache: HashMap::new(),
    };

    context.bindings.last_mut().unwrap().insert(
        Identifier::new("type"),
        (
            BindingContext::Bound(
                ExpressionKind::AttachImplementation {
                    type_expr: Box::new(intrinsic_type_expr(IntrinsicType::Type)),
                    implementation: Box::new(
                        ExpressionKind::Struct(vec![]).with_span(dummy_span()),
                    ),
                }
                .with_span(dummy_span()),
                PreserveBehavior::Inline,
                None,
            ),
            Vec::new(),
        ),
    );

    fn numeric_binary_intrinsic(
        symbol: &str,
        operator: BinaryIntrinsicOperator,
        intrinsic: IntrinsicType,
    ) -> (Identifier, Expression) {
        let typed_pattern = |name: &str| {
            BindingPattern::TypeHint(
                Box::new(BindingPattern::Identifier(
                    Identifier::new(name.to_string()),
                    dummy_span(),
                )),
                Box::new(intrinsic_type_expr(intrinsic.clone())),
                dummy_span(),
            )
        };

        (
            Identifier::new(symbol.to_string()),
            ExpressionKind::Function {
                parameter: typed_pattern("self"),
                return_type: Some(Box::new(
                    ExpressionKind::FunctionType {
                        parameter: Box::new(intrinsic_type_expr(intrinsic.clone())),
                        return_type: Box::new(intrinsic_type_expr(intrinsic.clone())),
                    }
                    .with_span(dummy_span()),
                )),
                body: Box::new(
                    ExpressionKind::Function {
                        parameter: typed_pattern("other"),
                        return_type: Some(Box::new(intrinsic_type_expr(intrinsic.clone()))),
                        body: Box::new(
                            ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(
                                Box::new(identifier_expr("self")),
                                Box::new(identifier_expr("other")),
                                operator,
                            ))
                            .with_span(dummy_span()),
                        ),
                    }
                    .with_span(dummy_span()),
                ),
            }
            .with_span(dummy_span()),
        )
    }

    context.bindings.last_mut().unwrap().insert(
        Identifier::new("i32"),
        (
            BindingContext::Bound(
                ExpressionKind::AttachImplementation {
                    type_expr: Box::new(intrinsic_type_expr(IntrinsicType::I32)),
                    implementation: Box::new(
                        ExpressionKind::Struct(vec![
                            numeric_binary_intrinsic(
                                "+",
                                BinaryIntrinsicOperator::I32Add,
                                IntrinsicType::I32,
                            ),
                            numeric_binary_intrinsic(
                                "-",
                                BinaryIntrinsicOperator::I32Subtract,
                                IntrinsicType::I32,
                            ),
                            numeric_binary_intrinsic(
                                "*",
                                BinaryIntrinsicOperator::I32Multiply,
                                IntrinsicType::I32,
                            ),
                            numeric_binary_intrinsic(
                                "/",
                                BinaryIntrinsicOperator::I32Divide,
                                IntrinsicType::I32,
                            ),
                            numeric_binary_intrinsic(
                                "==",
                                BinaryIntrinsicOperator::I32Equal,
                                IntrinsicType::I32,
                            ),
                            numeric_binary_intrinsic(
                                "!=",
                                BinaryIntrinsicOperator::I32NotEqual,
                                IntrinsicType::I32,
                            ),
                            numeric_binary_intrinsic(
                                "<",
                                BinaryIntrinsicOperator::I32LessThan,
                                IntrinsicType::I32,
                            ),
                            numeric_binary_intrinsic(
                                ">",
                                BinaryIntrinsicOperator::I32GreaterThan,
                                IntrinsicType::I32,
                            ),
                            numeric_binary_intrinsic(
                                "<=",
                                BinaryIntrinsicOperator::I32LessThanOrEqual,
                                IntrinsicType::I32,
                            ),
                            numeric_binary_intrinsic(
                                ">=",
                                BinaryIntrinsicOperator::I32GreaterThanOrEqual,
                                IntrinsicType::I32,
                            ),
                        ])
                        .with_span(dummy_span()),
                    ),
                }
                .with_span(dummy_span()),
                PreserveBehavior::Inline,
                None,
            ),
            Vec::new(),
        ),
    );

    context.bindings.last_mut().unwrap().insert(
        Identifier::new("u8"),
        (
            BindingContext::Bound(
                ExpressionKind::AttachImplementation {
                    type_expr: Box::new(intrinsic_type_expr(IntrinsicType::U8)),
                    implementation: Box::new(
                        ExpressionKind::Struct(vec![
                            numeric_binary_intrinsic(
                                "+",
                                BinaryIntrinsicOperator::I32Add,
                                IntrinsicType::U8,
                            ),
                            numeric_binary_intrinsic(
                                "-",
                                BinaryIntrinsicOperator::I32Subtract,
                                IntrinsicType::U8,
                            ),
                            numeric_binary_intrinsic(
                                "*",
                                BinaryIntrinsicOperator::I32Multiply,
                                IntrinsicType::U8,
                            ),
                            numeric_binary_intrinsic(
                                "/",
                                BinaryIntrinsicOperator::I32Divide,
                                IntrinsicType::U8,
                            ),
                            numeric_binary_intrinsic(
                                "==",
                                BinaryIntrinsicOperator::I32Equal,
                                IntrinsicType::U8,
                            ),
                            numeric_binary_intrinsic(
                                "!=",
                                BinaryIntrinsicOperator::I32NotEqual,
                                IntrinsicType::U8,
                            ),
                            numeric_binary_intrinsic(
                                "<",
                                BinaryIntrinsicOperator::I32LessThan,
                                IntrinsicType::U8,
                            ),
                            numeric_binary_intrinsic(
                                ">",
                                BinaryIntrinsicOperator::I32GreaterThan,
                                IntrinsicType::U8,
                            ),
                            numeric_binary_intrinsic(
                                "<=",
                                BinaryIntrinsicOperator::I32LessThanOrEqual,
                                IntrinsicType::U8,
                            ),
                            numeric_binary_intrinsic(
                                ">=",
                                BinaryIntrinsicOperator::I32GreaterThanOrEqual,
                                IntrinsicType::U8,
                            ),
                        ])
                        .with_span(dummy_span()),
                    ),
                }
                .with_span(dummy_span()),
                PreserveBehavior::Inline,
                None,
            ),
            Vec::new(),
        ),
    );

    fn bool_binary_intrinsic(
        symbol: &str,
        operator: BinaryIntrinsicOperator,
    ) -> (Identifier, Expression) {
        let typed_pattern = |name: &str| {
            BindingPattern::TypeHint(
                Box::new(BindingPattern::Identifier(
                    Identifier::new(name.to_string()),
                    dummy_span(),
                )),
                Box::new(intrinsic_type_expr(IntrinsicType::Boolean)),
                dummy_span(),
            )
        };

        (
            Identifier::new(symbol.to_string()),
            ExpressionKind::Function {
                parameter: typed_pattern("self"),
                return_type: Some(Box::new(Expression::new(
                    ExpressionKind::FunctionType {
                        parameter: Box::new(intrinsic_type_expr(IntrinsicType::Boolean)),
                        return_type: Box::new(intrinsic_type_expr(IntrinsicType::Boolean)),
                    },
                    dummy_span(),
                ))),
                body: Box::new(Expression::new(
                    ExpressionKind::Function {
                        parameter: typed_pattern("other"),
                        return_type: Some(Box::new(intrinsic_type_expr(IntrinsicType::Boolean))),
                        body: Box::new(Expression::new(
                            ExpressionKind::IntrinsicOperation(IntrinsicOperation::Binary(
                                Box::new(identifier_expr("self")),
                                Box::new(identifier_expr("other")),
                                operator,
                            )),
                            dummy_span(),
                        )),
                    },
                    dummy_span(),
                )),
            }
            .with_span(dummy_span()),
        )
    }

    context.bindings.last_mut().unwrap().insert(
        Identifier::new("bool"),
        (
            BindingContext::Bound(
                ExpressionKind::AttachImplementation {
                    type_expr: Box::new(intrinsic_type_expr(IntrinsicType::Boolean)),
                    implementation: Box::new(
                        ExpressionKind::Struct(vec![
                            bool_binary_intrinsic("==", BinaryIntrinsicOperator::I32Equal),
                            bool_binary_intrinsic("!=", BinaryIntrinsicOperator::I32NotEqual),
                            bool_binary_intrinsic("&&", BinaryIntrinsicOperator::BooleanAnd),
                            bool_binary_intrinsic("||", BinaryIntrinsicOperator::BooleanOr),
                            bool_binary_intrinsic("^", BinaryIntrinsicOperator::BooleanXor),
                        ])
                        .with_span(dummy_span()),
                    ),
                }
                .with_span(dummy_span()),
                PreserveBehavior::Inline,
                None,
            ),
            Vec::new(),
        ),
    );

    context.bindings.last_mut().unwrap().insert(
        Identifier::new("true"),
        (
            BindingContext::Bound(
                ExpressionKind::Literal(ExpressionLiteral::Boolean(true)).with_span(dummy_span()),
                PreserveBehavior::Inline,
                None,
            ),
            Vec::new(),
        ),
    );

    context.bindings.last_mut().unwrap().insert(
        Identifier::new("false"),
        (
            BindingContext::Bound(
                ExpressionKind::Literal(ExpressionLiteral::Boolean(false)).with_span(dummy_span()),
                PreserveBehavior::Inline,
                None,
            ),
            Vec::new(),
        ),
    );

    context.bindings.last_mut().unwrap().insert(
        Identifier::new("js"),
        (
            BindingContext::Bound(
                ExpressionKind::Literal(ExpressionLiteral::Target(TargetLiteral::JSTarget))
                    .with_span(dummy_span()),
                PreserveBehavior::Inline,
                None,
            ),
            Vec::new(),
        ),
    );

    context.bindings.last_mut().unwrap().insert(
        Identifier::new("wasm"),
        (
            BindingContext::Bound(
                ExpressionKind::Literal(ExpressionLiteral::Target(TargetLiteral::WasmTarget))
                    .with_span(dummy_span()),
                PreserveBehavior::Inline,
                None,
            ),
            Vec::new(),
        ),
    );

    context.bindings.last_mut().unwrap().insert(
        Identifier::new("enum"),
        (
            BindingContext::Bound(
                ExpressionKind::Function {
                    parameter: BindingPattern::TypeHint(
                        Box::new(BindingPattern::Identifier(
                            Identifier::new("struct_arg"),
                            dummy_span(),
                        )),
                        Box::new(intrinsic_type_expr(IntrinsicType::Type)),
                        dummy_span(),
                    ),
                    return_type: Some(Box::new(intrinsic_type_expr(IntrinsicType::Type))),
                    body: Box::new(Expression::new(
                        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(
                            Box::new(Expression::new(
                                ExpressionKind::Identifier(Identifier::new("struct_arg")),
                                dummy_span(),
                            )),
                            UnaryIntrinsicOperator::EnumFromStruct,
                        )),
                        dummy_span(),
                    )),
                }
                .with_span(dummy_span()),
                PreserveBehavior::Inline,
                None,
            ),
            Vec::new(),
        ),
    );

    context.bindings.last_mut().unwrap().insert(
        Identifier::new("use"),
        (
            BindingContext::Bound(
                ExpressionKind::Function {
                    parameter: BindingPattern::Identifier(Identifier::new("path"), dummy_span()),
                    return_type: None,
                    body: Box::new(Expression::new(
                        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(
                            Box::new(Expression::new(
                                ExpressionKind::Identifier(Identifier::new("path")),
                                dummy_span(),
                            )),
                            UnaryIntrinsicOperator::UseFromString,
                        )),
                        dummy_span(),
                    )),
                }
                .with_span(dummy_span()),
                PreserveBehavior::Inline,
                None,
            ),
            Vec::new(),
        ),
    );

    context.bindings.last_mut().unwrap().insert(
        Identifier::new("match"),
        (
            BindingContext::Bound(
                ExpressionKind::Function {
                    parameter: BindingPattern::Identifier(
                        Identifier::new("branches"),
                        dummy_span(),
                    ),
                    return_type: Some(Box::new(
                        ExpressionKind::FunctionType {
                            parameter: Box::new(intrinsic_type_expr(IntrinsicType::Type)),
                            return_type: Box::new(intrinsic_type_expr(IntrinsicType::Type)),
                        }
                        .with_span(dummy_span()),
                    )),
                    body: Box::new(Expression::new(
                        ExpressionKind::IntrinsicOperation(IntrinsicOperation::Unary(
                            Box::new(Expression::new(
                                ExpressionKind::Identifier(Identifier::new("branches")),
                                dummy_span(),
                            )),
                            UnaryIntrinsicOperator::MatchFromStruct,
                        )),
                        dummy_span(),
                    )),
                }
                .with_span(dummy_span()),
                PreserveBehavior::Inline,
                None,
            ),
            Vec::new(),
        ),
    );
    context
}

#[cfg(test)]
pub fn evaluate_text_to_raw_expression(program: &str) -> Result<(Expression, Context), Diagnostic> {
    let (expression, remaining) =
        crate::parsing::parse_block(program).expect("Failed to parse program text");
    assert!(
        remaining.trim().is_empty(),
        "Parser did not consume entire input, remaining: {remaining:?}"
    );

    let expression = uniquify::uniquify_program(expression);
    let mut context = intrinsic_context();
    interpret_program(expression, &mut context)
}

pub fn evaluate_text_to_expression(program: &str) -> Result<(Expression, Context), Diagnostic> {
    let (expression, remaining) = crate::parsing::parse_block(program)?;
    assert!(
        remaining.trim().is_empty(),
        "Parser did not consume entire input, remaining: {remaining:?}"
    );

    let expression = uniquify::uniquify_program(expression);
    let mut context = intrinsic_context();
    let (mut value, context) = interpret_program(expression, &mut context)?;

    println!("{}", value.pretty_print());

    while let ExpressionKind::Block(exprs) = value.kind {
        value = exprs.last().cloned().unwrap();
    }

    Ok((value, context))
}

pub fn evaluate_files_to_expression(
    files: Vec<(&str, &str)>,
    root: &str,
) -> Result<(Expression, Context), Diagnostic> {
    let file_map = loader::build_parsed_files(files)?;
    let root = loader::normalize_path(root);
    let expression = file_map
        .get(&root)
        .ok_or_else(|| Diagnostic::new(format!("Missing root source for {root}")))?
        .clone();
    let mut context = intrinsic_context_with_files(file_map);
    interpret_program(expression, &mut context)
}

#[cfg(test)]
fn evaluate_text_to_number(program: &str) -> i32 {
    match evaluate_text_to_expression(program)
        .unwrap_or_else(|e| {
            panic!(
                "Failed to interpret parsed expression: {:?}",
                e.render_with_source(program)
            )
        })
        .0
    {
        Expression {
            kind: ExpressionKind::Literal(ExpressionLiteral::Number(value)),
            ..
        } => value,
        _ => panic!("Expected numeric result"),
    }
}

#[test]
fn test_basic_binding_interpretation() {
    let mut context = intrinsic_context();

    let expr = ExpressionKind::Block(vec![
        ExpressionKind::Binding(Box::new(Binding {
            pattern: BindingPattern::TypeHint(
                Box::new(BindingPattern::Identifier(
                    Identifier::new("x"),
                    dummy_span(),
                )),
                Box::new(intrinsic_type_expr(IntrinsicType::I32)),
                dummy_span(),
            ),
            expr: literal_number_expr(5),
        }))
        .with_span(dummy_span()),
        identifier_expr("x"),
    ])
    .with_span(dummy_span());

    let result = interpret_expression(expr, &mut context).unwrap();

    if let ExpressionKind::Literal(ExpressionLiteral::Number(value)) = result.kind {
        assert_eq!(value, 5);
    } else {
        panic!("Expected a number literal as result");
    }
}

#[test]
fn interpret_text_basic_operations() {
    let cases = [
        ("5 + 10", 15),
        ("12 - 7", 5),
        ("3 * 4", 12),
        ("20 / 5", 4),
        ("2 + 3 * 4", 14),
    ];

    for (program, expected) in cases {
        assert_eq!(
            evaluate_text_to_number(program),
            expected,
            "program `{program}` produced unexpected value"
        );
    }
}

#[test]
fn test_basic_addition_interpretation() {
    let mut context = intrinsic_context();

    let expr = ExpressionKind::Operation {
        operator: "+".to_string(),
        left: Box::new(literal_number_expr(5)),
        right: Box::new(literal_number_expr(10)),
    }
    .with_span(dummy_span());

    let result = interpret_expression(expr, &mut context).unwrap();

    if let ExpressionKind::Literal(ExpressionLiteral::Number(value)) = result.kind {
        assert_eq!(value, 15);
    } else {
        panic!("Expected a number literal as result");
    }
}

#[test]
fn i32_is_constant() {
    let binding = intrinsic_context();
    let Some((BindingContext::Bound(expr, _, _), _)) =
        binding.get_identifier(&Identifier::new("i32"))
    else {
        panic!("i32 binding not found");
    };
    assert!(is_resolved_constant(expr));
}

#[test]
fn interpret_text_user_defined_function2() {
    let program = "
(bar: i32) => (
    bar + 1
)
    ";
    let (expr, _) = evaluate_text_to_expression(program).unwrap();
    assert!(matches!(expr.kind, ExpressionKind::Function { .. }));
}
#[test]
fn interpret_let_binding_function() {
    let program = "
Level2 := enum { Some = i32, None = {} };
Level1 := enum { Some = Level2, None = {} };

(export wasm) check := (x: i32) => (
    foo := if x > 0 then Level1::Some(Level2::Some(x)) else Level1::None;

    if Level1::Some(Level2::Some(b)) := foo then b else 0
);
{}
    ";
    let (expr, _) = evaluate_text_to_raw_expression(program).unwrap();
    println!("{expr:?}");
}

#[test]
fn interpret_text_user_defined_function() {
    let program = "
foo := (bar: i32) => (
    bar + 1
);
foo(123)
    ";
    assert_eq!(evaluate_text_to_number(program), 124);
}

#[test]
fn interpret_text_user_defined_tuple_arguments() {
    let program = "
foo2 := {bar1: i32, bar2: i32} => (
    bar1 + bar2
);
foo2{100, 24}
    ";
    assert_eq!(evaluate_text_to_number(program), 124);
}

#[test]
fn interpret_text_struct_property_access() {
    let program = "
point := { x = 5, y = 10 };
point.x
    ";
    assert_eq!(evaluate_text_to_number(program), 5);
}

#[test]
fn interpret_text_struct_property_call() {
    let program = "
container := {
    inc = (value: i32) => (
        value + 1
    )
};
container.inc(41)
    ";
    assert_eq!(evaluate_text_to_number(program), 42);
}

#[test]
fn interpret_binding_with_export_annotation() {
    let program = "
(export js) answer: i32 := 42;
answer
    ";
    assert_eq!(evaluate_text_to_number(program), 42);
}

#[test]
fn interpret_reports_unbound_identifier_span() {
    let source = "unknown";
    let (expr, remaining) = crate::parsing::parse_block(source).expect("parse should succeed");
    assert!(remaining.trim().is_empty());
    let mut context = intrinsic_context();
    let err = interpret_expression(expr, &mut context).expect_err("expected unbound identifier");
    let rendered = err.render_with_source(source);
    assert!(rendered.contains("Unbound identifier: unknown"));
    assert!(rendered.contains("line 1, column 1"));
}

#[test]
fn interpret_reports_calling_non_function_span() {
    let source = "5(1)";
    let (expr, remaining) = crate::parsing::parse_block(source).expect("parse should succeed");
    assert!(remaining.trim().is_empty());
    let mut context = intrinsic_context();
    let err = interpret_expression(expr, &mut context).expect_err("expected non-function call");
    let rendered = err.render_with_source(source);
    assert!(rendered.contains("Attempted to call a non-function value"));
    assert!(rendered.contains("line 1, column 1"));
}

#[test]
fn interpret_basic_enum_flow() {
    let program = "
    IntOption := enum { Some = i32, None = {} };
    (export wasm) pick_positive := (x: i32) => (
        opt := if x > 0 then IntOption::Some(x) else IntOption::None;

        if IntOption::Some(value) := opt then value else 0
    );
    pick_positive(3)
    ";

    let (ast, remaining) = crate::parsing::parse_block(program).unwrap();
    println!("{}", ast.pretty_print());
    assert!(remaining.trim().is_empty());
    let mut context = intrinsic_context();
    if let ExpressionKind::Block(exprs) = ast.kind {
        let mut last = None;
        for expr in exprs {
            last = Some(interpret_expression(expr, &mut context).unwrap());
        }
        match last.unwrap().kind {
            ExpressionKind::Literal(ExpressionLiteral::Number(v)) => assert_eq!(v, 3),
            other => panic!("unexpected result {other:?}"),
        }
    } else {
        panic!("expected block");
    }
}

#[test]
fn enum_intrinsic_exposes_function_type() {
    let mut context = intrinsic_context();
    let enum_binding = context
        .get_identifier(&Identifier::new("enum"))
        .expect("enum intrinsic should be present")
        .clone();

    match &enum_binding.0 {
        BindingContext::Bound(expr, _, _) => {
            match get_type_of_expression(expr, &mut context).unwrap().kind {
                ExpressionKind::FunctionType {
                    parameter,
                    return_type,
                } => {
                    assert!(parameter == return_type)
                }
                other => panic!("unexpected enum intrinsic type {other:?}"),
            }
        }
        other => panic!("unexpected enum intrinsic binding {other:?}"),
    }
}

#[test]
fn enum_intrinsic_can_be_aliased() {
    let program = "
    Alias := enum;
    IntOption := Alias { Some = i32, None = {} };
    IntOption::None
    ";

    let (value, _context) = evaluate_text_to_expression(program).unwrap();
    match value.kind {
        ExpressionKind::EnumValue { variant, .. } => assert_eq!(variant.name, "None"),
        other => panic!("unexpected alias result {other:?}"),
    }
}

#[test]
fn enum_pattern_rejects_unknown_type() {
    let program = "
    Opt := enum { Some = i32, None = {} };
    value := Opt::Some(1);
    if Missing::Some(v) := value then v else 0
    ";

    let error = match evaluate_text_to_expression(program) {
        Ok(_) => panic!("Expected error for unknown enum"),
        Err(err) => err,
    };
    if !error.message.contains("Unbound identifier: Missing") {
        panic!("unexpected error message: {}", error.message);
    }
}

#[test]
fn enum_pattern_requires_matching_type() {
    let program = "
    First := enum { Some = i32, None = {} };
    Second := enum { Some = {}, None = {} };
    check := {} => (
        value := First::Some(5);
        if Second::Some := value then 1 else 0
    );
    check{}
    ";

    let (value, _context) = evaluate_text_to_expression(program)
        .unwrap_or_else(|e| panic!("{}", e.render_with_source(program)));
    match value.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(result)) => assert_eq!(result, 0),
        other => panic!("unexpected result {other:?}"),
    }
}

#[test]
fn enum_rejects_value_payloads() {
    let program = "
    Bad := enum { Value = 1 };
    {};
    ";

    let (ast, remaining) = crate::parsing::parse_block(program).unwrap();
    assert!(remaining.trim().is_empty());
    let mut context = intrinsic_context();
    let result = interpret_program(ast, &mut context);
    assert!(result.is_err(), "expected enum construction to fail");
    let error = result.err().unwrap();
    assert!(
        error
            .message
            .contains("Enum variant payload must be a type"),
        "unexpected error: {}",
        error.message
    );
}
