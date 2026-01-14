use std::collections::HashSet;

use crate::{
    diagnostics::Diagnostic,
    intermediate::{
        IntermediateExportType, IntermediateKind, IntermediateLValue, IntermediateResult,
    },
    parsing::{BinaryIntrinsicOperator, BindingPattern, ExpressionLiteral, TargetLiteral},
};

pub fn compile_exports(intermediate: &IntermediateResult) -> Result<String, Diagnostic> {
    let mut output = String::new();

    // First, generate all functions as internal functions
    for (idx, function) in intermediate.functions.iter().enumerate() {
        let (params, destructuring) = flatten_parameters(&function.parameter, "arg")?;

        let mut bound_ids = HashSet::new();
        collect_bound_identifiers(&function.body, &mut bound_ids);

        output.push_str(&format!(
            "function __silk_fn_{}({}) {{\n",
            idx,
            params.join(", ")
        ));

        if !bound_ids.is_empty() {
            let ids: Vec<String> = bound_ids.into_iter().collect();
            output.push_str(&format!("  let {};\n", ids.join(", ")));
        }

        if !destructuring.is_empty() {
            output.push_str(&destructuring);
        }
        let body = compile_expression(&function.body, intermediate)?;
        output.push_str(&format!("  return {};\n", body));
        output.push_str("}\n");
    }

    // Then, generate exports
    for export in &intermediate.exports {
        if export.target != TargetLiteral::JSTarget {
            continue;
        }

        match export.export_type {
            IntermediateExportType::Global => {
                let global = &intermediate.globals[export.index];
                let value = compile_expression(&global.value, intermediate)?;
                output.push_str(&format!("export const {} = {};\n", global.name, value));
            }
            IntermediateExportType::Function => {
                // Re-export internal function
                output.push_str(&format!(
                    "export const {} = __silk_fn_{};\n",
                    export.name, export.index
                ));
            }
        }
    }

    Ok(output)
}

fn flatten_parameters(
    pattern: &BindingPattern,
    source: &str,
) -> Result<(Vec<String>, String), Diagnostic> {
    match pattern {
        BindingPattern::Struct(fields, _) => {
            let mut params = Vec::new();
            let mut destructuring_code = String::new();

            for (field_prop, field_pat) in fields {
                let param_name =
                    extract_simple_name(field_pat).unwrap_or_else(|| field_prop.name.clone());
                params.push(param_name.clone());

                if !is_simple_identifier(field_pat, &param_name) {
                    let extra = compile_pattern_destructuring(field_pat, &param_name)?;
                    destructuring_code.push_str(&extra);
                }
            }
            Ok((params, destructuring_code))
        }
        BindingPattern::Identifier(id, _) => Ok((vec![id.name.clone()], String::new())),
        BindingPattern::TypeHint(inner, _, _) => flatten_parameters(inner, source),
        BindingPattern::Annotated { pattern, .. } => flatten_parameters(pattern, source),
        _ => {
            // Fallback for single unknown arg type
            Ok((
                vec!["arg".to_string()],
                compile_pattern_destructuring(pattern, "arg")?,
            ))
        }
    }
}

fn extract_simple_name(pattern: &BindingPattern) -> Option<String> {
    match pattern {
        BindingPattern::Identifier(id, _) => Some(id.name.clone()),
        BindingPattern::TypeHint(inner, _, _) => extract_simple_name(inner),
        BindingPattern::Annotated { pattern, .. } => extract_simple_name(pattern),
        _ => None,
    }
}

fn is_simple_identifier(pattern: &BindingPattern, name: &str) -> bool {
    match pattern {
        BindingPattern::Identifier(id, _) => id.name == name,
        BindingPattern::TypeHint(inner, _, _) => is_simple_identifier(inner, name),
        BindingPattern::Annotated { pattern, .. } => is_simple_identifier(pattern, name),
        _ => false,
    }
}

fn compile_pattern_destructuring(
    pattern: &BindingPattern,
    source: &str,
) -> Result<String, Diagnostic> {
    match pattern {
        BindingPattern::Identifier(id, _) => Ok(format!("  {} = {};\n", id.name, source)),
        BindingPattern::Struct(fields, _) => {
            let mut output = String::new();
            for (field_name, field_pattern) in fields {
                let field_access = format!("{}.{}", source, field_name.name);
                output.push_str(&compile_pattern_destructuring(
                    field_pattern,
                    &field_access,
                )?);
            }
            Ok(output)
        }
        BindingPattern::TypeHint(pat, _, _) => compile_pattern_destructuring(pat, source),
        BindingPattern::Annotated { pattern, .. } => compile_pattern_destructuring(pattern, source),
        _ => Ok(String::new()),
    }
}

fn compile_expression(
    expr: &IntermediateKind,
    intermediate: &IntermediateResult,
) -> Result<String, Diagnostic> {
    match expr {
        IntermediateKind::Literal(lit) => match lit {
            ExpressionLiteral::Number(n) => Ok(n.to_string()),
            ExpressionLiteral::Boolean(b) => Ok(b.to_string()),
            ExpressionLiteral::String(bytes) => {
                let s = String::from_utf8_lossy(bytes);
                Ok(format!("\"{}\"", s.escape_debug()))
            }
            _ => Ok("null".to_string()),
        },
        IntermediateKind::Identifier(id) => Ok(id.name.clone()),
        IntermediateKind::IntrinsicOperation(op) => match op {
            crate::intermediate::IntermediateIntrinsicOperation::Binary(left, right, op) => {
                let l = compile_expression(left, intermediate)?;
                let r = compile_expression(right, intermediate)?;
                let op_str = match op {
                    BinaryIntrinsicOperator::I32Add => "+",
                    BinaryIntrinsicOperator::I32Subtract => "-",
                    BinaryIntrinsicOperator::I32Multiply => "*",
                    BinaryIntrinsicOperator::I32Divide => "/",
                    BinaryIntrinsicOperator::I32LessThan => "<",
                    BinaryIntrinsicOperator::I32GreaterThan => ">",
                    BinaryIntrinsicOperator::I32LessThanOrEqual => "<=",
                    BinaryIntrinsicOperator::I32GreaterThanOrEqual => ">=",
                    BinaryIntrinsicOperator::I32Equal => "===",
                    BinaryIntrinsicOperator::I32NotEqual => "!==",
                    BinaryIntrinsicOperator::BooleanAnd => "&&",
                    BinaryIntrinsicOperator::BooleanOr => "||",
                    BinaryIntrinsicOperator::BooleanXor => "^",
                };
                Ok(format!("({} {} {})", l, op_str, r))
            }
            _ => Err(Diagnostic::new("Unsupported intrinsic operation")),
        },
        IntermediateKind::Binding(binding) => {
            let val = compile_expression(&binding.expr, intermediate)?;
            Ok(format!("(({} = {}), true)", binding.identifier.name, val))
        }
        IntermediateKind::Block(exprs) => {
            if exprs.is_empty() {
                return Ok("undefined".to_string());
            }

            let mut stmts = Vec::new();
            for (i, e) in exprs.iter().enumerate() {
                let s = compile_expression(e, intermediate)?;
                if i == exprs.len() - 1 {
                    stmts.push(format!("return {};", s));
                } else {
                    if s.ends_with(";") {
                        stmts.push(s);
                    } else {
                        stmts.push(format!("{};", s));
                    }
                }
            }

            Ok(format!("(() => {{ {} }})()", stmts.join("\n")))
        }
        IntermediateKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let cond = compile_expression(condition, intermediate)?;
            let t = compile_expression(then_branch, intermediate)?;
            let e = compile_expression(else_branch, intermediate)?;
            Ok(format!("({} ? {} : {})", cond, t, e))
        }
        IntermediateKind::FunctionCall { function, argument } => {
            // Use internal function name directly
            let arg = compile_expression(argument, intermediate)?;
            Ok(format!("__silk_fn_{}({})", function, arg))
        }
        IntermediateKind::TypePropertyAccess { object, property } => {
            let obj = compile_expression(object, intermediate)?;
            Ok(format!("{}.{}", obj, property))
        }
        IntermediateKind::ArrayIndex { array, index } => {
            let arr = compile_expression(array, intermediate)?;
            let idx = compile_expression(index, intermediate)?;
            Ok(format!("{}[{}]", arr, idx))
        }
        IntermediateKind::Struct(fields) => {
            let mut parts = Vec::new();
            for (id, val) in fields {
                let v = compile_expression(val, intermediate)?;
                parts.push(format!("{}: {}", id.name, v));
            }
            Ok(format!("{{ {} }}", parts.join(", ")))
        }
        IntermediateKind::Assignment { target, expr } => {
            let lval = compile_lvalue(target, intermediate)?;
            let val = compile_expression(expr, intermediate)?;
            Ok(format!("{} = {}", lval, val))
        }
        IntermediateKind::ArrayLiteral {
            items, field_names, ..
        } => {
            // Structs may be lowered as ArrayLiteral with field_names
            if !field_names.is_empty() && field_names.len() == items.len() {
                let mut parts = Vec::new();
                for (name, item) in field_names.iter().zip(items.iter()) {
                    let v = compile_expression(item, intermediate)?;
                    parts.push(format!("{}: {}", name, v));
                }
                Ok(format!("{{ {} }}", parts.join(", ")))
            } else {
                // Regular array
                let mut parts = Vec::new();
                for item in items {
                    parts.push(compile_expression(item, intermediate)?);
                }
                Ok(format!("[{}]", parts.join(", ")))
            }
        }
        _ => Err(Diagnostic::new(
            "Unsupported expression type for JS backend",
        )),
    }
}

fn compile_lvalue(
    lvalue: &IntermediateLValue,
    intermediate: &IntermediateResult,
) -> Result<String, Diagnostic> {
    match lvalue {
        IntermediateLValue::Identifier(id, _) => Ok(id.name.clone()),
        IntermediateLValue::TypePropertyAccess {
            object, property, ..
        } => {
            let obj = compile_lvalue(object, intermediate)?;
            Ok(format!("{}.{}", obj, property))
        }
        IntermediateLValue::ArrayIndex { array, index, .. } => {
            let arr = compile_lvalue(array, intermediate)?;
            let idx = compile_expression(index, intermediate)?;
            Ok(format!("{}[{}]", arr, idx))
        }
    }
}

fn collect_bound_identifiers(expr: &IntermediateKind, ids: &mut HashSet<String>) {
    match expr {
        IntermediateKind::Binding(binding) => {
            ids.insert(binding.identifier.name.clone());
            collect_bound_identifiers(&binding.expr, ids);
        }
        IntermediateKind::Block(exprs) => {
            for e in exprs {
                collect_bound_identifiers(e, ids);
            }
        }
        IntermediateKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_bound_identifiers(condition, ids);
            collect_bound_identifiers(then_branch, ids);
            collect_bound_identifiers(else_branch, ids);
        }
        IntermediateKind::IntrinsicOperation(op) => match op {
            crate::intermediate::IntermediateIntrinsicOperation::Binary(l, r, _) => {
                collect_bound_identifiers(l, ids);
                collect_bound_identifiers(r, ids);
            }
            crate::intermediate::IntermediateIntrinsicOperation::Unary(e, _) => {
                collect_bound_identifiers(e, ids);
            }
        },
        IntermediateKind::FunctionCall { argument, .. } => {
            collect_bound_identifiers(argument, ids);
        }
        IntermediateKind::Struct(fields) => {
            for (_, val) in fields {
                collect_bound_identifiers(val, ids);
            }
        }
        IntermediateKind::Assignment { expr, .. } => {
            collect_bound_identifiers(expr, ids);
        }
        IntermediateKind::ArrayLiteral { items, .. } => {
            for item in items {
                collect_bound_identifiers(item, ids);
            }
        }
        IntermediateKind::ArrayIndex { array, index } => {
            collect_bound_identifiers(array, ids);
            collect_bound_identifiers(index, ids);
        }
        IntermediateKind::TypePropertyAccess { object, .. } => {
            collect_bound_identifiers(object, ids);
        }
        IntermediateKind::Diverge { value, .. } => {
            collect_bound_identifiers(value, ids);
        }
        IntermediateKind::Loop { body } => {
            collect_bound_identifiers(body, ids);
        }
        _ => {}
    }
}
