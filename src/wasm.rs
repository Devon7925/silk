use wasm_encoder::{
    CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction, Module,
    TypeSection, ValType,
};

use crate::{
    diagnostics::{Diagnostic, SourceSpan},
    interpret::{self, AnnotatedBinding},
    parsing::{
        BinaryIntrinsicOperator, BindingAnnotation, BindingPattern, Expression, ExpressionLiteral,
        IntrinsicOperation, IntrinsicType, TargetLiteral,
    },
};

struct WasmFunctionParam {
    name: String,
    ty: ValType,
}

struct WasmFunctionExport {
    name: String,
    params: Vec<WasmFunctionParam>,
    body: Expression,
}

pub fn compile_exports(context: &interpret::Context) -> Result<Vec<u8>, Diagnostic> {
    let exports = collect_wasm_exports(context)?;
    if exports.is_empty() {
        return Ok(Vec::new());
    }

    let mut module = Module::new();
    let mut type_section = TypeSection::new();
    let mut function_section = FunctionSection::new();
    let mut export_section = ExportSection::new();
    let mut code_section = CodeSection::new();

    let mut next_type_index = 0u32;
    let mut next_function_index = 0u32;

    for export in exports {
        let param_types: Vec<ValType> = export.params.iter().map(|param| param.ty).collect();
        type_section.function(param_types, vec![ValType::I32]);
        function_section.function(next_type_index);
        next_type_index += 1;

        let mut locals = Vec::new();
        let mut local_indices = std::collections::HashMap::new();

        // Add parameters to local_indices
        for (i, param) in export.params.iter().enumerate() {
            local_indices.insert(param.name.clone(), i as u32);
        }

        // Collect additional locals from body
        let body_locals = collect_locals(&export.body)?;
        for (name, ty) in body_locals {
            if !local_indices.contains_key(&name) {
                local_indices.insert(name, (export.params.len() + locals.len()) as u32);
                locals.push(ty);
            }
        }

        let mut func = Function::new(if locals.is_empty() {
            Vec::new()
        } else {
            vec![(locals.len() as u32, ValType::I32)]
        });

        emit_expression(&export.body, &local_indices, &mut func)?;
        func.instruction(&Instruction::End);
        code_section.function(&func);

        export_section.export(&export.name, ExportKind::Func, next_function_index);
        next_function_index += 1;
    }

    module.section(&type_section);
    module.section(&function_section);
    module.section(&export_section);
    module.section(&code_section);

    Ok(module.finish())
}

fn collect_wasm_exports(
    context: &interpret::Context,
) -> Result<Vec<WasmFunctionExport>, Diagnostic> {
    let mut exports = Vec::new();
    for binding in context.annotated_bindings() {
        let AnnotatedBinding {
            name,
            annotations,
            value,
        } = binding;
        let Some(annotation_span) = wasm_annotation_span(&annotations) else {
            continue;
        };
        let export = lower_function_export(&name, value, annotation_span)?;
        exports.push(export);
    }
    exports.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(exports)
}

fn lower_function_export(
    binding_name: &str,
    value: Expression,
    annotation_span: SourceSpan,
) -> Result<WasmFunctionExport, Diagnostic> {
    let Expression::Function {
        parameter,
        return_type,
        body,
        ..
    } = value
    else {
        return Err(Diagnostic::new(format!(
            "Only functions can be exported to wasm (binding `{binding_name}`)"
        ))
        .with_span(annotation_span));
    };

    if !is_supported_wasm_type(return_type.as_ref()) {
        return Err(Diagnostic::new(
            "Only functions returning i32 or bool can be exported to wasm",
        )
        .with_span(return_type.span()));
    }

    let params = extract_function_params(parameter)?;

    Ok(WasmFunctionExport {
        name: binding_name.to_string(),
        params,
        body: *body,
    })
}

fn wasm_annotation_span(annotations: &[BindingAnnotation]) -> Option<SourceSpan> {
    annotations.iter().find_map(|annotation| match annotation {
        BindingAnnotation::Export(target_expr, span)
            if matches!(
                target_expr,
                Expression::Literal(ExpressionLiteral::Target(TargetLiteral::WasmTarget), _)
            ) =>
        {
            Some(*span)
        }
        _ => None,
    })
}

fn extract_function_params(pattern: BindingPattern) -> Result<Vec<WasmFunctionParam>, Diagnostic> {
    match pattern {
        BindingPattern::Struct(fields, span) => {
            if fields.is_empty() {
                Ok(Vec::new())
            } else {
                Err(
                    Diagnostic::new("Wasm exports currently support at most one parameter")
                        .with_span(span),
                )
            }
        }
        BindingPattern::Annotated { pattern, .. } => extract_function_params(*pattern),
        BindingPattern::TypeHint(inner, ty_expr, _) => {
            if !is_supported_wasm_type(ty_expr.as_ref()) {
                return Err(
                    Diagnostic::new("Only i32 or bool parameters can be exported to wasm")
                        .with_span(ty_expr.span()),
                );
            }
            let name = extract_identifier_from_pattern(*inner)?;
            Ok(vec![WasmFunctionParam {
                name,
                ty: ValType::I32,
            }])
        }
        BindingPattern::Identifier(_, _) => Err(Diagnostic::new(
            "Wasm exports currently require parameter type hints",
        )
        .with_span(pattern.span())),
    }
}

fn extract_identifier_from_pattern(pattern: BindingPattern) -> Result<String, Diagnostic> {
    match pattern {
        BindingPattern::Identifier(identifier, _) => Ok(identifier.0),
        BindingPattern::Annotated { pattern, .. } => extract_identifier_from_pattern(*pattern),
        BindingPattern::TypeHint(pattern, _, _) => extract_identifier_from_pattern(*pattern),
        other => Err(
            Diagnostic::new("Only identifier parameters can be exported to wasm")
                .with_span(other.span()),
        ),
    }
}

fn is_supported_wasm_type(expr: &Expression) -> bool {
    match expr {
        Expression::IntrinsicType(IntrinsicType::I32, _) => true,
        Expression::IntrinsicType(IntrinsicType::Boolean, _) => true,
        Expression::AttachImplementation { type_expr, .. } => is_supported_wasm_type(type_expr),
        _ => false,
    }
}

fn collect_locals(expr: &Expression) -> Result<Vec<(String, ValType)>, Diagnostic> {
    let mut locals = Vec::new();
    match expr {
        Expression::Binding(binding, _) => {
            let name = extract_identifier_from_pattern(binding.pattern.clone())?;
            locals.push((name, ValType::I32));
            // Also recurse into the expression?
            // A binding expression usually doesn't contain other bindings unless it's a block?
            // But `binding.expr` is an expression.
            locals.extend(collect_locals(&binding.expr)?);
        }
        Expression::Block(exprs, _) => {
            for e in exprs {
                locals.extend(collect_locals(e)?);
            }
        }
        Expression::Function { body: _body, .. } => {
            // Locals in inner functions are separate?
            // But we don't support compiling inner functions to WASM closures yet.
            // So we can ignore them or error?
            // For now, let's ignore them as they won't be reached by the main emission if we don't support them.
        }
        Expression::IntrinsicOperation(IntrinsicOperation::Binary(left, right, _), _) => {
            locals.extend(collect_locals(left)?);
            locals.extend(collect_locals(right)?);
        }
        Expression::FunctionCall {
            function, argument, ..
        } => {
            locals.extend(collect_locals(function)?);
            locals.extend(collect_locals(argument)?);
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            locals.extend(collect_locals(condition)?);
            locals.extend(collect_locals(then_branch)?);
            if let Some(else_branch) = else_branch {
                locals.extend(collect_locals(else_branch)?);
            }
        }
        // Add other recursive cases if needed
        _ => {}
    }
    Ok(locals)
}

fn emit_expression(
    expr: &Expression,
    locals: &std::collections::HashMap<String, u32>,
    func: &mut Function,
) -> Result<(), Diagnostic> {
    match expr {
        Expression::Literal(ExpressionLiteral::Number(value), _) => {
            func.instruction(&Instruction::I32Const(*value));
            Ok(())
        }
        Expression::Literal(ExpressionLiteral::Boolean(value), _) => {
            func.instruction(&Instruction::I32Const(if *value { 1 } else { 0 }));
            Ok(())
        }
        Expression::Identifier(identifier, span) => {
            let local_index = locals.get(&identifier.0).copied().ok_or_else(|| {
                Diagnostic::new(format!(
                    "Identifier `{}` is not a local variable or parameter",
                    identifier.0
                ))
                .with_span(*span)
            })?;
            func.instruction(&Instruction::LocalGet(local_index));
            Ok(())
        }
        Expression::IntrinsicOperation(IntrinsicOperation::Binary(left, right, op), _) => {
            emit_expression(left, locals, func)?;
            emit_expression(right, locals, func)?;
            match op {
                BinaryIntrinsicOperator::I32Add => func.instruction(&Instruction::I32Add),
                BinaryIntrinsicOperator::I32Subtract => func.instruction(&Instruction::I32Sub),
                BinaryIntrinsicOperator::I32Multiply => func.instruction(&Instruction::I32Mul),
                BinaryIntrinsicOperator::I32Divide => func.instruction(&Instruction::I32DivS),
                BinaryIntrinsicOperator::I32Equal => func.instruction(&Instruction::I32Eq),
                BinaryIntrinsicOperator::I32NotEqual => func.instruction(&Instruction::I32Ne),
                BinaryIntrinsicOperator::I32LessThan => func.instruction(&Instruction::I32LtS),
                BinaryIntrinsicOperator::I32GreaterThan => func.instruction(&Instruction::I32GtS),
                BinaryIntrinsicOperator::I32LessThanOrEqual => {
                    func.instruction(&Instruction::I32LeS)
                }
                BinaryIntrinsicOperator::I32GreaterThanOrEqual => {
                    func.instruction(&Instruction::I32GeS)
                }
                BinaryIntrinsicOperator::BooleanAnd => func.instruction(&Instruction::I32And),
                BinaryIntrinsicOperator::BooleanOr => func.instruction(&Instruction::I32Or),
                BinaryIntrinsicOperator::BooleanXor => func.instruction(&Instruction::I32Xor),
            };
            Ok(())
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            emit_expression(condition, locals, func)?;
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
                ValType::I32,
            )));
            emit_expression(then_branch, locals, func)?;
            func.instruction(&Instruction::Else);
            if let Some(else_expr) = else_branch {
                emit_expression(else_expr, locals, func)?;
            } else {
                func.instruction(&Instruction::I32Const(0));
            }
            func.instruction(&Instruction::End);
            Ok(())
        }
        Expression::Binding(binding, _) => {
            emit_expression(&binding.expr, locals, func)?;
            let name = extract_identifier_from_pattern(binding.pattern.clone())?;
            let local_index = locals
                .get(&name)
                .copied()
                .expect("Local should have been collected");
            func.instruction(&Instruction::LocalSet(local_index));
            // Bindings don't leave a value on the stack in our block logic,
            // but if they are part of a block, the block handles dropping if needed?
            // Wait, if I have `let x = 1; let y = 2;`, the block will emit:
            // emit(let x = 1) -> sets local, stack empty.
            // emit(let y = 2) -> sets local, stack empty.
            // So we are good.
            Ok(())
        }
        Expression::Block(exprs, _) => {
            for (i, e) in exprs.iter().enumerate() {
                emit_expression(e, locals, func)?;
                // If it's not the last expression, and it left something on the stack, we should drop it.
                // But how do we know if it left something?
                // `Expression::Binding` leaves nothing (we just implemented it).
                // `Expression::Literal`, `Identifier`, `IntrinsicOperation` leave 1 value.
                // So we should drop if it's not a binding.
                if i < exprs.len() - 1 {
                    if !matches!(e, Expression::Binding(..)) {
                        func.instruction(&Instruction::Drop);
                    }
                }
            }
            Ok(())
        }
        other => Err(
            Diagnostic::new("Expression is not supported in wasm exports yet")
                .with_span(other.span()),
        ),
    }
}
