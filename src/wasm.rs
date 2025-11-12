use wasm_encoder::{
    CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction, Module,
    TypeSection, ValType,
};

use crate::{
    diagnostics::{Diagnostic, SourceSpan},
    interpret::{self, AnnotatedBinding},
    parsing::{
        BindingAnnotation, BindingPattern, Expression, ExpressionLiteral, IntrinsicType,
        TargetLiteral,
    },
};

struct ConstFunctionExport {
    name: String,
    value: i32,
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
        type_section.function(Vec::new(), vec![ValType::I32]);
        function_section.function(next_type_index);
        next_type_index += 1;

        let mut func = Function::new(Vec::new());
        func.instruction(&Instruction::I32Const(export.value));
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
) -> Result<Vec<ConstFunctionExport>, Diagnostic> {
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
        let const_value = extract_const_function_value(&name, value, annotation_span)?;
        exports.push(ConstFunctionExport {
            name,
            value: const_value,
        });
    }
    exports.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(exports)
}

fn extract_const_function_value(
    binding_name: &str,
    value: Expression,
    annotation_span: SourceSpan,
) -> Result<i32, Diagnostic> {
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

    if !is_zero_arg_pattern(&parameter) {
        return Err(Diagnostic::new(
            "Only zero-argument functions can be exported to wasm right now",
        )
        .with_span(parameter.span()));
    }

    if !is_i32_type(return_type.as_ref()) {
        return Err(
            Diagnostic::new("Only functions returning i32 can be exported to wasm")
                .with_span(return_type.span()),
        );
    }

    let body_span = body.span();
    match *body {
        Expression::Literal(ExpressionLiteral::Number(value), _) => Ok(value),
        _ => Err(
            Diagnostic::new("Only constant numeric function bodies can be exported to wasm")
                .with_span(body_span),
        ),
    }
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

fn is_zero_arg_pattern(pattern: &BindingPattern) -> bool {
    match pattern {
        BindingPattern::Struct(fields, _) => fields.is_empty(),
        BindingPattern::TypeHint(inner, _, _) => is_zero_arg_pattern(inner),
        BindingPattern::Annotated { pattern, .. } => is_zero_arg_pattern(pattern),
        _ => false,
    }
}

fn is_i32_type(expr: &Expression) -> bool {
    match expr {
        Expression::IntrinsicType(IntrinsicType::I32, _) => true,
        Expression::AttachImplementation { type_expr, .. } => is_i32_type(type_expr),
        _ => false,
    }
}
