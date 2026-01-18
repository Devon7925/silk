use std::collections::HashSet;

use crate::{
    diagnostics::Diagnostic,
    intermediate::{
        IntermediateExportType, IntermediateKind, IntermediateLValue, IntermediateResult,
        IntermediateType, IntermediateWrap,
    },
    parsing::{BinaryIntrinsicOperator, BindingPattern, ExpressionLiteral, TargetLiteral},
};

pub fn compile_exports(intermediate: &IntermediateResult) -> Result<String, Diagnostic> {
    let mut output = String::new();

    let js_wrappers: Vec<&IntermediateWrap> = intermediate
        .wrappers
        .iter()
        .filter(|wrap| wrap.wrap_target == TargetLiteral::JSTarget)
        .collect();
    let wasm_wrappers: Vec<&IntermediateWrap> = intermediate
        .wrappers
        .iter()
        .filter(|wrap| wrap.wrap_target == TargetLiteral::WasmTarget)
        .collect();

    let needs_wasm_loader = js_wrappers
        .iter()
        .any(|wrap| wrap.source_target == TargetLiteral::WasmTarget)
        || wasm_wrappers.iter().any(|wrap| {
            matches!(
                wrap.source_target,
                TargetLiteral::JSTarget | TargetLiteral::WgslTarget
            )
        });
    let needs_wgsl_runner = js_wrappers
        .iter()
        .any(|wrap| wrap.source_target == TargetLiteral::WgslTarget)
        || wasm_wrappers
            .iter()
            .any(|wrap| wrap.source_target == TargetLiteral::WgslTarget);
    let mut wgsl_wrapper_names = HashSet::new();

    // First, generate all functions as internal functions
    for (idx, function) in intermediate.functions.iter().enumerate() {
        let (params, destructuring) = flatten_parameters(&function.parameter)?;

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

    if needs_wgsl_runner {
        output.push_str("async function __silk_run_wgsl(entryPoint, inputValues, outputCount) {\n");
        output.push_str("  const wgslUrl = new URL(import.meta.url);\n");
        output.push_str("  wgslUrl.pathname = wgslUrl.pathname.replace(/\\.js$/, \".wgsl\");\n");
        output.push_str("  const wgsl = await fetch(wgslUrl).then((res) => res.text());\n");
        output.push_str("  const adapter = await navigator.gpu?.requestAdapter();\n");
        output.push_str("  if (!adapter) { throw new Error(\"No GPU adapter available\"); }\n");
        output.push_str("  const device = await adapter.requestDevice();\n");
        output.push_str("  const module = device.createShaderModule({ code: wgsl });\n");
        output.push_str("  const pipeline = device.createComputePipeline({ layout: \"auto\", compute: { module, entryPoint } });\n");
        output.push_str("  const bindGroupLayout = pipeline.getBindGroupLayout(0);\n");
        output.push_str("  const findBinding = (name) => {\n");
        output.push_str("    const varToken = \"var<storage, read_write> \" + name;\n");
        output.push_str("    const varIndex = wgsl.indexOf(varToken);\n");
        output.push_str("    if (varIndex === -1) return null;\n");
        output.push_str("    const marker = \"@binding(\";\n");
        output.push_str("    const bindingIndex = wgsl.lastIndexOf(marker, varIndex);\n");
        output.push_str("    if (bindingIndex === -1) return null;\n");
        output.push_str("    const start = bindingIndex + marker.length;\n");
        output.push_str("    const end = wgsl.indexOf(\")\", start);\n");
        output.push_str("    if (end === -1) return null;\n");
        output.push_str("    return Number(wgsl.slice(start, end));\n");
        output.push_str("  };\n");
        output.push_str("  const inBinding = inputValues.length ? findBinding(\"silk_in_\" + entryPoint) : null;\n");
        output.push_str("  const outBinding = outputCount ? findBinding(\"silk_out_\" + entryPoint) : null;\n");
        output.push_str("  const makeStorage = (count) => device.createBuffer({ size: Math.max(16, count * 4), usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_SRC | GPUBufferUsage.COPY_DST });\n");
        output.push_str("  const input = inputValues.length ? makeStorage(inputValues.length) : null;\n");
        output.push_str("  const output = outputCount ? makeStorage(outputCount) : null;\n");
        output.push_str("  if (input) device.queue.writeBuffer(input, 0, new Int32Array(inputValues));\n");
        output.push_str("  if (output) device.queue.writeBuffer(output, 0, new Int32Array(outputCount));\n");
        output.push_str("  const entries = [];\n");
        output.push_str("  if (input && inBinding !== null) entries.push({ binding: inBinding, resource: { buffer: input } });\n");
        output.push_str("  if (output && outBinding !== null) entries.push({ binding: outBinding, resource: { buffer: output } });\n");
        output.push_str("  const bindGroup = device.createBindGroup({ layout: bindGroupLayout, entries });\n");
        output.push_str("  const encoder = device.createCommandEncoder();\n");
        output.push_str("  const pass = encoder.beginComputePass();\n");
        output.push_str("  pass.setPipeline(pipeline);\n");
        output.push_str("  pass.setBindGroup(0, bindGroup);\n");
        output.push_str("  pass.dispatchWorkgroups(1);\n");
        output.push_str("  pass.end();\n");
        output.push_str("  device.queue.submit([encoder.finish()]);\n");
        output.push_str("  await device.queue.onSubmittedWorkDone();\n");
        output.push_str("  if (!outputCount) { return undefined; }\n");
        output.push_str("  const read = device.createBuffer({ size: Math.max(16, outputCount * 4), usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.MAP_READ });\n");
        output.push_str("  const copy = device.createCommandEncoder();\n");
        output.push_str("  copy.copyBufferToBuffer(output, 0, read, 0, Math.max(16, outputCount * 4));\n");
        output.push_str("  device.queue.submit([copy.finish()]);\n");
        output.push_str("  await device.queue.onSubmittedWorkDone();\n");
        output.push_str("  await read.mapAsync(GPUMapMode.READ);\n");
        output.push_str("  const view = new DataView(read.getMappedRange());\n");
        output.push_str("  const value = view.getInt32(0, true);\n");
        output.push_str("  read.unmap();\n");
        output.push_str("  return value;\n");
        output.push_str("}\n");
        output.push_str("function __silk_run_wgsl_sync(entryPoint, inputValues, outputCount) {\n");
        output.push_str("  if (typeof SharedArrayBuffer === \"undefined\" || typeof Atomics === \"undefined\" || typeof Atomics.wait !== \"function\" || typeof Worker === \"undefined\") {\n");
        output.push_str("    throw new Error(\"Synchronous WGSL wrapper requires SharedArrayBuffer, Atomics.wait, and Worker support.\");\n");
        output.push_str("  }\n");
        output.push_str("  const sab = new SharedArrayBuffer(8);\n");
        output.push_str("  const view = new Int32Array(sab);\n");
        output.push_str("  const wgslUrl = new URL(import.meta.url);\n");
        output.push_str("  wgslUrl.pathname = wgslUrl.pathname.replace(/\\.js$/, \".wgsl\");\n");
        output.push_str("  const workerSrc = `\n");
        output.push_str("    onmessage = async (e) => {\n");
        output.push_str("      const { entryPoint, inputValues, outputCount, wgslUrl, sab } = e.data;\n");
        output.push_str("      const view = new Int32Array(sab);\n");
        output.push_str("      try {\n");
        output.push_str("        const wgsl = await fetch(wgslUrl).then((res) => res.text());\n");
        output.push_str("        const adapter = await navigator.gpu?.requestAdapter();\n");
        output.push_str("        if (!adapter) throw new Error(\"No GPU adapter available\");\n");
        output.push_str("        const device = await adapter.requestDevice();\n");
        output.push_str("        const module = device.createShaderModule({ code: wgsl });\n");
        output.push_str("        const pipeline = device.createComputePipeline({ layout: \"auto\", compute: { module, entryPoint } });\n");
        output.push_str("        const bindGroupLayout = pipeline.getBindGroupLayout(0);\n");
        output.push_str("        const findBinding = (name) => {\n");
        output.push_str("          const varToken = \"var<storage, read_write> \" + name;\n");
        output.push_str("          const varIndex = wgsl.indexOf(varToken);\n");
        output.push_str("          if (varIndex === -1) return null;\n");
        output.push_str("          const marker = \"@binding(\";\n");
        output.push_str("          const bindingIndex = wgsl.lastIndexOf(marker, varIndex);\n");
        output.push_str("          if (bindingIndex === -1) return null;\n");
        output.push_str("          const start = bindingIndex + marker.length;\n");
        output.push_str("          const end = wgsl.indexOf(\")\", start);\n");
        output.push_str("          if (end === -1) return null;\n");
        output.push_str("          return Number(wgsl.slice(start, end));\n");
        output.push_str("        };\n");
        output.push_str("        const inBinding = inputValues.length ? findBinding(\"silk_in_\" + entryPoint) : null;\n");
        output.push_str("        const outBinding = outputCount ? findBinding(\"silk_out_\" + entryPoint) : null;\n");
        output.push_str("        const makeStorage = (count) => device.createBuffer({ size: Math.max(16, count * 4), usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_SRC | GPUBufferUsage.COPY_DST });\n");
        output.push_str("        const input = inputValues.length ? makeStorage(inputValues.length) : null;\n");
        output.push_str("        const output = outputCount ? makeStorage(outputCount) : null;\n");
        output.push_str("        if (input) device.queue.writeBuffer(input, 0, new Int32Array(inputValues));\n");
        output.push_str("        if (output) device.queue.writeBuffer(output, 0, new Int32Array(outputCount));\n");
        output.push_str("        const entries = [];\n");
        output.push_str("        if (input && inBinding !== null) entries.push({ binding: inBinding, resource: { buffer: input } });\n");
        output.push_str("        if (output && outBinding !== null) entries.push({ binding: outBinding, resource: { buffer: output } });\n");
        output.push_str("        const bindGroup = device.createBindGroup({ layout: bindGroupLayout, entries });\n");
        output.push_str("        const encoder = device.createCommandEncoder();\n");
        output.push_str("        const pass = encoder.beginComputePass();\n");
        output.push_str("        pass.setPipeline(pipeline);\n");
        output.push_str("        pass.setBindGroup(0, bindGroup);\n");
        output.push_str("        pass.dispatchWorkgroups(1);\n");
        output.push_str("        pass.end();\n");
        output.push_str("        device.queue.submit([encoder.finish()]);\n");
        output.push_str("        await device.queue.onSubmittedWorkDone();\n");
        output.push_str("        let value = 0;\n");
        output.push_str("        if (outputCount) {\n");
        output.push_str("          const read = device.createBuffer({ size: Math.max(16, outputCount * 4), usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.MAP_READ });\n");
        output.push_str("          const copy = device.createCommandEncoder();\n");
        output.push_str("          copy.copyBufferToBuffer(output, 0, read, 0, Math.max(16, outputCount * 4));\n");
        output.push_str("          device.queue.submit([copy.finish()]);\n");
        output.push_str("          await device.queue.onSubmittedWorkDone();\n");
        output.push_str("          await read.mapAsync(GPUMapMode.READ);\n");
        output.push_str("          const dataView = new DataView(read.getMappedRange());\n");
        output.push_str("          value = dataView.getInt32(0, true);\n");
        output.push_str("          read.unmap();\n");
        output.push_str("        }\n");
        output.push_str("        view[1] = value;\n");
        output.push_str("        Atomics.store(view, 0, 1);\n");
        output.push_str("        Atomics.notify(view, 0);\n");
        output.push_str("      } catch (err) {\n");
        output.push_str("        console.error(err);\n");
        output.push_str("        Atomics.store(view, 0, 2);\n");
        output.push_str("        Atomics.notify(view, 0);\n");
        output.push_str("      }\n");
        output.push_str("    };\n");
        output.push_str("  `;\n");
        output.push_str("  const worker = new Worker(URL.createObjectURL(new Blob([workerSrc], { type: \"text/javascript\" })), { type: \"module\" });\n");
        output.push_str("  worker.postMessage({ entryPoint, inputValues, outputCount, wgslUrl: wgslUrl.href, sab });\n");
        output.push_str("  Atomics.wait(view, 0, 0);\n");
        output.push_str("  worker.terminate();\n");
        output.push_str("  if (view[0] === 2) {\n");
        output.push_str("    throw new Error(\"wgsl wrapper failed\");\n");
        output.push_str("  }\n");
        output.push_str("  return outputCount ? view[1] : undefined;\n");
        output.push_str("}\n");
    }

    if needs_wasm_loader {
        output.push_str("let __silk_wasm_exports;\n");
        output.push_str("let __silk_wasm_promise;\n");
        if !wasm_wrappers.is_empty() {
            output.push_str("function __silk_build_wasm_imports() {\n");
            output.push_str("  return { silk: {\n");
            for wrap in &wasm_wrappers {
                let import_name = wrapper_import_name(&wrap.name);
                match wrap.export_type {
                    IntermediateExportType::Function => {
                        let call_target = match wrap.source_target {
                            TargetLiteral::JSTarget => format!("__silk_fn_{}", wrap.index),
                            TargetLiteral::WgslTarget => {
                                format!("__silk_js_wrap_sync_{}", wrap.name)
                            }
                            TargetLiteral::WasmTarget => wrap.name.clone(),
                        };
                        output.push_str(&format!(
                            "    {}: (...args) => {}(...args),\n",
                            import_name, call_target
                        ));
                    }
                    IntermediateExportType::Global => {
                        let call_target = match wrap.source_target {
                            TargetLiteral::JSTarget => format!("__silk_fn_{}", wrap.index),
                            TargetLiteral::WgslTarget => format!("__silk_js_wrap_{}", wrap.name),
                            TargetLiteral::WasmTarget => wrap.name.clone(),
                        };
                        output.push_str(&format!(
                            "    {}: () => {},\n",
                            import_name, call_target
                        ));
                    }
                }
            }
                output.push_str("  } };\n");
            output.push_str("}\n");
            output.push_str("const __silk_wasm_imports_internal = __silk_build_wasm_imports();\n");
        } else {
            output.push_str("const __silk_wasm_imports_internal = {};\n");
        }
        output.push_str("async function __silk_load_wasm() {\n");
        output.push_str("  if (__silk_wasm_exports) return __silk_wasm_exports;\n");
        output.push_str("  if (__silk_wasm_promise) return __silk_wasm_promise;\n");
        output.push_str("  const wasmUrl = new URL(import.meta.url);\n");
        output.push_str("  wasmUrl.pathname = wasmUrl.pathname.replace(/\\.js$/, \".wasm\");\n");
        output.push_str("  __silk_wasm_promise = fetch(wasmUrl)\n");
        output.push_str("    .then((res) => res.arrayBuffer())\n");
        output.push_str("    .then((bytes) => WebAssembly.instantiate(bytes, __silk_wasm_imports_internal))\n");
        output.push_str("    .then((result) => {\n");
        output.push_str("      __silk_wasm_exports = result.instance.exports;\n");
        output.push_str("      return __silk_wasm_exports;\n");
        output.push_str("    });\n");
        output.push_str("  return __silk_wasm_promise;\n");
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

    for wrap in &js_wrappers {
        match wrap.export_type {
            IntermediateExportType::Function => {
                let function = intermediate
                    .functions
                    .get(wrap.index)
                    .ok_or_else(|| {
                        Diagnostic::new("Missing function for js wrapper".to_string())
                    })?;
                let input_count = flatten_input_count(function.input_type.as_ref())?;
                let output_count = wgsl_output_count(function.return_type.as_ref())?;
                let accessors = pattern_object_accessors(&function.parameter);
                let wrapper_name = wrap.name.clone();
                match wrap.source_target {
                    TargetLiteral::WasmTarget => {
                        output.push_str(&format!(
                            "export const {} = async (...args) => {{\n",
                            wrapper_name
                        ));
                        output.push_str("  const wasm = await __silk_load_wasm();\n");
                        if let Some(accessors) = accessors {
                            output.push_str(
                                "  if (args.length === 1 && args[0] && typeof args[0] === \"object\") {\n",
                            );
                            output.push_str("    const value = args[0];\n");
                            output.push_str("    args = [");
                            output.push_str(&accessors.join(", "));
                            output.push_str("];\n");
                            output.push_str("  }\n");
                        }
                        output.push_str(&format!("  return wasm.{}(...args);\n", wrapper_name));
                        output.push_str("};\n");
                    }
                    TargetLiteral::WgslTarget => {
                        if input_count.is_none() || output_count.is_none() {
                            return Err(Diagnostic::new(
                                "wgsl wrapper only supports i32 inputs and outputs".to_string(),
                            ));
                        }
                        output.push_str(&format!(
                            "async function __silk_js_wrap_{}(...args) {{\n",
                            wrapper_name
                        ));
                        if let Some(accessors) = accessors {
                            output.push_str(
                                "  if (args.length === 1 && args[0] && typeof args[0] === \"object\") {\n",
                            );
                            output.push_str("    const value = args[0];\n");
                            output.push_str("    args = [");
                            output.push_str(&accessors.join(", "));
                            output.push_str("];\n");
                            output.push_str("  }\n");
                        }
                        output.push_str(&format!(
                            "  return __silk_run_wgsl(\"{}\", args, {});\n",
                            wrapper_name,
                            output_count.unwrap_or(0)
                        ));
                        output.push_str("}\n");
                        output.push_str(&format!(
                            "function __silk_js_wrap_sync_{}(...args) {{\n",
                            wrapper_name
                        ));
                        output.push_str(&format!(
                            "  return __silk_run_wgsl_sync(\"{}\", args, {});\n",
                            wrapper_name,
                            output_count.unwrap_or(0)
                        ));
                        output.push_str("}\n");
                        output.push_str(&format!(
                            "export const {} = __silk_js_wrap_{};\n",
                            wrapper_name, wrapper_name
                        ));
                        wgsl_wrapper_names.insert(wrapper_name.clone());
                    }
                    TargetLiteral::JSTarget => {}
                }
            }
            IntermediateExportType::Global => {
                match wrap.source_target {
                    TargetLiteral::WasmTarget => {
                        output.push_str(&format!("export const {} = async () => {{\n", wrap.name));
                        output.push_str("  const wasm = await __silk_load_wasm();\n");
                        output.push_str(&format!("  const value = wasm.{};\n", wrap.name));
                        output.push_str(
                            "  return value && typeof value === \"object\" && \"value\" in value ? value.value : value;\n",
                        );
                        output.push_str("};\n");
                    }
                    _ => {
                        return Err(Diagnostic::new(
                            "wrap annotation does not support globals for js target".to_string(),
                        ));
                    }
                }
            }
        }
    }

    for wrap in &wasm_wrappers {
        if wrap.source_target != TargetLiteral::WgslTarget {
            continue;
        }
        if wgsl_wrapper_names.contains(&wrap.name) {
            continue;
        }
        let function = intermediate
            .functions
            .get(wrap.index)
            .ok_or_else(|| Diagnostic::new("Missing function for wgsl wrapper".to_string()))?;
        let input_count = flatten_input_count(function.input_type.as_ref())?;
        let output_count = wgsl_output_count(function.return_type.as_ref())?;
        if input_count.is_none() || output_count.is_none() {
            return Err(Diagnostic::new(
                "wgsl wrapper only supports i32 inputs and outputs".to_string(),
            ));
        }
        output.push_str(&format!(
            "async function __silk_js_wrap_{}(...args) {{\n",
            wrap.name
        ));
        if let Some(accessors) = pattern_object_accessors(&function.parameter) {
            output.push_str(
                "  if (args.length === 1 && args[0] && typeof args[0] === \"object\") {\n",
            );
            output.push_str("    const value = args[0];\n");
            output.push_str("    args = [");
            output.push_str(&accessors.join(", "));
            output.push_str("];\n");
            output.push_str("  }\n");
        }
        output.push_str(&format!(
            "  return __silk_run_wgsl(\"{}\", args, {});\n",
            wrap.name,
            output_count.unwrap_or(0)
        ));
        output.push_str("}\n");
        output.push_str(&format!(
            "function __silk_js_wrap_sync_{}(...args) {{\n",
            wrap.name
        ));
        output.push_str(&format!(
            "  return __silk_run_wgsl_sync(\"{}\", args, {});\n",
            wrap.name,
            output_count.unwrap_or(0)
        ));
        output.push_str("}\n");
        wgsl_wrapper_names.insert(wrap.name.clone());
    }

    if !wasm_wrappers.is_empty() {
        output.push_str("export function __silk_wasm_imports() {\n");
        output.push_str("  return __silk_build_wasm_imports();\n");
        output.push_str("}\n");
    }

    Ok(output)
}

fn wrapper_import_name(name: &str) -> String {
    format!("__silk_wrap_{}", name)
}

fn flatten_input_count(ty: &IntermediateType) -> Result<Option<usize>, Diagnostic> {
    match ty {
        IntermediateType::I32 | IntermediateType::U8 => Ok(Some(1)),
        IntermediateType::Struct(fields) if fields.is_empty() => Ok(Some(0)),
        IntermediateType::Struct(_) => Ok(None),
        IntermediateType::Array { .. } => Ok(None),
        IntermediateType::Box { .. } => Ok(None),
    }
}

fn wgsl_output_count(ty: &IntermediateType) -> Result<Option<usize>, Diagnostic> {
    match ty {
        IntermediateType::I32 | IntermediateType::U8 => Ok(Some(1)),
        IntermediateType::Struct(fields) if fields.is_empty() => Ok(Some(0)),
        _ => Ok(None),
    }
}

fn pattern_object_accessors(pattern: &BindingPattern) -> Option<Vec<String>> {
    fn is_js_identifier(name: &str) -> bool {
        let mut chars = name.chars();
        let Some(first) = chars.next() else {
            return false;
        };
        if !(first.is_ascii_alphabetic() || first == '_') {
            return false;
        }
        chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
    }

    fn field_accessor(prefix: &str, name: &str) -> String {
        if is_js_identifier(name) {
            format!("{}.{}", prefix, name)
        } else {
            format!("{}[\"{}\"]", prefix, name)
        }
    }

    fn stripped_pattern<'a>(pattern: &'a BindingPattern) -> &'a BindingPattern {
        let mut current = pattern;
        loop {
            match current {
                BindingPattern::TypeHint(inner, _, _) => current = inner,
                BindingPattern::Annotated { pattern: inner, .. } => current = inner,
                other => return other,
            }
        }
    }

    fn walk(pattern: &BindingPattern, prefix: &str, out: &mut Vec<String>) -> bool {
        match pattern {
            BindingPattern::Identifier(_, _) => {
                out.push(prefix.to_string());
                true
            }
            BindingPattern::Struct(fields, _) => {
                for (field_name, field_pattern) in fields {
                    let access_name =
                        extract_simple_name(field_pattern).unwrap_or_else(|| field_name.name.clone());
                    let next = field_accessor(prefix, &access_name);
                    if !walk(field_pattern, &next, out) {
                        return false;
                    }
                }
                true
            }
            BindingPattern::TypeHint(inner, _, _) => walk(inner, prefix, out),
            BindingPattern::Annotated { pattern, .. } => walk(pattern, prefix, out),
            BindingPattern::Literal(_, _) => false,
            BindingPattern::EnumVariant { .. } => false,
        }
    }

    if !matches!(stripped_pattern(pattern), BindingPattern::Struct(_, _)) {
        return None;
    }

    let mut out = Vec::new();
    if walk(pattern, "value", &mut out) && !out.is_empty() {
        Some(out)
    } else {
        None
    }
}

fn flatten_parameters(pattern: &BindingPattern) -> Result<(Vec<String>, String), Diagnostic> {
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
        BindingPattern::TypeHint(inner, _, _) => flatten_parameters(inner),
        BindingPattern::Annotated { pattern, .. } => flatten_parameters(pattern),
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
        IntermediateKind::InlineAssembly { target, code } => {
            if *target != TargetLiteral::JSTarget {
                return Ok("(() => { throw new Error(\"inline asm target mismatch\"); })()"
                    .to_string());
            }
            let source = String::from_utf8(code.clone())
                .map_err(|_| Diagnostic::new("asm string must be valid UTF-8"))?;
            Ok(format!("({})", source))
        }
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
                } else if s.ends_with(";") {
                    stmts.push(s);
                } else {
                    stmts.push(format!("{};", s));
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
        IntermediateKind::BoxAlloc { value, .. } => compile_expression(value, intermediate),
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
        IntermediateKind::InlineAssembly { .. } => {}
        _ => {}
    }
}
