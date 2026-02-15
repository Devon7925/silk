use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::rc::Rc;
use std::sync::OnceLock;

use wasmtime::{Config, Engine, Instance, Memory, Module, Store};

use crate::diagnostics::{Diagnostic, SourceSpan};
use crate::interpret;
use crate::loader;
use crate::parsing::{BindingPattern, Expression, ExpressionKind, ExpressionLiteral, Identifier};
use crate::wasm;

const VALUE_NUMBER: i32 = 0;
const VALUE_BOOLEAN: i32 = 1;
const VALUE_CHAR: i32 = 2;
const VALUE_STRING: i32 = 3;
const VALUE_UNIT: i32 = 4;
const VALUE_FUNCTION: i32 = 5;
const VALUE_ENUM: i32 = 10;
const INTERP_ERR_ARRAY_INDEX_OUT_OF_RANGE: i32 = 2;
const INTERP_ERR_WRAP_REQUIRES_SINGLE_EXPORT_TARGET: i32 = 3;
const INTERP_ERR_WRAP_GLOBAL_ONLY_WASM_TO_JS: i32 = 4;
const INTERP_ERR_UNBOUND_IDENTIFIER: i32 = 5;
const INTERP_ERR_IF_BRANCH_TYPE_MISMATCH: i32 = 6;
const INTERP_ERR_MATCH_NO_BRANCH: i32 = 7;
const INTERP_ERR_MISSING_FIELD: i32 = 8;
const INTERP_ERR_TRAIT_MISSING_FIELD: i32 = 9;

const INTERPRETER_MODULE_CACHE_PATH: &str = "target/wasm_cache/interpreter.wasmtime";
const INTERPRETER_MODULE_HASH_PATH: &str = "target/wasm_cache/interpreter.wasmtime.hash";
const INTERPRETER_WASM_HASH_PATH: &str = "target/wasm_cache/interpreter.wasm.hash";

static PARSER_WASM_BYTES: OnceLock<Result<Vec<u8>, Diagnostic>> = OnceLock::new();
static INTERPRETER_WASM_BYTES: OnceLock<Result<Vec<u8>, Diagnostic>> = OnceLock::new();
static WASM_ENGINE: OnceLock<Result<Engine, Diagnostic>> = OnceLock::new();
static PARSER_MODULE: OnceLock<Result<Module, Diagnostic>> = OnceLock::new();
static INTERPRETER_MODULE: OnceLock<Result<Module, Diagnostic>> = OnceLock::new();

pub fn evaluate_text(program: &str) -> Result<Option<Expression>, Diagnostic> {
    if std::env::var_os("SILK_DISABLE_WASM_INTERPRETER").is_some() {
        return Ok(None);
    }

    let mut runtime = WasmRuntime::new()?;
    runtime.write_single_input(program)?;

    let root = runtime.parser_call0("parse")?;
    let parse_error_pos = runtime.parser_call0("get_state_error")?;
    if parse_error_pos != -1 {
        return Err(parse_error(program, parse_error_pos));
    }

    runtime.copy_parser_state_to_interpreter()?;

    let result_idx = runtime.interpreter_call1("interpret", root)?;
    let interp_error_pos = runtime.interpreter_call0("get_interp_error")?;
    if interp_error_pos != -1 {
        let interp_error_code = runtime.interpreter_call0("get_interp_error_code")?;
        return Err(interpreter_error(
            program,
            interp_error_pos,
            interp_error_code,
        ));
    }

    runtime.decode_value_expression(result_idx)
}

pub fn evaluate_files(
    files: Vec<(&str, &str)>,
    root: &str,
) -> Result<Option<Expression>, Diagnostic> {
    if std::env::var_os("SILK_DISABLE_WASM_INTERPRETER").is_some() {
        return Ok(None);
    }

    let root_path = loader::normalize_path(root);
    let mut file_sources: HashMap<String, String> = HashMap::with_capacity(files.len());
    for (path, source) in files {
        file_sources.insert(loader::normalize_path(path), source.to_string());
    }

    let root_source = file_sources
        .get(&root_path)
        .ok_or_else(|| Diagnostic::new(format!("Missing root source for {root_path}")))?
        .clone();

    let mut imports: Vec<(String, String)> = file_sources
        .into_iter()
        .filter(|(path, _)| path != &root_path)
        .collect();
    imports.sort_by(|(a, _), (b, _)| a.cmp(b));

    let mut runtime = WasmRuntime::new()?;

    let mut segments: Vec<String> = Vec::with_capacity(1 + imports.len() * 2);
    segments.push(root_source.clone());
    for (_, source) in &imports {
        segments.push(source.clone());
    }
    for (path, _) in &imports {
        segments.push(path.clone());
    }

    let segment_refs: Vec<&str> = segments.iter().map(String::as_str).collect();
    let offsets = runtime.write_segmented_input(&segment_refs)?;

    let root_idx = runtime.parser_call0("parse")?;
    let root_parse_error = runtime.parser_call0("get_state_error")?;
    if root_parse_error != -1 {
        return Err(parse_error(&root_source, root_parse_error));
    }

    let import_count = imports.len();
    let import_offsets = &offsets[1..1 + import_count];
    let path_offsets = &offsets[1 + import_count..];

    let mut import_roots = Vec::with_capacity(import_count);
    for (idx, offset) in import_offsets.iter().copied().enumerate() {
        let import_root = runtime.parser_call1("parse_at", offset)?;
        let parse_error_pos = runtime.parser_call0("get_state_error")?;
        if parse_error_pos != -1 {
            let rel_pos = parse_error_pos.saturating_sub(offset);
            return Err(parse_error(&imports[idx].1, rel_pos));
        }
        import_roots.push(import_root);
    }

    runtime.copy_parser_state_to_interpreter()?;

    let _ = runtime.interpreter_call0("clear_file_registry")?;
    for idx in 0..import_count {
        let path = &imports[idx].0;
        let path_len = to_i32(path.len(), "Path length does not fit i32")?;
        let _ = runtime.interpreter_call3(
            "register_file",
            path_offsets[idx],
            path_len,
            import_roots[idx],
        )?;
    }

    let result_idx = runtime.interpreter_call1("interpret", root_idx)?;
    let interp_error_pos = runtime.interpreter_call0("get_interp_error")?;
    if interp_error_pos != -1 {
        let interp_error_code = runtime.interpreter_call0("get_interp_error_code")?;
        let binding_count = runtime.interpreter_call0("get_binding_count").ok();
        let value_count = runtime.interpreter_call0("get_value_count").ok();
        let binding_snapshot = if std::env::var_os("SILK_DEBUG_WASM_BINDINGS").is_some() {
            runtime.debug_binding_snapshot().ok()
        } else {
            None
        };
        if let Some((path, source, rel_pos)) = map_error_position(
            interp_error_pos,
            &root_path,
            &root_source,
            &imports,
            import_offsets,
        ) {
            let mut diag = interpreter_error(source, rel_pos, interp_error_code);
            match (binding_count, value_count) {
                (Some(binding_count), Some(value_count)) => {
                    diag.message = format!(
                        "{} (code {interp_error_code}, bindings {binding_count}, values {value_count})",
                        diag.message
                    );
                }
                (Some(binding_count), None) => {
                    diag.message = format!(
                        "{} (code {interp_error_code}, bindings {binding_count})",
                        diag.message
                    );
                }
                (None, Some(value_count)) => {
                    diag.message = format!(
                        "{} (code {interp_error_code}, values {value_count})",
                        diag.message
                    );
                }
                (None, None) => {
                    diag.message = format!("{} (code {interp_error_code})", diag.message);
                }
            }
            if path != root_path {
                diag.message = format!("{path}: {}", diag.message);
            }
            if let Some(snapshot) = binding_snapshot.as_deref() {
                diag.message = format!("{}\n{}", diag.message, snapshot);
            }
            return Err(diag);
        }
        match (binding_count, value_count) {
            (Some(binding_count), Some(value_count)) => {
                let mut message = format!(
                    "Silk wasm interpreter reported an error (code {interp_error_code}, pos {interp_error_pos}, bindings {binding_count}, values {value_count})"
                );
                if let Some(snapshot) = binding_snapshot.as_deref() {
                    message.push('\n');
                    message.push_str(snapshot);
                }
                return Err(Diagnostic::new(message));
            }
            (Some(binding_count), None) => {
                let mut message = format!(
                    "Silk wasm interpreter reported an error (code {interp_error_code}, pos {interp_error_pos}, bindings {binding_count})"
                );
                if let Some(snapshot) = binding_snapshot.as_deref() {
                    message.push('\n');
                    message.push_str(snapshot);
                }
                return Err(Diagnostic::new(message));
            }
            (None, Some(value_count)) => {
                let mut message = format!(
                    "Silk wasm interpreter reported an error (code {interp_error_code}, pos {interp_error_pos}, values {value_count})"
                );
                if let Some(snapshot) = binding_snapshot.as_deref() {
                    message.push('\n');
                    message.push_str(snapshot);
                }
                return Err(Diagnostic::new(message));
            }
            (None, None) => {}
        }
        let mut message = format!(
            "Silk wasm interpreter reported an error (code {interp_error_code}, pos {interp_error_pos})"
        );
        if let Some(snapshot) = binding_snapshot.as_deref() {
            message.push('\n');
            message.push_str(snapshot);
        }
        return Err(Diagnostic::new(message));
    }

    runtime.decode_value_expression(result_idx)
}

fn map_error_position<'a>(
    absolute_pos: i32,
    root_path: &'a str,
    root_source: &'a str,
    imports: &'a [(String, String)],
    import_offsets: &[i32],
) -> Option<(&'a str, &'a str, i32)> {
    if absolute_pos < 0 {
        return None;
    }

    let root_len = root_source.len() as i32;
    if absolute_pos <= root_len {
        return Some((root_path, root_source, absolute_pos));
    }

    for ((path, source), start) in imports.iter().zip(import_offsets.iter().copied()) {
        let len = source.len() as i32;
        let end = start.saturating_add(len);
        if absolute_pos >= start && absolute_pos <= end {
            return Some((path.as_str(), source.as_str(), absolute_pos - start));
        }
    }

    None
}

struct WasmRuntime {
    store: Store<()>,
    parser_instance: Instance,
    interpreter_instance: Instance,
    parser_input: Memory,
    parser_nodes: Memory,
    parser_list_nodes: Memory,
    parser_state: Memory,
    interpreter_input: Memory,
    interpreter_nodes: Memory,
    interpreter_list_nodes: Memory,
    interpreter_state: Memory,
}

impl WasmRuntime {
    fn new() -> Result<Self, Diagnostic> {
        let engine = wasm_engine()?;
        let parser_module = parser_module()?;
        let interpreter_module = interpreter_module()?;

        let mut store = Store::new(engine, ());
        let parser_instance = Instance::new(&mut store, parser_module, &[]).map_err(|err| {
            Diagnostic::new(format!("Failed to instantiate silk parser wasm: {err}"))
        })?;
        let interpreter_instance =
            Instance::new(&mut store, interpreter_module, &[]).map_err(|err| {
                Diagnostic::new(format!(
                    "Failed to instantiate silk interpreter wasm: {err}"
                ))
            })?;

        let parser_input = parser_instance
            .get_memory(&mut store, "input")
            .ok_or_else(|| Diagnostic::new("Parser export `input` memory not found"))?;
        let parser_nodes = parser_instance
            .get_memory(&mut store, "nodes")
            .ok_or_else(|| Diagnostic::new("Parser export `nodes` memory not found"))?;
        let parser_list_nodes = parser_instance
            .get_memory(&mut store, "list_nodes")
            .ok_or_else(|| Diagnostic::new("Parser export `list_nodes` memory not found"))?;
        let parser_state = parser_instance
            .get_memory(&mut store, "state")
            .ok_or_else(|| Diagnostic::new("Parser export `state` memory not found"))?;

        let interpreter_input = interpreter_instance
            .get_memory(&mut store, "input")
            .ok_or_else(|| Diagnostic::new("Interpreter export `input` memory not found"))?;
        let interpreter_nodes = interpreter_instance
            .get_memory(&mut store, "nodes")
            .ok_or_else(|| Diagnostic::new("Interpreter export `nodes` memory not found"))?;
        let interpreter_list_nodes = interpreter_instance
            .get_memory(&mut store, "list_nodes")
            .ok_or_else(|| Diagnostic::new("Interpreter export `list_nodes` memory not found"))?;
        let interpreter_state = interpreter_instance
            .get_memory(&mut store, "state")
            .ok_or_else(|| Diagnostic::new("Interpreter export `state` memory not found"))?;

        Ok(Self {
            store,
            parser_instance,
            interpreter_instance,
            parser_input,
            parser_nodes,
            parser_list_nodes,
            parser_state,
            interpreter_input,
            interpreter_nodes,
            interpreter_list_nodes,
            interpreter_state,
        })
    }

    fn write_single_input(&mut self, source: &str) -> Result<(), Diagnostic> {
        let memory = self.parser_input;
        let data = memory.data_mut(&mut self.store);
        data.fill(0);
        let bytes = source.as_bytes();
        if bytes.len() + 1 > data.len() {
            return Err(Diagnostic::new(format!(
                "Input exceeds parser buffer (max {} bytes)",
                data.len().saturating_sub(1)
            )));
        }
        data[..bytes.len()].copy_from_slice(bytes);
        data[bytes.len()] = 0;
        Ok(())
    }

    fn write_segmented_input(&mut self, segments: &[&str]) -> Result<Vec<i32>, Diagnostic> {
        let memory = self.parser_input;
        let data = memory.data_mut(&mut self.store);
        data.fill(0);

        let mut offsets = Vec::with_capacity(segments.len());
        let mut cursor = 0usize;

        for segment in segments {
            offsets.push(to_i32(cursor, "Input cursor does not fit i32")?);
            let bytes = segment.as_bytes();
            let needed = bytes.len() + 1;
            if cursor + needed > data.len() {
                return Err(Diagnostic::new(format!(
                    "Input exceeds parser buffer (max {} bytes)",
                    data.len().saturating_sub(1)
                )));
            }
            data[cursor..cursor + bytes.len()].copy_from_slice(bytes);
            cursor += bytes.len();
            data[cursor] = 0;
            cursor += 1;
        }

        Ok(offsets)
    }

    fn parser_call0(&mut self, name: &str) -> Result<i32, Diagnostic> {
        let func = self
            .parser_instance
            .get_typed_func::<(), i32>(&mut self.store, name)
            .map_err(|err| Diagnostic::new(format!("Missing parser export `{name}`: {err}")))?;
        func.call(&mut self.store, ())
            .map_err(|err| Diagnostic::new(format!("Parser call `{name}` failed: {err}")))
    }

    fn parser_call1(&mut self, name: &str, arg: i32) -> Result<i32, Diagnostic> {
        let func = self
            .parser_instance
            .get_typed_func::<i32, i32>(&mut self.store, name)
            .map_err(|err| Diagnostic::new(format!("Missing parser export `{name}`: {err}")))?;
        func.call(&mut self.store, arg)
            .map_err(|err| Diagnostic::new(format!("Parser call `{name}` failed: {err}")))
    }

    fn interpreter_call0(&mut self, name: &str) -> Result<i32, Diagnostic> {
        let func = self
            .interpreter_instance
            .get_typed_func::<(), i32>(&mut self.store, name)
            .map_err(|err| {
                Diagnostic::new(format!("Missing interpreter export `{name}`: {err}"))
            })?;
        func.call(&mut self.store, ())
            .map_err(|err| Diagnostic::new(format!("Interpreter call `{name}` failed: {err}")))
    }

    fn interpreter_call1(&mut self, name: &str, arg: i32) -> Result<i32, Diagnostic> {
        let func = self
            .interpreter_instance
            .get_typed_func::<i32, i32>(&mut self.store, name)
            .map_err(|err| {
                Diagnostic::new(format!("Missing interpreter export `{name}`: {err}"))
            })?;
        func.call(&mut self.store, arg)
            .map_err(|err| Diagnostic::new(format!("Interpreter call `{name}` failed: {err}")))
    }

    fn interpreter_call3(
        &mut self,
        name: &str,
        arg0: i32,
        arg1: i32,
        arg2: i32,
    ) -> Result<i32, Diagnostic> {
        let func = self
            .interpreter_instance
            .get_typed_func::<(i32, i32, i32), i32>(&mut self.store, name)
            .map_err(|err| {
                Diagnostic::new(format!("Missing interpreter export `{name}`: {err}"))
            })?;
        func.call(&mut self.store, (arg0, arg1, arg2))
            .map_err(|err| Diagnostic::new(format!("Interpreter call `{name}` failed: {err}")))
    }

    fn copy_parser_state_to_interpreter(&mut self) -> Result<(), Diagnostic> {
        self.copy_memory(self.parser_input, self.interpreter_input)?;
        self.copy_memory(self.parser_nodes, self.interpreter_nodes)?;
        self.copy_memory(self.parser_list_nodes, self.interpreter_list_nodes)?;
        self.copy_memory(self.parser_state, self.interpreter_state)?;
        Ok(())
    }

    fn copy_memory(&mut self, src: Memory, dst: Memory) -> Result<(), Diagnostic> {
        let src_data = src.data(&self.store).to_vec();
        let dst_data = dst.data_mut(&mut self.store);

        if dst_data.len() < src_data.len() {
            return Err(Diagnostic::new(
                "Interpreter memory is smaller than parser memory",
            ));
        }

        dst_data.fill(0);
        dst_data[..src_data.len()].copy_from_slice(&src_data);
        Ok(())
    }

    fn decode_value_expression(
        &mut self,
        value_idx: i32,
    ) -> Result<Option<Expression>, Diagnostic> {
        let tag = self.interpreter_call1("get_value_tag", value_idx)?;
        let span = SourceSpan::default();

        match tag {
            VALUE_NUMBER => {
                let value = self.interpreter_call1("get_value_number", value_idx)?;
                Ok(Some(Expression::new(
                    ExpressionKind::Literal(ExpressionLiteral::Number(value)),
                    span,
                )))
            }
            VALUE_BOOLEAN => {
                let value = self.interpreter_call1("get_value_boolean", value_idx)?;
                Ok(Some(Expression::new(
                    ExpressionKind::Literal(ExpressionLiteral::Boolean(value != 0)),
                    span,
                )))
            }
            VALUE_CHAR => {
                let value = self.interpreter_call1("get_value_char", value_idx)?;
                if !(0..=255).contains(&value) {
                    return Ok(None);
                }
                Ok(Some(Expression::new(
                    ExpressionKind::Literal(ExpressionLiteral::Char(value as u8)),
                    span,
                )))
            }
            VALUE_STRING => {
                let start = self.interpreter_call1("get_value_string_start", value_idx)?;
                let len = self.interpreter_call1("get_value_string_length", value_idx)?;
                let Some(bytes) = self.decode_string_bytes(start, len)? else {
                    return Ok(None);
                };
                Ok(Some(Expression::new(
                    ExpressionKind::Literal(ExpressionLiteral::String(bytes)),
                    span,
                )))
            }
            VALUE_UNIT => Ok(Some(Expression::new(
                ExpressionKind::Struct(Vec::new()),
                span,
            ))),
            VALUE_FUNCTION => {
                let _ = self.interpreter_call1("get_value_function_node", value_idx)?;
                let body = Rc::new(Expression::new(ExpressionKind::Struct(Vec::new()), span));
                Ok(Some(Expression::new(
                    ExpressionKind::Function {
                        parameter: BindingPattern::Struct(Vec::new(), span),
                        return_type: None,
                        body,
                    },
                    span,
                )))
            }
            VALUE_ENUM => {
                let variant_start =
                    self.interpreter_call1("get_value_enum_variant_name_start", value_idx)?;
                let variant_len =
                    self.interpreter_call1("get_value_enum_variant_name_length", value_idx)?;
                let Some(variant_name_bytes) =
                    self.decode_string_bytes(variant_start, variant_len)?
                else {
                    return Ok(None);
                };
                let variant_name = String::from_utf8(variant_name_bytes).map_err(|err| {
                    Diagnostic::new(format!("Enum variant name is not valid UTF-8: {err}"))
                })?;

                let payload_idx = self.interpreter_call1("get_value_enum_payload", value_idx)?;
                let payload = if payload_idx >= 0 && payload_idx != value_idx {
                    self.decode_value_expression(payload_idx)?
                        .unwrap_or_else(|| {
                            Expression::new(ExpressionKind::Struct(Vec::new()), span)
                        })
                } else {
                    Expression::new(ExpressionKind::Struct(Vec::new()), span)
                };

                Ok(Some(Expression::new(
                    ExpressionKind::EnumValue {
                        enum_type: Rc::new(Expression::new(
                            ExpressionKind::EnumType(Vec::new()),
                            span,
                        )),
                        variant: Identifier::new(variant_name),
                        variant_index: 0,
                        payload: Rc::new(payload),
                    },
                    span,
                )))
            }
            _ => Ok(None),
        }
    }

    fn decode_string_bytes(&self, start: i32, len: i32) -> Result<Option<Vec<u8>>, Diagnostic> {
        if len < 0 {
            return Ok(None);
        }

        if start < 0 {
            let Some(synthetic) = synthetic_string_bytes(start, len) else {
                return Ok(None);
            };
            return Ok(Some(synthetic.to_vec()));
        }

        let start = start as usize;
        let len = len as usize;
        let data = self.interpreter_input.data(&self.store);
        if start + len > data.len() {
            return Err(Diagnostic::new(
                "String value is outside interpreter input memory",
            ));
        }
        Ok(Some(data[start..start + len].to_vec()))
    }

    fn debug_binding_snapshot(&mut self) -> Result<String, Diagnostic> {
        let binding_count = self.interpreter_call0("get_binding_count")?;
        let intrinsic_binding_count = self
            .interpreter_call0("get_intrinsic_binding_count")
            .unwrap_or(-1);
        let debug_error_binding_count = self
            .interpreter_call0("get_debug_error_binding_count")
            .unwrap_or(0);
        let debug_init_binding_count = self
            .interpreter_call0("get_debug_init_binding_count")
            .unwrap_or(-1);
        let snapshot_count = if binding_count > 0 {
            binding_count
        } else {
            debug_error_binding_count
        };
        let runtime_type_idx = self.interpreter_call0("get_runtime_type_value_idx")?;
        let mut lines = vec![format!(
            "wasm binding snapshot: binding_count={binding_count}, intrinsic_binding_count={intrinsic_binding_count}, debug_init_binding_count={debug_init_binding_count}, debug_error_binding_count={debug_error_binding_count}, runtime_type_value_idx={runtime_type_idx}"
        )];

        let start = (snapshot_count - 24).max(0);
        for idx in start..snapshot_count {
            let name_start = self.interpreter_call1("get_binding_name_start", idx)?;
            let name_len = self.interpreter_call1("get_binding_name_length", idx)?;
            let name = match self.decode_string_bytes(name_start, name_len)? {
                Some(bytes) => String::from_utf8_lossy(&bytes).into_owned(),
                None => format!("<invalid:{name_start}:{name_len}>"),
            };
            let value_idx = self.interpreter_call1("get_binding_value", idx)?;
            let binding_type_idx = self.interpreter_call1("get_binding_type_value", idx)?;
            let value_type_idx = if value_idx >= 0 {
                self.interpreter_call1("get_value_type_value", value_idx)?
            } else {
                -1
            };
            lines.push(format!(
                "  [{idx}] name={name} value_idx={value_idx} binding_type_idx={binding_type_idx} value_type_idx={value_type_idx}"
            ));
        }

        Ok(lines.join("\n"))
    }
}

fn synthetic_string_bytes(start: i32, len: i32) -> Option<&'static [u8]> {
    match (start, len) {
        (-2, 3) => Some(b"mut"),
        (-3, 6) => Some(b"Option"),
        (-4, 4) => Some(b"Some"),
        (-5, 7) => Some(b"IterTy"),
        (-6, 4) => Some(b"next"),
        (-7, 10) => Some(b"__for_iter"),
        (-8, 3) => Some(b"i32"),
        (-9, 2) => Some(b"u8"),
        (-10, 4) => Some(b"bool"),
        (-11, 4) => Some(b"type"),
        (-12, 6) => Some(b"target"),
        (-13, 18) => Some(b"binding_annotation"),
        (-14, 4) => Some(b"None"),
        (-15, 7) => Some(b"current"),
        (-16, 3) => Some(b"end"),
        (-17, 3) => Some(b"Box"),
        (-18, 2) => Some(b"js"),
        (-19, 4) => Some(b"wasm"),
        (-20, 4) => Some(b"wgsl"),
        (-21, 5) => Some(b"Range"),
        (-22, 1) => Some(b"+"),
        (-23, 1) => Some(b"-"),
        (-24, 1) => Some(b"*"),
        (-25, 1) => Some(b"/"),
        (-26, 2) => Some(b"=="),
        (-27, 2) => Some(b"!="),
        (-28, 1) => Some(b"<"),
        (-29, 1) => Some(b">"),
        (-30, 2) => Some(b"<="),
        (-31, 2) => Some(b">="),
        (-32, 2) => Some(b".."),
        (-33, 2) => Some(b"&&"),
        (-34, 2) => Some(b"||"),
        (-35, 1) => Some(b"^"),
        (-36, 13) => Some(b"__target_type"),
        _ => None,
    }
}

fn parser_wasm_bytes() -> Result<&'static [u8], Diagnostic> {
    let result = PARSER_WASM_BYTES.get_or_init(|| {
        crate::silk_parser::parse_block("0")?;
        fs::read("binaries/parser.wasm")
            .map_err(|err| Diagnostic::new(format!("Failed to read binaries/parser.wasm: {err}")))
    });

    match result {
        Ok(bytes) => Ok(bytes.as_slice()),
        Err(err) => Err(err.clone()),
    }
}

fn interpreter_wasm_bytes() -> Result<&'static [u8], Diagnostic> {
    let result = INTERPRETER_WASM_BYTES.get_or_init(|| {
        let path = "binaries/interpreter.wasm";
        if !interpreter_wasm_needs_rebuild(path)
            && let Ok(bytes) = fs::read(path)
        {
            return Ok(bytes);
        }

        let bytes = compile_silk_interpreter_wasm()?;
        let _ = fs::write(path, &bytes);
        write_interpreter_hash();
        Ok(bytes)
    });

    match result {
        Ok(bytes) => Ok(bytes.as_slice()),
        Err(err) => Err(err.clone()),
    }
}

fn wasm_engine() -> Result<&'static Engine, Diagnostic> {
    let result = WASM_ENGINE.get_or_init(|| {
        let mut config = Config::new();
        config.wasm_reference_types(true);
        config.wasm_gc(true);
        Engine::new(&config)
            .map_err(|err| Diagnostic::new(format!("Failed to create wasm engine: {err}")))
    });

    match result {
        Ok(engine) => Ok(engine),
        Err(err) => Err(err.clone()),
    }
}

fn parser_module() -> Result<&'static Module, Diagnostic> {
    let result = PARSER_MODULE.get_or_init(|| {
        let engine = wasm_engine()?;
        let bytes = parser_wasm_bytes()?;
        Module::new(engine, bytes)
            .map_err(|err| Diagnostic::new(format!("Failed to compile parser.wasm: {err}")))
    });

    match result {
        Ok(module) => Ok(module),
        Err(err) => Err(err.clone()),
    }
}

fn interpreter_module() -> Result<&'static Module, Diagnostic> {
    let result = INTERPRETER_MODULE.get_or_init(|| {
        let engine = wasm_engine()?;
        let bytes = interpreter_wasm_bytes()?;

        if let Some(module) = load_cached_interpreter_module(engine) {
            return Ok(module);
        }

        let module = Module::new(engine, bytes)
            .map_err(|err| Diagnostic::new(format!("Failed to compile interpreter.wasm: {err}")))?;
        save_cached_interpreter_module(&module);
        Ok(module)
    });

    match result {
        Ok(module) => Ok(module),
        Err(err) => Err(err.clone()),
    }
}

fn load_cached_interpreter_module(engine: &Engine) -> Option<Module> {
    let expected = interpreter_source_hash()?;
    let stored = fs::read_to_string(INTERPRETER_MODULE_HASH_PATH).ok()?;
    if stored.trim() != expected.to_string() {
        return None;
    }
    let bytes = fs::read(INTERPRETER_MODULE_CACHE_PATH).ok()?;
    // SAFETY: Module bytes are created by `Module::serialize` with the same engine settings.
    unsafe { Module::deserialize(engine, &bytes).ok() }
}

fn save_cached_interpreter_module(module: &Module) {
    let Ok(bytes) = module.serialize() else {
        return;
    };

    if let Some(parent) = Path::new(INTERPRETER_MODULE_CACHE_PATH).parent() {
        let _ = fs::create_dir_all(parent);
    }

    let tmp_path = format!("{INTERPRETER_MODULE_CACHE_PATH}.tmp");
    if fs::write(&tmp_path, &bytes).is_ok() {
        let _ = fs::rename(&tmp_path, INTERPRETER_MODULE_CACHE_PATH);
    }

    if let Some(hash) = interpreter_source_hash() {
        if let Some(parent) = Path::new(INTERPRETER_MODULE_HASH_PATH).parent() {
            let _ = fs::create_dir_all(parent);
        }
        let _ = fs::write(INTERPRETER_MODULE_HASH_PATH, hash.to_string());
    }
}

fn interpreter_wasm_needs_rebuild(path: &str) -> bool {
    if fs::metadata(path).is_err() {
        return true;
    }

    let Some(expected) = interpreter_source_hash() else {
        return true;
    };

    let Ok(stored) = fs::read_to_string(INTERPRETER_WASM_HASH_PATH) else {
        return true;
    };

    stored.trim() != expected.to_string()
}

fn interpreter_source_hash() -> Option<u64> {
    let interpreter_bytes = fs::read("silk_src/interpreter.silk").ok()?;
    let types_bytes = fs::read("silk_src/types.silk").ok()?;

    let mut hash: u64 = 14695981039346656037;
    hash = fnv1a64(hash, &interpreter_bytes);
    hash = fnv1a64(hash, &[0]);
    hash = fnv1a64(hash, &types_bytes);
    Some(hash)
}

fn fnv1a64(mut hash: u64, bytes: &[u8]) -> u64 {
    for byte in bytes {
        hash ^= *byte as u64;
        hash = hash.wrapping_mul(1099511628211);
    }
    hash
}

fn write_interpreter_hash() {
    let Some(hash) = interpreter_source_hash() else {
        return;
    };

    if let Some(parent) = Path::new(INTERPRETER_WASM_HASH_PATH).parent() {
        let _ = fs::create_dir_all(parent);
    }
    let _ = fs::write(INTERPRETER_WASM_HASH_PATH, hash.to_string());
}

fn compile_silk_interpreter_wasm() -> Result<Vec<u8>, Diagnostic> {
    let path = "silk_src/interpreter.silk";
    let source = fs::read_to_string(path)
        .map_err(|err| Diagnostic::new(format!("Failed to read {path}: {err}")))?;

    let (ast, remaining) = crate::parsing::parse_block(&source)?;
    ensure_no_trailing_input(
        &source,
        remaining,
        "Unexpected trailing input while compiling the interpreter source",
    )?;

    let types_path = "silk_src/types.silk";
    let types_source = fs::read_to_string(types_path)
        .map_err(|err| Diagnostic::new(format!("Failed to read {types_path}: {err}")))?;
    let (types_ast, types_remaining) = crate::parsing::parse_block(&types_source)?;
    ensure_no_trailing_input(
        &types_source,
        types_remaining,
        "Unexpected trailing input while compiling the interpreter types source",
    )?;

    let mut file_map = HashMap::new();
    file_map.insert(loader::normalize_path(path), ast.clone());
    file_map.insert(loader::normalize_path("types.silk"), types_ast);

    let mut context = interpret::intrinsic_context_with_files_bootstrap(file_map);
    let program_context = interpret::interpret_program_for_context(ast, &mut context)?;
    let (intermediate, _) = crate::silk_intermediate::lower_context(&program_context, &source)?;
    wasm::compile_exports(&intermediate)
}

fn ensure_no_trailing_input(
    source: &str,
    remaining: &str,
    message: &str,
) -> Result<(), Diagnostic> {
    let leftover = remaining.trim_start();
    if leftover.is_empty() {
        return Ok(());
    }

    let token_len = leftover
        .chars()
        .take_while(|ch| !ch.is_whitespace())
        .map(|ch| ch.len_utf8())
        .sum::<usize>()
        .max(1);
    let start = source.len().saturating_sub(leftover.len());
    let span = SourceSpan::new(start, token_len);

    Err(Diagnostic::new(message).with_span(span))
}

fn parse_error(source: &str, pos: i32) -> Diagnostic {
    let start = if pos < 0 { 0 } else { pos as usize };
    let span = SourceSpan::new(start.min(source.len()), 1);
    Diagnostic::new("Parse error").with_span(span)
}

fn interpreter_error(source: &str, pos: i32, code: i32) -> Diagnostic {
    let start = if pos < 0 { 0 } else { pos as usize };
    let span = SourceSpan::new(start.min(source.len()), 1);
    let message = match code {
        INTERP_ERR_ARRAY_INDEX_OUT_OF_RANGE => "Array index out of range".to_string(),
        INTERP_ERR_WRAP_REQUIRES_SINGLE_EXPORT_TARGET => {
            "wrap annotation requires exactly one export target".to_string()
        }
        INTERP_ERR_WRAP_GLOBAL_ONLY_WASM_TO_JS => {
            "wrap annotation only supports globals for wasm exports wrapped to js".to_string()
        }
        INTERP_ERR_UNBOUND_IDENTIFIER => {
            let ident = extract_identifier(source, start);
            if ident.is_empty() {
                "Unbound identifier".to_string()
            } else {
                format!("Unbound identifier: {ident}")
            }
        }
        INTERP_ERR_IF_BRANCH_TYPE_MISMATCH => "Type mismatch between if branches".to_string(),
        INTERP_ERR_MATCH_NO_BRANCH => "No match branches matched".to_string(),
        INTERP_ERR_MISSING_FIELD => {
            let ident = extract_identifier(source, start);
            if ident.is_empty() {
                "Missing field".to_string()
            } else {
                format!("Missing field {ident}")
            }
        }
        INTERP_ERR_TRAIT_MISSING_FIELD => {
            let ident = extract_identifier(source, start);
            if ident.is_empty() {
                "Type does not implement trait: missing field".to_string()
            } else {
                format!("Type does not implement trait: missing field {ident}")
            }
        }
        _ => "Interpreter error".to_string(),
    };
    Diagnostic::new(message).with_span(span)
}

fn extract_identifier(source: &str, start: usize) -> String {
    if start >= source.len() {
        return String::new();
    }

    source[start..]
        .chars()
        .take_while(|ch| ch.is_ascii_alphanumeric() || *ch == '_')
        .collect()
}

fn to_i32(value: usize, err: &str) -> Result<i32, Diagnostic> {
    i32::try_from(value).map_err(|_| Diagnostic::new(err))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wasm_interpreter_evaluates_scalar_expression() {
        let value = evaluate_text("1 + 2 * 3")
            .expect("wasm evaluation should succeed")
            .expect("scalar result should decode");

        assert!(matches!(
            value.kind,
            ExpressionKind::Literal(ExpressionLiteral::Number(7))
        ));
    }

    #[test]
    fn wasm_interpreter_decodes_function_results() {
        let value = evaluate_text("(x: i32) => x").expect("wasm evaluation should run");
        let value = value.expect("function result should decode");
        assert!(matches!(value.kind, ExpressionKind::Function { .. }));
    }

    #[test]
    fn wasm_interpreter_evaluates_boolean_operations() {
        let value = evaluate_text("true && false")
            .expect("wasm evaluation should succeed")
            .expect("boolean result should decode");
        assert!(matches!(
            value.kind,
            ExpressionKind::Literal(ExpressionLiteral::Boolean(false))
        ));

        let value = evaluate_text("true == false")
            .expect("wasm evaluation should succeed")
            .expect("boolean equality result should decode");
        assert!(matches!(
            value.kind,
            ExpressionKind::Literal(ExpressionLiteral::Boolean(false))
        ));
    }

    #[test]
    fn wasm_interpreter_keeps_range_operator_behavior() {
        let value = evaluate_text("range: Range := 0..3; range.end")
            .expect("wasm evaluation should succeed")
            .expect("range result should decode");
        assert!(matches!(
            value.kind,
            ExpressionKind::Literal(ExpressionLiteral::Number(3))
        ));
    }
}
