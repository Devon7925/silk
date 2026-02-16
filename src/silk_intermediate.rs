use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::sync::OnceLock;

use wasmtime::{Config, Engine, Instance, Memory, Module, Store};

use crate::diagnostics::{Diagnostic, SourceSpan};
use crate::intermediate::{
    self, IntermediateExport, IntermediateExportType, IntermediateFunction, IntermediateGlobal,
    IntermediateIntrinsicOperation, IntermediateKind, IntermediateResult, IntermediateType,
    IntermediateWrap,
};
use crate::interpret::{self, Context};
use crate::loader;
use crate::parsing::{
    BinaryIntrinsicOperator, DivergeExpressionType, ExpressionLiteral, Identifier, TargetLiteral,
    UnaryIntrinsicOperator,
};
use crate::wasm;

const INTERMEDIATE_SOURCE_PATH: &str = "silk_src/intermediate.silk";
const TYPES_SOURCE_PATH: &str = "silk_src/types.silk";
const PARSER_WASM_PATH: &str = "binaries/parser.wasm";
const INTERPRETER_WASM_PATH: &str = "binaries/interpreter.wasm";
const INTERMEDIATE_WASM_PATH: &str = "binaries/intermediate.wasm";

const INTERMEDIATE_MODULE_CACHE_PATH: &str = "target/wasm_cache/intermediate.wasmtime";
const INTERMEDIATE_MODULE_HASH_PATH: &str = "target/wasm_cache/intermediate.wasmtime.hash";
const INTERMEDIATE_WASM_HASH_PATH: &str = "target/wasm_cache/intermediate.wasm.hash";

const LOWER_STATUS_OK: i32 = 0;
const LOWER_STATUS_UNIMPLEMENTED: i32 = 1;
const LOWER_STATUS_ERROR: i32 = 2;

const OUTPUT_TARGET_JS: i32 = 0;
const OUTPUT_TARGET_WASM: i32 = 1;
const OUTPUT_TARGET_WGSL: i32 = 2;

const OUTPUT_EXPORT_TYPE_FUNCTION: i32 = 0;
const OUTPUT_EXPORT_TYPE_GLOBAL: i32 = 1;

const OUTPUT_GLOBAL_TYPE_INFER: i32 = -1;
const OUTPUT_GLOBAL_TYPE_I32: i32 = 0;
const OUTPUT_GLOBAL_TYPE_U8: i32 = 1;

const OUTPUT_VALUE_TAG_NUMBER: i32 = 1;
const OUTPUT_VALUE_TAG_BOOLEAN: i32 = 2;
const OUTPUT_VALUE_TAG_CHAR: i32 = 3;

const OUTPUT_VALUE_KIND_NUMBER: i32 = 1;
const OUTPUT_VALUE_KIND_BOOLEAN: i32 = 2;
const OUTPUT_VALUE_KIND_CHAR: i32 = 3;
const OUTPUT_VALUE_KIND_STRUCT: i32 = 4;
const OUTPUT_VALUE_KIND_ARRAY: i32 = 5;
const OUTPUT_VALUE_KIND_INTRINSIC_BINARY: i32 = 6;
const OUTPUT_VALUE_KIND_INTRINSIC_UNARY: i32 = 7;
const OUTPUT_VALUE_KIND_IF: i32 = 8;
const OUTPUT_VALUE_KIND_IDENTIFIER: i32 = 9;
const OUTPUT_VALUE_KIND_TYPE_PROPERTY_ACCESS: i32 = 10;
const OUTPUT_VALUE_KIND_ARRAY_INDEX: i32 = 11;
const OUTPUT_VALUE_KIND_BLOCK: i32 = 12;
const OUTPUT_VALUE_KIND_DIVERGE: i32 = 13;
const OUTPUT_VALUE_KIND_LOOP: i32 = 14;

const OUTPUT_VALUE_TYPE_UNKNOWN: i32 = 0;
const OUTPUT_VALUE_TYPE_I32: i32 = 1;
const OUTPUT_VALUE_TYPE_U8: i32 = 2;

static INTERMEDIATE_WASM_BYTES: OnceLock<Result<Vec<u8>, Diagnostic>> = OnceLock::new();
static WASM_ENGINE: OnceLock<Result<Engine, Diagnostic>> = OnceLock::new();
static PARSER_MODULE: OnceLock<Result<Module, Diagnostic>> = OnceLock::new();
static INTERPRETER_MODULE: OnceLock<Result<Module, Diagnostic>> = OnceLock::new();
static INTERMEDIATE_MODULE: OnceLock<Result<Module, Diagnostic>> = OnceLock::new();
static STAGE_READY: OnceLock<Result<bool, Diagnostic>> = OnceLock::new();

#[derive(Debug, Clone)]
struct DecodedOutputValueSlot {
    kind_tag: i32,
    value_i32: i32,
    name_start: i32,
    name_length: i32,
    item_start: i32,
    item_count: i32,
}

#[derive(Debug, Clone)]
struct DecodedOutputValueFieldSlot {
    name_start: i32,
    name_length: i32,
    value_ref: i32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum IntermediateLoweringBackend {
    SilkWasm,
    RustFallback,
}

impl IntermediateLoweringBackend {
    pub(crate) fn as_str(self) -> &'static str {
        match self {
            IntermediateLoweringBackend::SilkWasm => "silk_wasm",
            IntermediateLoweringBackend::RustFallback => "rust_fallback",
        }
    }
}

pub(crate) fn lower_context(
    context: &Context,
    source: &str,
) -> Result<(IntermediateResult, IntermediateLoweringBackend), Diagnostic> {
    if std::env::var_os("SILK_DISABLE_WASM_INTERMEDIATE").is_some() {
        return Ok((
            intermediate::context_to_intermediate(context),
            IntermediateLoweringBackend::RustFallback,
        ));
    }

    match lower_with_wasm_with_context(source, Some(context)) {
        Ok(Some(intermediate)) => Ok((intermediate, IntermediateLoweringBackend::SilkWasm)),
        Ok(None) => {
            if wasm_intermediate_strict_mode() {
                return Err(Diagnostic::new(
                    "Silk intermediate wasm backend is not available or is still unimplemented",
                ));
            }
            Ok((
                intermediate::context_to_intermediate(context),
                IntermediateLoweringBackend::RustFallback,
            ))
        }
        Err(err) => {
            if wasm_intermediate_strict_mode() {
                return Err(err);
            }
            Ok((
                intermediate::context_to_intermediate(context),
                IntermediateLoweringBackend::RustFallback,
            ))
        }
    }
}

fn wasm_intermediate_strict_mode() -> bool {
    std::env::var_os("SILK_WASM_INTERMEDIATE_STRICT").is_some()
}

fn lower_with_wasm(source: &str) -> Result<Option<IntermediateResult>, Diagnostic> {
    lower_with_wasm_with_context(source, None)
}

fn lower_with_wasm_with_context(
    source: &str,
    context: Option<&Context>,
) -> Result<Option<IntermediateResult>, Diagnostic> {
    if fs::metadata(INTERMEDIATE_SOURCE_PATH).is_err() {
        return Ok(None);
    }

    if !stage_ready()? {
        return Ok(None);
    }

    let engine = wasm_engine()?;
    let parser_module = parser_module()?;
    let module = intermediate_module()?;
    let mut store = Store::new(engine, ());
    let parser_instance = Instance::new(&mut store, parser_module, &[])
        .map_err(|err| Diagnostic::new(format!("Failed to instantiate parser wasm: {err}")))?;
    let instance = Instance::new(&mut store, module, &[]).map_err(|err| {
        Diagnostic::new(format!("Failed to instantiate intermediate wasm: {err}"))
    })?;

    write_input_payload(&mut store, &parser_instance, source.as_bytes())?;

    let parse_func = parser_instance
        .get_typed_func::<(), i32>(&mut store, "parse")
        .map_err(|err| Diagnostic::new(format!("Missing parser export `parse`: {err}")))?;
    let root = parse_func
        .call(&mut store, ())
        .map_err(|err| Diagnostic::new(format!("Parser call `parse` failed: {err}")))?;
    let parse_error = parser_instance
        .get_typed_func::<(), i32>(&mut store, "get_state_error")
        .map_err(|err| Diagnostic::new(format!("Missing parser export `get_state_error`: {err}")))?
        .call(&mut store, ())
        .map_err(|err| Diagnostic::new(format!("Parser call `get_state_error` failed: {err}")))?;
    if parse_error != -1 {
        let start = parse_error.max(0) as usize;
        return Err(
            Diagnostic::new("Parse error while preparing intermediate lowering")
                .with_span(SourceSpan::new(start.min(source.len()), 1)),
        );
    }

    copy_named_memory(&mut store, &parser_instance, &instance, "input")?;
    copy_named_memory(&mut store, &parser_instance, &instance, "nodes")?;
    copy_named_memory(&mut store, &parser_instance, &instance, "list_nodes")?;
    copy_named_memory(&mut store, &parser_instance, &instance, "state")?;

    let lower_func = instance
        .get_typed_func::<i32, i32>(&mut store, "lower_context")
        .map_err(|err| {
            Diagnostic::new(format!(
                "Missing intermediate export `lower_context`: {err}"
            ))
        })?;
    let status = lower_func.call(&mut store, root).map_err(|err| {
        Diagnostic::new(format!("Intermediate call `lower_context` failed: {err}"))
    })?;

    match status {
        LOWER_STATUS_OK => {
            let lowered = decode_lowered_output(&mut store, &instance, source.as_bytes(), context)?;
            Ok(Some(lowered))
        }
        LOWER_STATUS_UNIMPLEMENTED => Ok(None),
        LOWER_STATUS_ERROR => {
            let error_code = instance
                .get_typed_func::<(), i32>(&mut store, "get_lower_error_code")
                .ok()
                .and_then(|func| func.call(&mut store, ()).ok())
                .unwrap_or(0);
            Err(Diagnostic::new(format!(
                "Silk intermediate wasm reported an error: {} (code {error_code})",
                lower_error_code_message(error_code)
            )))
        }
        other => Err(Diagnostic::new(format!(
            "Silk intermediate wasm returned unknown lower status {other}"
        ))),
    }
}

fn stage_ready() -> Result<bool, Diagnostic> {
    let result = STAGE_READY.get_or_init(probe_stage_ready);
    match result {
        Ok(ready) => Ok(*ready),
        Err(err) => Err(err.clone()),
    }
}

fn probe_stage_ready() -> Result<bool, Diagnostic> {
    let engine = wasm_engine()?;
    let parser = parser_module()?;
    let interpreter = interpreter_module()?;
    let module = intermediate_module()?;
    let mut store = Store::new(engine, ());
    let _parser_instance = Instance::new(&mut store, parser, &[])
        .map_err(|err| Diagnostic::new(format!("Failed to instantiate parser wasm: {err}")))?;
    let interpreter_instance = Instance::new(&mut store, interpreter, &[])
        .map_err(|err| Diagnostic::new(format!("Failed to instantiate interpreter wasm: {err}")))?;
    let instance = Instance::new(&mut store, module, &[]).map_err(|err| {
        Diagnostic::new(format!("Failed to instantiate intermediate wasm: {err}"))
    })?;

    let version_func = instance
        .get_typed_func::<(), i32>(&mut store, "intermediate_stage_version")
        .map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate wasm is missing `intermediate_stage_version`: {err}"
            ))
        })?;
    let version = version_func.call(&mut store, ()).map_err(|err| {
        Diagnostic::new(format!(
            "Failed calling `intermediate_stage_version`: {err}"
        ))
    })?;
    if version <= 0 {
        return Err(Diagnostic::new(format!(
            "Intermediate wasm reported invalid stage version {version}"
        )));
    }

    Ok(instance
        .get_typed_func::<i32, i32>(&mut store, "lower_context")
        .is_ok()
        && interpreter_instance
            .get_typed_func::<i32, i32>(&mut store, "interpret")
            .is_ok()
        && interpreter_instance
            .get_typed_func::<(), i32>(&mut store, "get_interp_error")
            .is_ok())
}

fn intermediate_wasm_bytes() -> Result<&'static [u8], Diagnostic> {
    let result = INTERMEDIATE_WASM_BYTES.get_or_init(|| {
        if !intermediate_wasm_needs_rebuild(INTERMEDIATE_WASM_PATH)
            && let Ok(bytes) = fs::read(INTERMEDIATE_WASM_PATH)
        {
            return Ok(bytes);
        }

        let bytes = compile_silk_intermediate_wasm()?;
        let _ = fs::write(INTERMEDIATE_WASM_PATH, &bytes);
        write_intermediate_hash();
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
        let bytes = fs::read(PARSER_WASM_PATH)
            .map_err(|err| Diagnostic::new(format!("Failed to read {PARSER_WASM_PATH}: {err}")))?;
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
        let bytes = fs::read(INTERPRETER_WASM_PATH).map_err(|err| {
            Diagnostic::new(format!("Failed to read {INTERPRETER_WASM_PATH}: {err}"))
        })?;
        Module::new(engine, bytes)
            .map_err(|err| Diagnostic::new(format!("Failed to compile interpreter.wasm: {err}")))
    });

    match result {
        Ok(module) => Ok(module),
        Err(err) => Err(err.clone()),
    }
}

fn intermediate_module() -> Result<&'static Module, Diagnostic> {
    let result = INTERMEDIATE_MODULE.get_or_init(|| {
        let engine = wasm_engine()?;
        let bytes = intermediate_wasm_bytes()?;

        if let Some(module) = load_cached_intermediate_module(engine) {
            return Ok(module);
        }

        let module = Module::new(engine, bytes).map_err(|err| {
            Diagnostic::new(format!(
                "Failed to compile intermediate.wasm: {err} ({err:?})"
            ))
        })?;
        save_cached_intermediate_module(&module);
        Ok(module)
    });

    match result {
        Ok(module) => Ok(module),
        Err(err) => Err(err.clone()),
    }
}

fn load_cached_intermediate_module(engine: &Engine) -> Option<Module> {
    let expected = intermediate_source_hash()?;
    let stored = fs::read_to_string(INTERMEDIATE_MODULE_HASH_PATH).ok()?;
    if stored.trim() != expected.to_string() {
        return None;
    }
    let bytes = fs::read(INTERMEDIATE_MODULE_CACHE_PATH).ok()?;
    // SAFETY: Module bytes are created by `Module::serialize` with the same engine settings.
    unsafe { Module::deserialize(engine, &bytes).ok() }
}

fn save_cached_intermediate_module(module: &Module) {
    let Ok(bytes) = module.serialize() else {
        return;
    };

    if let Some(parent) = Path::new(INTERMEDIATE_MODULE_CACHE_PATH).parent() {
        let _ = fs::create_dir_all(parent);
    }

    let tmp_path = format!("{INTERMEDIATE_MODULE_CACHE_PATH}.tmp");
    if fs::write(&tmp_path, &bytes).is_ok() {
        let _ = fs::rename(&tmp_path, INTERMEDIATE_MODULE_CACHE_PATH);
    }

    if let Some(hash) = intermediate_source_hash() {
        if let Some(parent) = Path::new(INTERMEDIATE_MODULE_HASH_PATH).parent() {
            let _ = fs::create_dir_all(parent);
        }
        let _ = fs::write(INTERMEDIATE_MODULE_HASH_PATH, hash.to_string());
    }
}

fn intermediate_wasm_needs_rebuild(path: &str) -> bool {
    if fs::metadata(path).is_err() {
        return true;
    }

    let Some(expected) = intermediate_source_hash() else {
        return true;
    };

    let Ok(stored) = fs::read_to_string(INTERMEDIATE_WASM_HASH_PATH) else {
        return true;
    };

    stored.trim() != expected.to_string()
}

fn intermediate_source_hash() -> Option<u64> {
    let intermediate_bytes = fs::read(INTERMEDIATE_SOURCE_PATH).ok()?;
    let types_bytes = fs::read(TYPES_SOURCE_PATH).ok()?;

    let mut hash: u64 = 14695981039346656037;
    hash = fnv1a64(hash, &intermediate_bytes);
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

fn write_intermediate_hash() {
    let Some(hash) = intermediate_source_hash() else {
        return;
    };

    if let Some(parent) = Path::new(INTERMEDIATE_WASM_HASH_PATH).parent() {
        let _ = fs::create_dir_all(parent);
    }
    let _ = fs::write(INTERMEDIATE_WASM_HASH_PATH, hash.to_string());
}

fn compile_silk_intermediate_wasm() -> Result<Vec<u8>, Diagnostic> {
    let source = fs::read_to_string(INTERMEDIATE_SOURCE_PATH).map_err(|err| {
        Diagnostic::new(format!("Failed to read {INTERMEDIATE_SOURCE_PATH}: {err}"))
    })?;

    let (ast, remaining) = crate::parsing::parse_block(&source)?;
    ensure_no_trailing_input(
        &source,
        remaining,
        "Unexpected trailing input while compiling the intermediate source",
    )?;

    let types_source = fs::read_to_string(TYPES_SOURCE_PATH)
        .map_err(|err| Diagnostic::new(format!("Failed to read {TYPES_SOURCE_PATH}: {err}")))?;
    let (types_ast, types_remaining) = crate::parsing::parse_block(&types_source)?;
    ensure_no_trailing_input(
        &types_source,
        types_remaining,
        "Unexpected trailing input while compiling the intermediate types source",
    )?;

    let mut file_map = HashMap::new();
    file_map.insert(
        loader::normalize_path(INTERMEDIATE_SOURCE_PATH),
        ast.clone(),
    );
    file_map.insert(loader::normalize_path("types.silk"), types_ast);

    let mut context = interpret::intrinsic_context_with_files_bootstrap(file_map);
    let program_context = interpret::interpret_program_for_context(ast, &mut context)?;
    // Bootstrap note: this stage still uses Rust lowering to compile the silk intermediate module.
    let intermediate = intermediate::context_to_intermediate(&program_context);
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

fn write_input_payload(
    store: &mut Store<()>,
    instance: &Instance,
    payload: &[u8],
) -> Result<(), Diagnostic> {
    let input = instance
        .get_memory(&mut *store, "input")
        .ok_or_else(|| Diagnostic::new("Intermediate export `input` memory not found"))?;
    write_memory_payload(store, input, payload)
}

fn write_memory_payload(
    store: &mut Store<()>,
    memory: Memory,
    payload: &[u8],
) -> Result<(), Diagnostic> {
    let data = memory.data_mut(store);
    if payload.len() + 1 > data.len() {
        return Err(Diagnostic::new(format!(
            "Context payload exceeds intermediate input buffer (max {} bytes)",
            data.len().saturating_sub(1)
        )));
    }
    data.fill(0);
    data[..payload.len()].copy_from_slice(payload);
    data[payload.len()] = 0;
    Ok(())
}

fn copy_named_memory(
    store: &mut Store<()>,
    src_instance: &Instance,
    dst_instance: &Instance,
    memory_name: &str,
) -> Result<(), Diagnostic> {
    let src = src_instance
        .get_memory(&mut *store, memory_name)
        .ok_or_else(|| Diagnostic::new(format!("Missing source memory `{memory_name}`")))?;
    let dst = dst_instance
        .get_memory(&mut *store, memory_name)
        .ok_or_else(|| Diagnostic::new(format!("Missing destination memory `{memory_name}`")))?;

    let src_data = src.data(&*store).to_vec();
    let dst_data = dst.data_mut(&mut *store);
    if dst_data.len() < src_data.len() {
        return Err(Diagnostic::new(format!(
            "Destination memory `{memory_name}` is smaller than source memory"
        )));
    }

    dst_data.fill(0);
    dst_data[..src_data.len()].copy_from_slice(&src_data);
    Ok(())
}

fn function_lookup_from_context(context: &Context) -> HashMap<String, IntermediateFunction> {
    let lowered = intermediate::context_to_intermediate(context);
    let mut lookup = HashMap::new();

    for export in &lowered.exports {
        if !matches!(export.export_type, IntermediateExportType::Function) {
            continue;
        }
        if let Some(function) = lowered.functions.get(export.index) {
            lookup
                .entry(export.name.clone())
                .or_insert_with(|| function.clone());
        }
    }

    for wrap in &lowered.wrappers {
        if !matches!(wrap.export_type, IntermediateExportType::Function) {
            continue;
        }
        if let Some(function) = lowered.functions.get(wrap.index) {
            lookup
                .entry(wrap.name.clone())
                .or_insert_with(|| function.clone());
        }
    }

    lookup
}

fn decode_lowered_output(
    store: &mut Store<()>,
    instance: &Instance,
    input_bytes: &[u8],
    context: Option<&Context>,
) -> Result<IntermediateResult, Diagnostic> {
    let global_count = call_required_i32_export(instance, store, "get_lower_output_global_count")?;
    let function_count =
        call_required_i32_export(instance, store, "get_lower_output_function_count")?;
    let export_count = call_required_i32_export(instance, store, "get_lower_output_export_count")?;
    let wrapper_count =
        call_required_i32_export(instance, store, "get_lower_output_wrapper_count")?;
    let inline_binding_count =
        call_required_i32_export(instance, store, "get_lower_output_inline_binding_count")?;
    let value_count = call_required_i32_export(instance, store, "get_lower_output_value_count")?;
    let value_field_count =
        call_required_i32_export(instance, store, "get_lower_output_value_field_count")?;

    if global_count < 0
        || function_count < 0
        || export_count < 0
        || wrapper_count < 0
        || inline_binding_count < 0
        || value_count < 0
        || value_field_count < 0
    {
        return Err(Diagnostic::new(format!(
            "Silk intermediate returned negative output counts (globals={global_count}, functions={function_count}, exports={export_count}, wrappers={wrapper_count}, inline_bindings={inline_binding_count}, values={value_count}, value_fields={value_field_count})"
        )));
    }

    if global_count == 0
        && function_count == 0
        && export_count == 0
        && wrapper_count == 0
        && inline_binding_count == 0
    {
        return Ok(IntermediateResult {
            functions: Vec::new(),
            globals: Vec::new(),
            exports: Vec::new(),
            wrappers: Vec::new(),
            inline_bindings: HashMap::new(),
        });
    }

    let get_global_name_start =
        required_i32_to_i32_export(instance, store, "get_lower_output_global_name_start")?;
    let get_global_name_length =
        required_i32_to_i32_export(instance, store, "get_lower_output_global_name_length")?;
    let get_global_type_tag =
        required_i32_to_i32_export(instance, store, "get_lower_output_global_type_tag")?;
    let get_global_value_tag =
        required_i32_to_i32_export(instance, store, "get_lower_output_global_value_tag")?;
    let get_global_value_i32 =
        required_i32_to_i32_export(instance, store, "get_lower_output_global_value_i32")?;
    let get_global_value_ref =
        required_i32_to_i32_export(instance, store, "get_lower_output_global_value_ref")?;

    let get_value_kind =
        required_i32_to_i32_export(instance, store, "get_lower_output_value_kind")?;
    let get_value_i32 = required_i32_to_i32_export(instance, store, "get_lower_output_value_i32")?;
    let get_value_name_start =
        required_i32_to_i32_export(instance, store, "get_lower_output_value_name_start")?;
    let get_value_name_length =
        required_i32_to_i32_export(instance, store, "get_lower_output_value_name_length")?;
    let get_value_item_start =
        required_i32_to_i32_export(instance, store, "get_lower_output_value_item_start")?;
    let get_value_item_count =
        required_i32_to_i32_export(instance, store, "get_lower_output_value_item_count")?;
    let get_value_field_name_start =
        required_i32_to_i32_export(instance, store, "get_lower_output_value_field_name_start")?;
    let get_value_field_name_length =
        required_i32_to_i32_export(instance, store, "get_lower_output_value_field_name_length")?;
    let get_value_field_value_ref =
        required_i32_to_i32_export(instance, store, "get_lower_output_value_field_value_ref")?;

    let mut decoded_value_slots = Vec::with_capacity(value_count as usize);
    for idx in 0..value_count {
        let kind_tag = get_value_kind.call(&mut *store, idx).map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `get_lower_output_value_kind` failed: {err}"
            ))
        })?;
        let value_i32 = get_value_i32.call(&mut *store, idx).map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `get_lower_output_value_i32` failed: {err}"
            ))
        })?;
        let name_start = get_value_name_start.call(&mut *store, idx).map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `get_lower_output_value_name_start` failed: {err}"
            ))
        })?;
        let name_length = get_value_name_length
            .call(&mut *store, idx)
            .map_err(|err| {
                Diagnostic::new(format!(
                    "Intermediate call `get_lower_output_value_name_length` failed: {err}"
                ))
            })?;
        let item_start = get_value_item_start.call(&mut *store, idx).map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `get_lower_output_value_item_start` failed: {err}"
            ))
        })?;
        let item_count = get_value_item_count.call(&mut *store, idx).map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `get_lower_output_value_item_count` failed: {err}"
            ))
        })?;
        decoded_value_slots.push(DecodedOutputValueSlot {
            kind_tag,
            value_i32,
            name_start,
            name_length,
            item_start,
            item_count,
        });
    }

    let mut decoded_value_field_slots = Vec::with_capacity(value_field_count as usize);
    for idx in 0..value_field_count {
        let name_start = get_value_field_name_start
            .call(&mut *store, idx)
            .map_err(|err| {
                Diagnostic::new(format!(
                    "Intermediate call `get_lower_output_value_field_name_start` failed: {err}"
                ))
            })?;
        let name_length = get_value_field_name_length
            .call(&mut *store, idx)
            .map_err(|err| {
                Diagnostic::new(format!(
                    "Intermediate call `get_lower_output_value_field_name_length` failed: {err}"
                ))
            })?;
        let value_ref = get_value_field_value_ref
            .call(&mut *store, idx)
            .map_err(|err| {
                Diagnostic::new(format!(
                    "Intermediate call `get_lower_output_value_field_value_ref` failed: {err}"
                ))
            })?;
        decoded_value_field_slots.push(DecodedOutputValueFieldSlot {
            name_start,
            name_length,
            value_ref,
        });
    }

    let mut decoded_value_cache = HashMap::new();
    let mut globals = Vec::with_capacity(global_count as usize);
    for idx in 0..global_count {
        let name_start = get_global_name_start
            .call(&mut *store, idx)
            .map_err(|err| {
                Diagnostic::new(format!(
                    "Intermediate call `get_lower_output_global_name_start` failed: {err}"
                ))
            })?;
        let name_length = get_global_name_length
            .call(&mut *store, idx)
            .map_err(|err| {
                Diagnostic::new(format!(
                    "Intermediate call `get_lower_output_global_name_length` failed: {err}"
                ))
            })?;
        let name = decode_name_from_input(input_bytes, name_start, name_length)?;
        let ty_tag = get_global_type_tag.call(&mut *store, idx).map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `get_lower_output_global_type_tag` failed: {err}"
            ))
        })?;
        let value_tag = get_global_value_tag.call(&mut *store, idx).map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `get_lower_output_global_value_tag` failed: {err}"
            ))
        })?;
        let value_i32 = get_global_value_i32.call(&mut *store, idx).map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `get_lower_output_global_value_i32` failed: {err}"
            ))
        })?;
        let value_ref = get_global_value_ref.call(&mut *store, idx).map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `get_lower_output_global_value_ref` failed: {err}"
            ))
        })?;

        let value = if value_ref >= 0 {
            decode_output_value(
                value_ref,
                &decoded_value_slots,
                &decoded_value_field_slots,
                input_bytes,
                &mut decoded_value_cache,
            )?
        } else {
            decode_output_literal(value_tag, value_i32).map(IntermediateKind::Literal)?
        };
        let ty = match decode_output_global_type(ty_tag)? {
            Some(decoded) => decoded,
            None => infer_intermediate_type_from_value(&value)?,
        };
        globals.push(IntermediateGlobal { name, ty, value });
    }

    let mut functions = Vec::with_capacity(function_count as usize);
    if function_count > 0 {
        let get_function_name_start =
            required_i32_to_i32_export(instance, store, "get_lower_output_function_name_start")?;
        let get_function_name_length =
            required_i32_to_i32_export(instance, store, "get_lower_output_function_name_length")?;
        let function_lookup = context
            .map(function_lookup_from_context)
            .ok_or_else(|| {
                Diagnostic::new(
                    "Silk intermediate output includes function rows, but lowering context was not provided",
                )
            })?;

        for idx in 0..function_count {
            let name_start = get_function_name_start
                .call(&mut *store, idx)
                .map_err(|err| {
                    Diagnostic::new(format!(
                        "Intermediate call `get_lower_output_function_name_start` failed: {err}"
                    ))
                })?;
            let name_length = get_function_name_length
                .call(&mut *store, idx)
                .map_err(|err| {
                    Diagnostic::new(format!(
                        "Intermediate call `get_lower_output_function_name_length` failed: {err}"
                    ))
                })?;
            let name = decode_name_from_input(input_bytes, name_start, name_length)?;
            let function = function_lookup.get(&name).ok_or_else(|| {
                Diagnostic::new(format!(
                    "Silk intermediate function `{name}` was not found in Rust lowering context"
                ))
            })?;
            functions.push(function.clone());
        }
    }

    let get_export_target_tag =
        required_i32_to_i32_export(instance, store, "get_lower_output_export_target_tag")?;
    let get_export_type_tag =
        required_i32_to_i32_export(instance, store, "get_lower_output_export_type_tag")?;
    let get_export_index =
        required_i32_to_i32_export(instance, store, "get_lower_output_export_index")?;
    let get_export_name_start =
        required_i32_to_i32_export(instance, store, "get_lower_output_export_name_start")?;
    let get_export_name_length =
        required_i32_to_i32_export(instance, store, "get_lower_output_export_name_length")?;

    let mut exports = Vec::with_capacity(export_count as usize);
    for idx in 0..export_count {
        let target_tag = get_export_target_tag
            .call(&mut *store, idx)
            .map_err(|err| {
                Diagnostic::new(format!(
                    "Intermediate call `get_lower_output_export_target_tag` failed: {err}"
                ))
            })?;
        let export_type_tag = get_export_type_tag.call(&mut *store, idx).map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `get_lower_output_export_type_tag` failed: {err}"
            ))
        })?;
        let item_index = get_export_index.call(&mut *store, idx).map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `get_lower_output_export_index` failed: {err}"
            ))
        })? as usize;
        let name_start = get_export_name_start
            .call(&mut *store, idx)
            .map_err(|err| {
                Diagnostic::new(format!(
                    "Intermediate call `get_lower_output_export_name_start` failed: {err}"
                ))
            })?;
        let name_length = get_export_name_length
            .call(&mut *store, idx)
            .map_err(|err| {
                Diagnostic::new(format!(
                    "Intermediate call `get_lower_output_export_name_length` failed: {err}"
                ))
            })?;
        let name = decode_name_from_input(input_bytes, name_start, name_length)?;

        let target = decode_output_target(target_tag)?;
        let export_type = decode_output_export_type(export_type_tag)?;
        if matches!(export_type, IntermediateExportType::Global) && item_index >= globals.len() {
            return Err(Diagnostic::new(format!(
                "Silk intermediate output export {idx} references missing global index {item_index} (global count {})",
                globals.len()
            )));
        }
        if matches!(export_type, IntermediateExportType::Function) && item_index >= functions.len()
        {
            return Err(Diagnostic::new(format!(
                "Silk intermediate output export {idx} references missing function index {item_index} (function count {})",
                functions.len()
            )));
        }
        exports.push(IntermediateExport {
            target,
            name,
            export_type,
            index: item_index,
        });
    }

    let get_wrapper_source_target_tag = required_i32_to_i32_export(
        instance,
        store,
        "get_lower_output_wrapper_source_target_tag",
    )?;
    let get_wrapper_wrap_target_tag =
        required_i32_to_i32_export(instance, store, "get_lower_output_wrapper_wrap_target_tag")?;
    let get_wrapper_export_type_tag =
        required_i32_to_i32_export(instance, store, "get_lower_output_wrapper_export_type_tag")?;
    let get_wrapper_index =
        required_i32_to_i32_export(instance, store, "get_lower_output_wrapper_index")?;
    let get_wrapper_name_start =
        required_i32_to_i32_export(instance, store, "get_lower_output_wrapper_name_start")?;
    let get_wrapper_name_length =
        required_i32_to_i32_export(instance, store, "get_lower_output_wrapper_name_length")?;

    let mut wrappers = Vec::with_capacity(wrapper_count as usize);
    for idx in 0..wrapper_count {
        let source_target_tag = get_wrapper_source_target_tag
            .call(&mut *store, idx)
            .map_err(|err| {
                Diagnostic::new(format!(
                    "Intermediate call `get_lower_output_wrapper_source_target_tag` failed: {err}"
                ))
            })?;
        let wrap_target_tag =
            get_wrapper_wrap_target_tag
                .call(&mut *store, idx)
                .map_err(|err| {
                    Diagnostic::new(format!(
                        "Intermediate call `get_lower_output_wrapper_wrap_target_tag` failed: {err}"
                    ))
                })?;
        let export_type_tag =
            get_wrapper_export_type_tag
                .call(&mut *store, idx)
                .map_err(|err| {
                    Diagnostic::new(format!(
                        "Intermediate call `get_lower_output_wrapper_export_type_tag` failed: {err}"
                    ))
                })?;
        let item_index = get_wrapper_index.call(&mut *store, idx).map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `get_lower_output_wrapper_index` failed: {err}"
            ))
        })? as usize;
        let name_start = get_wrapper_name_start
            .call(&mut *store, idx)
            .map_err(|err| {
                Diagnostic::new(format!(
                    "Intermediate call `get_lower_output_wrapper_name_start` failed: {err}"
                ))
            })?;
        let name_length = get_wrapper_name_length
            .call(&mut *store, idx)
            .map_err(|err| {
                Diagnostic::new(format!(
                    "Intermediate call `get_lower_output_wrapper_name_length` failed: {err}"
                ))
            })?;
        let name = decode_name_from_input(input_bytes, name_start, name_length)?;

        let source_target = decode_output_target(source_target_tag)?;
        let wrap_target = decode_output_target(wrap_target_tag)?;
        let export_type = decode_output_export_type(export_type_tag)?;
        if matches!(export_type, IntermediateExportType::Global) && item_index >= globals.len() {
            return Err(Diagnostic::new(format!(
                "Silk intermediate output wrapper {idx} references missing global index {item_index} (global count {})",
                globals.len()
            )));
        }
        if matches!(export_type, IntermediateExportType::Function) && item_index >= functions.len()
        {
            return Err(Diagnostic::new(format!(
                "Silk intermediate output wrapper {idx} references missing function index {item_index} (function count {})",
                functions.len()
            )));
        }
        wrappers.push(IntermediateWrap {
            source_target,
            wrap_target,
            name,
            export_type,
            index: item_index,
        });
    }

    let get_inline_binding_name_start = required_i32_to_i32_export(
        instance,
        store,
        "get_lower_output_inline_binding_name_start",
    )?;
    let get_inline_binding_name_length = required_i32_to_i32_export(
        instance,
        store,
        "get_lower_output_inline_binding_name_length",
    )?;
    let get_inline_binding_value_tag =
        required_i32_to_i32_export(instance, store, "get_lower_output_inline_binding_value_tag")?;
    let get_inline_binding_value_i32 =
        required_i32_to_i32_export(instance, store, "get_lower_output_inline_binding_value_i32")?;
    let get_inline_binding_value_ref =
        required_i32_to_i32_export(instance, store, "get_lower_output_inline_binding_value_ref")?;

    let mut inline_bindings = HashMap::with_capacity(inline_binding_count as usize);
    for idx in 0..inline_binding_count {
        let name_start = get_inline_binding_name_start
            .call(&mut *store, idx)
            .map_err(|err| {
                Diagnostic::new(format!(
                    "Intermediate call `get_lower_output_inline_binding_name_start` failed: {err}"
                ))
            })?;
        let name_length = get_inline_binding_name_length
            .call(&mut *store, idx)
            .map_err(|err| {
                Diagnostic::new(format!(
                    "Intermediate call `get_lower_output_inline_binding_name_length` failed: {err}"
                ))
            })?;
        let name = decode_name_from_input(input_bytes, name_start, name_length)?;
        let value_tag = get_inline_binding_value_tag
            .call(&mut *store, idx)
            .map_err(|err| {
                Diagnostic::new(format!(
                    "Intermediate call `get_lower_output_inline_binding_value_tag` failed: {err}"
                ))
            })?;
        let value_i32 = get_inline_binding_value_i32
            .call(&mut *store, idx)
            .map_err(|err| {
                Diagnostic::new(format!(
                    "Intermediate call `get_lower_output_inline_binding_value_i32` failed: {err}"
                ))
            })?;
        let value_ref = get_inline_binding_value_ref
            .call(&mut *store, idx)
            .map_err(|err| {
                Diagnostic::new(format!(
                    "Intermediate call `get_lower_output_inline_binding_value_ref` failed: {err}"
                ))
            })?;
        let value = if value_ref >= 0 {
            decode_output_value(
                value_ref,
                &decoded_value_slots,
                &decoded_value_field_slots,
                input_bytes,
                &mut decoded_value_cache,
            )?
        } else {
            decode_output_literal(value_tag, value_i32).map(IntermediateKind::Literal)?
        };
        inline_bindings.insert(name, value);
    }

    Ok(IntermediateResult {
        functions,
        globals,
        exports,
        wrappers,
        inline_bindings,
    })
}

fn decode_output_value(
    root: i32,
    value_slots: &[DecodedOutputValueSlot],
    value_fields: &[DecodedOutputValueFieldSlot],
    input_bytes: &[u8],
    memo: &mut HashMap<i32, IntermediateKind>,
) -> Result<IntermediateKind, Diagnostic> {
    let mut visiting = HashMap::new();
    decode_output_value_inner(
        root,
        value_slots,
        value_fields,
        input_bytes,
        memo,
        &mut visiting,
    )
}

fn decode_output_value_inner(
    root: i32,
    value_slots: &[DecodedOutputValueSlot],
    value_fields: &[DecodedOutputValueFieldSlot],
    input_bytes: &[u8],
    memo: &mut HashMap<i32, IntermediateKind>,
    visiting: &mut HashMap<i32, ()>,
) -> Result<IntermediateKind, Diagnostic> {
    if let Some(existing) = memo.get(&root) {
        return Ok(existing.clone());
    }
    if root < 0 || root as usize >= value_slots.len() {
        return Err(Diagnostic::new(format!(
            "Output value reference {root} is out of bounds (value count {})",
            value_slots.len()
        )));
    }
    if visiting.contains_key(&root) {
        return Err(Diagnostic::new(format!(
            "Cyclic output value graph detected at value {root}"
        )));
    }
    visiting.insert(root, ());

    let slot = &value_slots[root as usize];
    let decoded = match slot.kind_tag {
        OUTPUT_VALUE_KIND_NUMBER => {
            IntermediateKind::Literal(ExpressionLiteral::Number(slot.value_i32))
        }
        OUTPUT_VALUE_KIND_BOOLEAN => {
            IntermediateKind::Literal(ExpressionLiteral::Boolean(slot.value_i32 != 0))
        }
        OUTPUT_VALUE_KIND_CHAR => {
            if !(0..=u8::MAX as i32).contains(&slot.value_i32) {
                return Err(Diagnostic::new(format!(
                    "Output char value out of range for u8: {}",
                    slot.value_i32
                )));
            }
            IntermediateKind::Literal(ExpressionLiteral::Char(slot.value_i32 as u8))
        }
        OUTPUT_VALUE_KIND_IDENTIFIER => {
            let name = decode_name_from_input(input_bytes, slot.name_start, slot.name_length)?;
            IntermediateKind::Identifier(Identifier::new(name))
        }
        OUTPUT_VALUE_KIND_TYPE_PROPERTY_ACCESS => {
            let mut children = decode_output_value_children(
                slot,
                root,
                1,
                value_slots,
                value_fields,
                input_bytes,
                memo,
                visiting,
                "type property access",
            )?
            .into_iter();
            let object = children.next().expect("type property access object child");
            let property = decode_name_from_input(input_bytes, slot.name_start, slot.name_length)?;
            IntermediateKind::TypePropertyAccess {
                object: Box::new(object),
                property,
            }
        }
        OUTPUT_VALUE_KIND_ARRAY_INDEX => {
            let mut children = decode_output_value_children(
                slot,
                root,
                2,
                value_slots,
                value_fields,
                input_bytes,
                memo,
                visiting,
                "array index",
            )?
            .into_iter();
            let array = children.next().expect("array index array child");
            let index = children.next().expect("array index index child");
            IntermediateKind::ArrayIndex {
                array: Box::new(array),
                index: Box::new(index),
            }
        }
        OUTPUT_VALUE_KIND_INTRINSIC_BINARY => {
            let mut children = decode_output_value_children(
                slot,
                root,
                2,
                value_slots,
                value_fields,
                input_bytes,
                memo,
                visiting,
                "intrinsic binary",
            )?
            .into_iter();
            let left = children.next().expect("binary intrinsic left child");
            let right = children.next().expect("binary intrinsic right child");
            let operator = decode_output_binary_intrinsic_operator(slot.value_i32)?;
            IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Binary(
                Box::new(left),
                Box::new(right),
                operator,
            ))
        }
        OUTPUT_VALUE_KIND_INTRINSIC_UNARY => {
            let mut children = decode_output_value_children(
                slot,
                root,
                1,
                value_slots,
                value_fields,
                input_bytes,
                memo,
                visiting,
                "intrinsic unary",
            )?
            .into_iter();
            let operand = children.next().expect("unary intrinsic operand");
            let operator = decode_output_unary_intrinsic_operator(slot.value_i32)?;
            IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Unary(
                Box::new(operand),
                operator,
            ))
        }
        OUTPUT_VALUE_KIND_IF => {
            let mut children = decode_output_value_children(
                slot,
                root,
                3,
                value_slots,
                value_fields,
                input_bytes,
                memo,
                visiting,
                "if expression",
            )?
            .into_iter();
            let condition = children.next().expect("if condition child");
            let then_branch = children.next().expect("if then child");
            let else_branch = children.next().expect("if else child");
            IntermediateKind::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            }
        }
        OUTPUT_VALUE_KIND_BLOCK => {
            if slot.item_start < 0 || slot.item_count < 0 {
                return Err(Diagnostic::new(format!(
                    "Output block value has invalid item span start={} count={}",
                    slot.item_start, slot.item_count
                )));
            }
            let item_start = slot.item_start as usize;
            let item_count = slot.item_count as usize;
            let item_end = item_start.saturating_add(item_count);
            if item_end > value_fields.len() {
                return Err(Diagnostic::new(format!(
                    "Output block value item span out of bounds: start={} count={} fields={}",
                    item_start,
                    item_count,
                    value_fields.len()
                )));
            }

            let mut expressions = Vec::with_capacity(item_count);
            for field in &value_fields[item_start..item_end] {
                expressions.push(decode_output_value_inner(
                    field.value_ref,
                    value_slots,
                    value_fields,
                    input_bytes,
                    memo,
                    visiting,
                )?);
            }
            IntermediateKind::Block(expressions)
        }
        OUTPUT_VALUE_KIND_DIVERGE => {
            let mut children = decode_output_value_children(
                slot,
                root,
                1,
                value_slots,
                value_fields,
                input_bytes,
                memo,
                visiting,
                "diverge expression",
            )?
            .into_iter();
            let value = children.next().expect("diverge value child");
            let divergance_type = decode_output_diverge_expression_type(slot.value_i32)?;
            IntermediateKind::Diverge {
                value: Box::new(value),
                divergance_type,
            }
        }
        OUTPUT_VALUE_KIND_LOOP => {
            let mut children = decode_output_value_children(
                slot,
                root,
                1,
                value_slots,
                value_fields,
                input_bytes,
                memo,
                visiting,
                "loop expression",
            )?
            .into_iter();
            let body = children.next().expect("loop body child");
            IntermediateKind::Loop {
                body: Box::new(body),
            }
        }
        OUTPUT_VALUE_KIND_ARRAY => {
            if slot.item_start < 0 || slot.item_count < 0 {
                return Err(Diagnostic::new(format!(
                    "Output array value has invalid item span start={} count={}",
                    slot.item_start, slot.item_count
                )));
            }
            let item_start = slot.item_start as usize;
            let item_count = slot.item_count as usize;
            let item_end = item_start.saturating_add(item_count);
            if item_end > value_fields.len() {
                return Err(Diagnostic::new(format!(
                    "Output array value item span out of bounds: start={} count={} fields={}",
                    item_start,
                    item_count,
                    value_fields.len()
                )));
            }

            let mut items = Vec::with_capacity(item_count);
            let mut field_names = Vec::with_capacity(item_count);
            let mut item_types = Vec::with_capacity(item_count);

            for field in &value_fields[item_start..item_end] {
                let name = decode_output_value_field_name(
                    input_bytes,
                    field.name_start,
                    field.name_length,
                )?;
                let value = decode_output_value_inner(
                    field.value_ref,
                    value_slots,
                    value_fields,
                    input_bytes,
                    memo,
                    visiting,
                )?;
                item_types.push(infer_intermediate_type_from_value(&value)?);
                items.push(value);
                field_names.push(name);
            }

            let element_type = if let Some(first) = item_types.first() {
                if !item_types.iter().skip(1).all(|ty| ty == first) {
                    return Err(Diagnostic::new(
                        "Output array value has non-homogeneous element types",
                    ));
                }
                first.clone()
            } else {
                decode_output_value_type(slot.value_i32)?
            };

            IntermediateKind::ArrayLiteral {
                items,
                element_type,
                field_names: normalize_field_names(field_names),
            }
        }
        OUTPUT_VALUE_KIND_STRUCT => {
            if slot.item_start < 0 || slot.item_count < 0 {
                return Err(Diagnostic::new(format!(
                    "Output struct value has invalid item span start={} count={}",
                    slot.item_start, slot.item_count
                )));
            }
            let item_start = slot.item_start as usize;
            let item_count = slot.item_count as usize;
            let item_end = item_start.saturating_add(item_count);
            if item_end > value_fields.len() {
                return Err(Diagnostic::new(format!(
                    "Output struct value item span out of bounds: start={} count={} fields={}",
                    item_start,
                    item_count,
                    value_fields.len()
                )));
            }

            let mut struct_fields = Vec::with_capacity(item_count);
            let mut array_items = Vec::with_capacity(item_count);
            let mut field_names = Vec::with_capacity(item_count);
            let mut item_types = Vec::with_capacity(item_count);

            for field in &value_fields[item_start..item_end] {
                let name = decode_output_value_field_name(
                    input_bytes,
                    field.name_start,
                    field.name_length,
                )?;
                let value = decode_output_value_inner(
                    field.value_ref,
                    value_slots,
                    value_fields,
                    input_bytes,
                    memo,
                    visiting,
                )?;
                let value_ty = infer_intermediate_type_from_value(&value)?;
                struct_fields.push((Identifier::new(name.clone()), value.clone()));
                array_items.push(value);
                field_names.push(name);
                item_types.push(value_ty);
            }

            let homogeneous = if let Some(first) = item_types.first() {
                item_types.iter().skip(1).all(|ty| ty == first)
            } else {
                false
            };

            if !item_types.is_empty() && homogeneous && is_tuple_field_names(&field_names) {
                IntermediateKind::ArrayLiteral {
                    items: array_items,
                    element_type: item_types[0].clone(),
                    field_names: normalize_field_names(field_names),
                }
            } else {
                IntermediateKind::Struct(struct_fields)
            }
        }
        other => {
            return Err(Diagnostic::new(format!(
                "Unsupported output value kind tag {other}"
            )));
        }
    };

    visiting.remove(&root);
    memo.insert(root, decoded.clone());
    Ok(decoded)
}

fn decode_output_value_children(
    slot: &DecodedOutputValueSlot,
    root: i32,
    expected_count: usize,
    value_slots: &[DecodedOutputValueSlot],
    value_fields: &[DecodedOutputValueFieldSlot],
    input_bytes: &[u8],
    memo: &mut HashMap<i32, IntermediateKind>,
    visiting: &mut HashMap<i32, ()>,
    kind_label: &str,
) -> Result<Vec<IntermediateKind>, Diagnostic> {
    if slot.item_start < 0 || slot.item_count < 0 {
        return Err(Diagnostic::new(format!(
            "Output {kind_label} value {root} has invalid item span start={} count={}",
            slot.item_start, slot.item_count
        )));
    }

    let item_start = slot.item_start as usize;
    let item_count = slot.item_count as usize;
    if item_count != expected_count {
        return Err(Diagnostic::new(format!(
            "Output {kind_label} value {root} expected {expected_count} children, got {item_count}"
        )));
    }

    let item_end = item_start.saturating_add(item_count);
    if item_end > value_fields.len() {
        return Err(Diagnostic::new(format!(
            "Output {kind_label} value {root} item span out of bounds: start={} count={} fields={}",
            item_start,
            item_count,
            value_fields.len()
        )));
    }

    let mut children = Vec::with_capacity(item_count);
    for field in &value_fields[item_start..item_end] {
        children.push(decode_output_value_inner(
            field.value_ref,
            value_slots,
            value_fields,
            input_bytes,
            memo,
            visiting,
        )?);
    }
    Ok(children)
}

fn decode_output_value_field_name(
    input_bytes: &[u8],
    start: i32,
    len: i32,
) -> Result<String, Diagnostic> {
    if start == -1 {
        if len < 0 {
            return Err(Diagnostic::new(format!(
                "Invalid synthetic field name index {len}"
            )));
        }
        return Ok(len.to_string());
    }
    if start < -1 {
        return Err(Diagnostic::new(format!(
            "Unsupported field name encoding: start={start} len={len}"
        )));
    }
    decode_name_from_input(input_bytes, start, len)
}

fn infer_intermediate_type_from_value(
    value: &IntermediateKind,
) -> Result<IntermediateType, Diagnostic> {
    match value {
        IntermediateKind::Literal(ExpressionLiteral::Number(_))
        | IntermediateKind::Literal(ExpressionLiteral::Boolean(_)) => Ok(IntermediateType::I32),
        IntermediateKind::Literal(ExpressionLiteral::Char(_)) => Ok(IntermediateType::U8),
        IntermediateKind::Struct(fields) => {
            let mut field_types = Vec::with_capacity(fields.len());
            for (field_name, field_value) in fields {
                field_types.push((
                    field_name.name.clone(),
                    infer_intermediate_type_from_value(field_value)?,
                ));
            }
            Ok(IntermediateType::Struct(field_types))
        }
        IntermediateKind::ArrayLiteral {
            items,
            element_type,
            field_names,
        } => Ok(IntermediateType::Array {
            element: Box::new(element_type.clone()),
            length: items.len(),
            field_names: field_names.clone(),
        }),
        IntermediateKind::IntrinsicOperation(op) => match op {
            IntermediateIntrinsicOperation::Binary(..) => Ok(IntermediateType::I32),
            IntermediateIntrinsicOperation::Unary(_, UnaryIntrinsicOperator::BooleanNot) => {
                Ok(IntermediateType::I32)
            }
            IntermediateIntrinsicOperation::Unary(_, other) => Err(Diagnostic::new(format!(
                "Unable to infer intermediate type from unary intrinsic operator {:?}",
                other
            ))),
        },
        IntermediateKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            let then_ty = infer_intermediate_type_from_value(then_branch.as_ref())?;
            let else_ty = infer_intermediate_type_from_value(else_branch.as_ref())?;
            if then_ty == else_ty {
                Ok(then_ty)
            } else {
                Err(Diagnostic::new(format!(
                    "If expression branch type mismatch during output decode: then={:?} else={:?}",
                    then_ty, else_ty
                )))
            }
        }
        IntermediateKind::Block(expressions) => {
            if let Some(last) = expressions.last() {
                infer_intermediate_type_from_value(last)
            } else {
                Ok(IntermediateType::I32)
            }
        }
        IntermediateKind::Diverge { value, .. } => infer_intermediate_type_from_value(value),
        IntermediateKind::Loop { .. } => Ok(IntermediateType::I32),
        other => Err(Diagnostic::new(format!(
            "Unable to infer intermediate type from lowered output value: {:?}",
            other
        ))),
    }
}

fn is_tuple_field_names(field_names: &[String]) -> bool {
    field_names
        .iter()
        .enumerate()
        .all(|(index, name)| *name == index.to_string())
}

fn normalize_field_names(mut field_names: Vec<String>) -> Vec<String> {
    if !is_tuple_field_names(&field_names) {
        field_names.sort();
    }
    field_names
}

fn decode_name_from_input(input_bytes: &[u8], start: i32, len: i32) -> Result<String, Diagnostic> {
    if start < 0 || len < 0 {
        return Err(Diagnostic::new(format!(
            "Invalid name span from intermediate output: start={start} len={len}"
        )));
    }
    let start = start as usize;
    let len = len as usize;
    let end = start.saturating_add(len);
    if end > input_bytes.len() {
        return Err(Diagnostic::new(format!(
            "Intermediate output name span exceeds input buffer bounds: start={start} len={len} input_len={}",
            input_bytes.len()
        )));
    }
    String::from_utf8(input_bytes[start..end].to_vec())
        .map_err(|err| Diagnostic::new(format!("Intermediate output name is not UTF-8: {err}")))
}

fn call_required_i32_export(
    instance: &Instance,
    store: &mut Store<()>,
    export: &str,
) -> Result<i32, Diagnostic> {
    instance
        .get_typed_func::<(), i32>(&mut *store, export)
        .map_err(|err| Diagnostic::new(format!("Missing intermediate export `{export}`: {err}")))?
        .call(&mut *store, ())
        .map_err(|err| Diagnostic::new(format!("Intermediate call `{export}` failed: {err}")))
}

fn required_i32_to_i32_export(
    instance: &Instance,
    store: &mut Store<()>,
    export: &str,
) -> Result<wasmtime::TypedFunc<i32, i32>, Diagnostic> {
    instance
        .get_typed_func::<i32, i32>(&mut *store, export)
        .map_err(|err| Diagnostic::new(format!("Missing intermediate export `{export}`: {err}")))
}

fn lower_error_code_message(code: i32) -> &'static str {
    match code {
        0 => "none",
        1 => "input buffer bounds check failed",
        2 => "reserved bad-magic error",
        3 => "reserved bad-version error",
        4 => "binding count mismatch",
        5 => "ast body parse failed",
        6 => "ast body count mismatch",
        7 => "output encoding failed",
        _ => "unknown lower error",
    }
}

fn decode_output_target(tag: i32) -> Result<TargetLiteral, Diagnostic> {
    match tag {
        OUTPUT_TARGET_JS => Ok(TargetLiteral::JSTarget),
        OUTPUT_TARGET_WASM => Ok(TargetLiteral::WasmTarget),
        OUTPUT_TARGET_WGSL => Ok(TargetLiteral::WgslTarget),
        _ => Err(Diagnostic::new(format!(
            "Unsupported output target tag {tag}"
        ))),
    }
}

fn decode_output_export_type(tag: i32) -> Result<IntermediateExportType, Diagnostic> {
    match tag {
        OUTPUT_EXPORT_TYPE_FUNCTION => Ok(IntermediateExportType::Function),
        OUTPUT_EXPORT_TYPE_GLOBAL => Ok(IntermediateExportType::Global),
        _ => Err(Diagnostic::new(format!(
            "Unsupported output export type tag {tag}"
        ))),
    }
}

fn decode_output_binary_intrinsic_operator(
    tag: i32,
) -> Result<BinaryIntrinsicOperator, Diagnostic> {
    match tag {
        0 => Ok(BinaryIntrinsicOperator::I32Add),
        1 => Ok(BinaryIntrinsicOperator::I32Subtract),
        2 => Ok(BinaryIntrinsicOperator::I32Multiply),
        3 => Ok(BinaryIntrinsicOperator::I32Divide),
        4 => Ok(BinaryIntrinsicOperator::I32Equal),
        5 => Ok(BinaryIntrinsicOperator::I32NotEqual),
        6 => Ok(BinaryIntrinsicOperator::I32LessThan),
        7 => Ok(BinaryIntrinsicOperator::I32GreaterThan),
        8 => Ok(BinaryIntrinsicOperator::I32LessThanOrEqual),
        9 => Ok(BinaryIntrinsicOperator::I32GreaterThanOrEqual),
        10 => Ok(BinaryIntrinsicOperator::BooleanAnd),
        11 => Ok(BinaryIntrinsicOperator::BooleanOr),
        12 => Ok(BinaryIntrinsicOperator::BooleanXor),
        _ => Err(Diagnostic::new(format!(
            "Unsupported binary intrinsic operator tag {tag}"
        ))),
    }
}

fn decode_output_unary_intrinsic_operator(tag: i32) -> Result<UnaryIntrinsicOperator, Diagnostic> {
    match tag {
        0 => Ok(UnaryIntrinsicOperator::BooleanNot),
        1 => Ok(UnaryIntrinsicOperator::EnumFromStruct),
        2 => Ok(UnaryIntrinsicOperator::MatchFromStruct),
        3 => Ok(UnaryIntrinsicOperator::UseFromString),
        4 => Ok(UnaryIntrinsicOperator::BoxFromType),
        5 => Ok(UnaryIntrinsicOperator::BindingAnnotationExportFromTarget),
        6 => Ok(UnaryIntrinsicOperator::BindingAnnotationTargetFromTarget),
        7 => Ok(UnaryIntrinsicOperator::BindingAnnotationWrapFromTarget),
        8 => Ok(UnaryIntrinsicOperator::AssemblyFromTarget),
        _ => Err(Diagnostic::new(format!(
            "Unsupported unary intrinsic operator tag {tag}"
        ))),
    }
}

fn decode_output_diverge_expression_type(tag: i32) -> Result<DivergeExpressionType, Diagnostic> {
    match tag {
        0 => Ok(DivergeExpressionType::Return),
        1 => Ok(DivergeExpressionType::Break),
        _ => Err(Diagnostic::new(format!(
            "Unsupported diverge expression type tag {tag}"
        ))),
    }
}

fn decode_output_global_type(tag: i32) -> Result<Option<IntermediateType>, Diagnostic> {
    match tag {
        OUTPUT_GLOBAL_TYPE_INFER => Ok(None),
        OUTPUT_GLOBAL_TYPE_I32 => Ok(Some(IntermediateType::I32)),
        OUTPUT_GLOBAL_TYPE_U8 => Ok(Some(IntermediateType::U8)),
        _ => Err(Diagnostic::new(format!(
            "Unsupported output global type tag {tag}"
        ))),
    }
}

fn decode_output_value_type(tag: i32) -> Result<IntermediateType, Diagnostic> {
    match tag {
        OUTPUT_VALUE_TYPE_I32 => Ok(IntermediateType::I32),
        OUTPUT_VALUE_TYPE_U8 => Ok(IntermediateType::U8),
        OUTPUT_VALUE_TYPE_UNKNOWN => Err(Diagnostic::new(
            "Unable to infer array element type from empty output array value",
        )),
        _ => Err(Diagnostic::new(format!(
            "Unsupported output value type tag {tag}"
        ))),
    }
}

fn decode_output_literal(tag: i32, value_i32: i32) -> Result<ExpressionLiteral, Diagnostic> {
    match tag {
        OUTPUT_VALUE_TAG_NUMBER => Ok(ExpressionLiteral::Number(value_i32)),
        OUTPUT_VALUE_TAG_BOOLEAN => Ok(ExpressionLiteral::Boolean(value_i32 != 0)),
        OUTPUT_VALUE_TAG_CHAR => {
            if !(0..=u8::MAX as i32).contains(&value_i32) {
                return Err(Diagnostic::new(format!(
                    "Char literal value out of range for u8: {value_i32}"
                )));
            }
            Ok(ExpressionLiteral::Char(value_i32 as u8))
        }
        _ => Err(Diagnostic::new(format!(
            "Unsupported output literal tag {tag}"
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wasm_stage_accepts_ast_input_and_returns_known_status() {
        if !wasm_stage_available() {
            return;
        }
        let source = "x := 1; y := x + 2; y";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 2);
        assert_eq!(lowered.globals.len(), 0);
        assert_eq!(lowered.exports.len(), 0);
        assert_eq!(lowered.wrappers.len(), 0);
        assert!(matches!(
            lowered.inline_bindings.get("x"),
            Some(IntermediateKind::Literal(ExpressionLiteral::Number(1)))
        ));
        assert!(matches!(
            lowered.inline_bindings.get("y"),
            Some(IntermediateKind::IntrinsicOperation(
                IntermediateIntrinsicOperation::Binary(_, _, BinaryIntrinsicOperator::I32Add)
            ))
        ));
    }

    #[test]
    fn lower_context_uses_silk_backend_when_stage_is_available() {
        if std::env::var_os("SILK_DISABLE_WASM_INTERMEDIATE").is_some() {
            return;
        }
        if !wasm_stage_available() {
            return;
        }

        let source = "(export wasm) answer := 42; answer";
        let context = interpreted_context(source);
        let (_lowered, backend) =
            lower_context(&context, source).expect("lower_context should succeed");
        assert_eq!(backend, IntermediateLoweringBackend::SilkWasm);
    }

    #[test]
    fn wasm_stage_lowers_exported_literal_global() {
        if !wasm_stage_available() {
            return;
        }
        let source = "(export wasm) answer := 42; answer";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.wrappers.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 0);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);

        let global = &lowered.globals[0];
        assert_eq!(global.name, "answer");
        assert_eq!(global.ty, IntermediateType::I32);
        assert!(matches!(
            global.value,
            IntermediateKind::Literal(ExpressionLiteral::Number(42))
        ));

        let export = &lowered.exports[0];
        assert_eq!(export.target, TargetLiteral::WasmTarget);
        assert_eq!(export.name, "answer");
        assert_eq!(export.export_type, IntermediateExportType::Global);
        assert_eq!(export.index, 0);
    }

    #[test]
    fn wasm_stage_lowers_exported_literal_global_with_target_annotation() {
        if !wasm_stage_available() {
            return;
        }
        let source = "(target wasm) base := 1; (export wasm) answer := 42; answer";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.wrappers.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 1);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);
        assert!(matches!(
            lowered.inline_bindings.get("base"),
            Some(IntermediateKind::Literal(ExpressionLiteral::Number(1)))
        ));

        assert_eq!(lowered.globals[0].name, "answer");
        assert_eq!(lowered.globals[0].ty, IntermediateType::I32);
        assert!(matches!(
            lowered.globals[0].value,
            IntermediateKind::Literal(ExpressionLiteral::Number(42))
        ));
        assert_eq!(lowered.exports[0].target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.exports[0].name, "answer");
        assert_eq!(
            lowered.exports[0].export_type,
            IntermediateExportType::Global
        );
        assert_eq!(lowered.exports[0].index, 0);
    }

    #[test]
    fn wasm_stage_lowers_wrapped_literal_global() {
        if !wasm_stage_available() {
            return;
        }
        let source = "(export wasm) (wrap js) answer: i32 := 42; answer";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 0);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);
        assert_eq!(lowered.wrappers.len(), 1);

        assert_eq!(lowered.globals[0].name, "answer");
        assert_eq!(lowered.globals[0].ty, IntermediateType::I32);
        assert!(matches!(
            lowered.globals[0].value,
            IntermediateKind::Literal(ExpressionLiteral::Number(42))
        ));

        assert_eq!(lowered.exports[0].target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.exports[0].name, "answer");
        assert_eq!(
            lowered.exports[0].export_type,
            IntermediateExportType::Global
        );
        assert_eq!(lowered.exports[0].index, 0);

        assert_eq!(lowered.wrappers[0].source_target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.wrappers[0].wrap_target, TargetLiteral::JSTarget);
        assert_eq!(lowered.wrappers[0].name, "answer");
        assert_eq!(
            lowered.wrappers[0].export_type,
            IntermediateExportType::Global
        );
        assert_eq!(lowered.wrappers[0].index, 0);
    }

    #[test]
    fn wasm_stage_lowers_wrapped_literal_global_with_multiple_export_targets() {
        if !wasm_stage_available() {
            return;
        }
        let source = "(export wasm) (export wgsl) (wrap js) answer: i32 := 42; answer";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 0);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 2);
        assert_eq!(lowered.wrappers.len(), 1);

        assert_eq!(lowered.globals[0].name, "answer");
        assert_eq!(lowered.globals[0].ty, IntermediateType::I32);
        assert!(matches!(
            lowered.globals[0].value,
            IntermediateKind::Literal(ExpressionLiteral::Number(42))
        ));

        assert_eq!(lowered.exports[0].target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.exports[0].name, "answer");
        assert_eq!(
            lowered.exports[0].export_type,
            IntermediateExportType::Global
        );
        assert_eq!(lowered.exports[0].index, 0);

        assert_eq!(lowered.exports[1].target, TargetLiteral::WgslTarget);
        assert_eq!(lowered.exports[1].name, "answer");
        assert_eq!(
            lowered.exports[1].export_type,
            IntermediateExportType::Global
        );
        assert_eq!(lowered.exports[1].index, 0);

        assert_eq!(lowered.wrappers[0].source_target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.wrappers[0].wrap_target, TargetLiteral::JSTarget);
        assert_eq!(lowered.wrappers[0].name, "answer");
        assert_eq!(
            lowered.wrappers[0].export_type,
            IntermediateExportType::Global
        );
        assert_eq!(lowered.wrappers[0].index, 0);
    }

    #[test]
    fn wasm_stage_uses_first_export_target_order_for_wrapped_literal_global() {
        if !wasm_stage_available() {
            return;
        }
        let source = "(export wasm) (export js) (wrap wgsl) answer: i32 := 42; answer";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 2);
        assert_eq!(lowered.wrappers.len(), 1);
        assert_eq!(lowered.wrappers[0].source_target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.wrappers[0].wrap_target, TargetLiteral::WgslTarget);
    }

    #[test]
    fn wasm_stage_ignores_wrap_without_export_for_inline_literal_binding() {
        if !wasm_stage_available() {
            return;
        }
        let source = "(wrap js) answer := 42; answer";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 1);
        assert_eq!(lowered.globals.len(), 0);
        assert_eq!(lowered.exports.len(), 0);
        assert_eq!(lowered.wrappers.len(), 0);
        assert!(matches!(
            lowered.inline_bindings.get("answer"),
            Some(IntermediateKind::Literal(ExpressionLiteral::Number(42)))
        ));
    }

    #[test]
    fn wasm_stage_lowers_non_exported_mut_literal_global() {
        if !wasm_stage_available() {
            return;
        }
        let source = "mut counter := 7; counter";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 0);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 0);
        assert_eq!(lowered.wrappers.len(), 0);

        assert_eq!(lowered.globals[0].name, "counter");
        assert_eq!(lowered.globals[0].ty, IntermediateType::I32);
        assert!(matches!(
            lowered.globals[0].value,
            IntermediateKind::Literal(ExpressionLiteral::Number(7))
        ));
    }

    #[test]
    fn wasm_stage_lowers_exported_u8_typed_literal_global() {
        if !wasm_stage_available() {
            return;
        }
        let source = "(export wasm) byte: u8 := 7; byte";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 0);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);

        assert_eq!(lowered.globals[0].name, "byte");
        assert_eq!(lowered.globals[0].ty, IntermediateType::U8);
        assert!(matches!(
            lowered.globals[0].value,
            IntermediateKind::Literal(ExpressionLiteral::Number(7))
        ));
        assert_eq!(lowered.exports[0].target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.exports[0].name, "byte");
        assert_eq!(
            lowered.exports[0].export_type,
            IntermediateExportType::Global
        );
    }

    #[test]
    fn wasm_stage_lowers_exported_u8_typed_literal_global_with_type_alias() {
        if !wasm_stage_available() {
            return;
        }
        let source = "Byte := u8; (export wasm) byte: Byte := 255; byte";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 0);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);

        assert_eq!(lowered.globals[0].name, "byte");
        assert_eq!(lowered.globals[0].ty, IntermediateType::U8);
        assert!(matches!(
            lowered.globals[0].value,
            IntermediateKind::Literal(ExpressionLiteral::Number(255))
        ));
        assert_eq!(lowered.exports[0].target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.exports[0].name, "byte");
        assert_eq!(
            lowered.exports[0].export_type,
            IntermediateExportType::Global
        );
        assert_eq!(lowered.exports[0].index, 0);
    }

    #[test]
    fn wasm_stage_lowers_exported_literal_identifier_alias() {
        if !wasm_stage_available() {
            return;
        }
        let source = "base := 42; (export wasm) answer := base; answer";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 1);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);
        assert!(matches!(
            lowered.inline_bindings.get("base"),
            Some(IntermediateKind::Literal(ExpressionLiteral::Number(42)))
        ));

        assert_eq!(lowered.globals[0].name, "answer");
        assert_eq!(lowered.globals[0].ty, IntermediateType::I32);
        assert!(matches!(
            lowered.globals[0].value,
            IntermediateKind::Literal(ExpressionLiteral::Number(42))
        ));
        assert_eq!(lowered.exports[0].target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.exports[0].name, "answer");
        assert_eq!(
            lowered.exports[0].export_type,
            IntermediateExportType::Global
        );
        assert_eq!(lowered.exports[0].index, 0);
    }

    #[test]
    fn wasm_stage_preserves_u8_type_through_scalar_alias_chain() {
        if !wasm_stage_available() {
            return;
        }
        let source = "Byte := u8; seed: Byte := 255; (export wasm) out := seed; out";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 1);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);
        assert!(matches!(
            lowered.inline_bindings.get("seed"),
            Some(IntermediateKind::Literal(ExpressionLiteral::Number(255)))
        ));

        assert_eq!(lowered.globals[0].name, "out");
        assert_eq!(lowered.globals[0].ty, IntermediateType::U8);
        assert!(matches!(
            lowered.globals[0].value,
            IntermediateKind::Literal(ExpressionLiteral::Number(255))
        ));
        assert_eq!(lowered.exports[0].target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.exports[0].name, "out");
        assert_eq!(
            lowered.exports[0].export_type,
            IntermediateExportType::Global
        );
        assert_eq!(lowered.exports[0].index, 0);
    }

    #[test]
    fn wasm_stage_lowers_non_literal_mut_struct_global() {
        if !wasm_stage_available() {
            return;
        }
        let source = "mut point := { x = 1, y = 2 }; point";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 0);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 0);
        assert_eq!(lowered.wrappers.len(), 0);

        assert_eq!(lowered.globals[0].name, "point");
        assert_eq!(
            lowered.globals[0].ty,
            IntermediateType::Struct(vec![
                ("x".to_string(), IntermediateType::I32),
                ("y".to_string(), IntermediateType::I32),
            ])
        );
        match &lowered.globals[0].value {
            IntermediateKind::Struct(fields) => {
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].0.name, "x");
                assert!(matches!(
                    fields[0].1,
                    IntermediateKind::Literal(ExpressionLiteral::Number(1))
                ));
                assert_eq!(fields[1].0.name, "y");
                assert!(matches!(
                    fields[1].1,
                    IntermediateKind::Literal(ExpressionLiteral::Number(2))
                ));
            }
            other => panic!("expected struct literal global value, got {other:?}"),
        }
    }

    #[test]
    fn wasm_stage_lowers_exported_struct_identifier_alias() {
        if !wasm_stage_available() {
            return;
        }
        let source = "base := { x = 1, y = 2 }; (export wasm) point := base; point";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 1);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);
        assert_eq!(lowered.wrappers.len(), 0);

        assert!(matches!(
            lowered.inline_bindings.get("base"),
            Some(IntermediateKind::Struct(_))
        ));
        assert_eq!(
            lowered.globals[0].ty,
            IntermediateType::Struct(vec![
                ("x".to_string(), IntermediateType::I32),
                ("y".to_string(), IntermediateType::I32),
            ])
        );
        assert!(matches!(
            lowered.globals[0].value,
            IntermediateKind::Struct(_)
        ));
        assert_eq!(lowered.exports[0].target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.exports[0].name, "point");
        assert_eq!(
            lowered.exports[0].export_type,
            IntermediateExportType::Global
        );
        assert_eq!(lowered.exports[0].index, 0);
    }

    #[test]
    fn wasm_stage_lowers_exported_struct_property_access_from_function_call_shape() {
        if !wasm_stage_available() {
            return;
        }
        let source = "base := { x = 7, y = 8 }; (export wasm) out := base.x; out";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 1);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);
        assert_eq!(lowered.wrappers.len(), 0);

        assert!(matches!(
            lowered.inline_bindings.get("base"),
            Some(IntermediateKind::Struct(_))
        ));
        assert_eq!(lowered.globals[0].name, "out");
        assert_eq!(lowered.globals[0].ty, IntermediateType::I32);
        assert!(matches!(
            lowered.globals[0].value,
            IntermediateKind::Literal(ExpressionLiteral::Number(7))
        ));
        assert_eq!(lowered.exports[0].target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.exports[0].name, "out");
        assert_eq!(
            lowered.exports[0].export_type,
            IntermediateExportType::Global
        );
        assert_eq!(lowered.exports[0].index, 0);
    }

    #[test]
    fn wasm_stage_lowers_exported_array_index_from_function_call_shape() {
        if !wasm_stage_available() {
            return;
        }
        let source = "base := {10, 20, 30}; (export wasm) out := base(1); out";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 1);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);
        assert_eq!(lowered.wrappers.len(), 0);

        assert_eq!(lowered.globals[0].name, "out");
        assert_eq!(lowered.globals[0].ty, IntermediateType::I32);
        assert!(matches!(
            lowered.globals[0].value,
            IntermediateKind::Literal(ExpressionLiteral::Number(20))
        ));
        assert_eq!(lowered.exports[0].target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.exports[0].name, "out");
        assert_eq!(
            lowered.exports[0].export_type,
            IntermediateExportType::Global
        );
        assert_eq!(lowered.exports[0].index, 0);
    }

    #[test]
    fn wasm_stage_lowers_array_index_from_function_call_shape_with_scalar_alias() {
        if !wasm_stage_available() {
            return;
        }
        let source = "idx := 2; base := {10, 20, 30}; (export wasm) out := base(idx); out";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 2);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);
        assert_eq!(lowered.wrappers.len(), 0);

        assert_eq!(lowered.globals[0].name, "out");
        assert_eq!(lowered.globals[0].ty, IntermediateType::I32);
        assert!(matches!(
            lowered.globals[0].value,
            IntermediateKind::Literal(ExpressionLiteral::Number(30))
        ));
        assert_eq!(lowered.exports[0].target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.exports[0].name, "out");
        assert_eq!(
            lowered.exports[0].export_type,
            IntermediateExportType::Global
        );
        assert_eq!(lowered.exports[0].index, 0);
    }

    #[test]
    fn wasm_stage_lowers_exported_dynamic_struct_property_access_with_explicit_type() {
        if !wasm_stage_available() {
            return;
        }
        let source = "left := { x = 7 }; right := { x = 8 }; pick_obj := if true then left else right; (export wasm) out: i32 := pick_obj.x; out";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);
        assert_eq!(lowered.wrappers.len(), 0);
        assert_eq!(lowered.globals[0].name, "out");
        assert_eq!(lowered.globals[0].ty, IntermediateType::I32);
        match &lowered.globals[0].value {
            IntermediateKind::TypePropertyAccess { object, property } => {
                assert_eq!(property, "x");
                assert!(matches!(object.as_ref(), IntermediateKind::If { .. }));
            }
            other => panic!("expected type-property access value, got {other:?}"),
        }
        assert_eq!(lowered.exports[0].target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.exports[0].name, "out");
        assert_eq!(
            lowered.exports[0].export_type,
            IntermediateExportType::Global
        );
        assert_eq!(lowered.exports[0].index, 0);
    }

    #[test]
    fn wasm_stage_lowers_exported_dynamic_array_index_with_explicit_type() {
        if !wasm_stage_available() {
            return;
        }
        let source = "base := {10, 20, 30}; idx := if true then 1 else 2; (export wasm) out: i32 := base(idx); out";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);
        assert_eq!(lowered.wrappers.len(), 0);
        assert_eq!(lowered.globals[0].name, "out");
        assert_eq!(lowered.globals[0].ty, IntermediateType::I32);
        match &lowered.globals[0].value {
            IntermediateKind::ArrayIndex { array, index } => {
                assert!(matches!(
                    array.as_ref(),
                    IntermediateKind::ArrayLiteral { .. }
                ));
                assert!(matches!(index.as_ref(), IntermediateKind::If { .. }));
            }
            other => panic!("expected array-index value, got {other:?}"),
        }
        assert_eq!(lowered.exports[0].target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.exports[0].name, "out");
        assert_eq!(
            lowered.exports[0].export_type,
            IntermediateExportType::Global
        );
        assert_eq!(lowered.exports[0].index, 0);
    }

    #[test]
    fn wasm_stage_lowers_exported_string_global_as_u8_array() {
        if !wasm_stage_available() {
            return;
        }
        let source = "(export wasm) bytes := \"abc\"; bytes";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 0);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);

        assert_eq!(
            lowered.globals[0].ty,
            IntermediateType::Array {
                element: Box::new(IntermediateType::U8),
                length: 3,
                field_names: vec!["0".to_string(), "1".to_string(), "2".to_string()],
            }
        );
        match &lowered.globals[0].value {
            IntermediateKind::ArrayLiteral {
                items,
                element_type,
                field_names,
            } => {
                assert_eq!(*element_type, IntermediateType::U8);
                assert_eq!(
                    field_names,
                    &vec!["0".to_string(), "1".to_string(), "2".to_string()]
                );
                assert_eq!(items.len(), 3);
                assert!(matches!(
                    items[0],
                    IntermediateKind::Literal(ExpressionLiteral::Char(b'a'))
                ));
                assert!(matches!(
                    items[1],
                    IntermediateKind::Literal(ExpressionLiteral::Char(b'b'))
                ));
                assert!(matches!(
                    items[2],
                    IntermediateKind::Literal(ExpressionLiteral::Char(b'c'))
                ));
            }
            other => panic!("expected array literal value, got {other:?}"),
        }
        assert_eq!(lowered.exports[0].target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.exports[0].name, "bytes");
        assert_eq!(
            lowered.exports[0].export_type,
            IntermediateExportType::Global
        );
        assert_eq!(lowered.exports[0].index, 0);
    }

    #[test]
    fn wasm_stage_lowers_exported_string_identifier_alias() {
        if !wasm_stage_available() {
            return;
        }
        let source = "base := \"abc\"; (export wasm) out := base; out";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 1);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);
        assert!(matches!(
            lowered.inline_bindings.get("base"),
            Some(IntermediateKind::ArrayLiteral { .. })
        ));

        assert_eq!(lowered.globals[0].name, "out");
        assert_eq!(
            lowered.globals[0].ty,
            IntermediateType::Array {
                element: Box::new(IntermediateType::U8),
                length: 3,
                field_names: vec!["0".to_string(), "1".to_string(), "2".to_string()],
            }
        );
        assert!(matches!(
            lowered.globals[0].value,
            IntermediateKind::ArrayLiteral { .. }
        ));
        assert_eq!(lowered.exports[0].target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.exports[0].name, "out");
        assert_eq!(
            lowered.exports[0].export_type,
            IntermediateExportType::Global
        );
        assert_eq!(lowered.exports[0].index, 0);
    }

    #[test]
    fn wasm_stage_lowers_inline_string_binding_without_materialized_outputs() {
        if !wasm_stage_available() {
            return;
        }
        let source = "base := \"abc\"; base";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.globals.len(), 0);
        assert_eq!(lowered.exports.len(), 0);
        assert_eq!(lowered.wrappers.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 1);
        assert!(matches!(
            lowered.inline_bindings.get("base"),
            Some(IntermediateKind::ArrayLiteral { .. })
        ));
    }

    #[test]
    fn wasm_stage_lowers_exported_empty_string_global_as_empty_u8_array() {
        if !wasm_stage_available() {
            return;
        }
        let source = "(export wasm) empty := \"\"; empty";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 0);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);

        assert_eq!(
            lowered.globals[0].ty,
            IntermediateType::Array {
                element: Box::new(IntermediateType::U8),
                length: 0,
                field_names: vec![],
            }
        );
        match &lowered.globals[0].value {
            IntermediateKind::ArrayLiteral {
                items,
                element_type,
                field_names,
            } => {
                assert_eq!(*element_type, IntermediateType::U8);
                assert!(items.is_empty());
                assert!(field_names.is_empty());
            }
            other => panic!("expected array literal value, got {other:?}"),
        }
    }

    #[test]
    fn wasm_stage_lowers_exported_array_repeat_scalar_global() {
        if !wasm_stage_available() {
            return;
        }
        let source = "(export wasm) triple := {7; 3}; triple";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 0);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);

        assert_eq!(
            lowered.globals[0].ty,
            IntermediateType::Array {
                element: Box::new(IntermediateType::I32),
                length: 3,
                field_names: vec!["0".to_string(), "1".to_string(), "2".to_string()],
            }
        );
        match &lowered.globals[0].value {
            IntermediateKind::ArrayLiteral {
                items,
                element_type,
                field_names,
            } => {
                assert_eq!(*element_type, IntermediateType::I32);
                assert_eq!(
                    field_names,
                    &vec!["0".to_string(), "1".to_string(), "2".to_string()]
                );
                assert_eq!(items.len(), 3);
                assert!(matches!(
                    items[0],
                    IntermediateKind::Literal(ExpressionLiteral::Number(7))
                ));
                assert!(matches!(
                    items[1],
                    IntermediateKind::Literal(ExpressionLiteral::Number(7))
                ));
                assert!(matches!(
                    items[2],
                    IntermediateKind::Literal(ExpressionLiteral::Number(7))
                ));
            }
            other => panic!("expected array literal value, got {other:?}"),
        }
    }

    #[test]
    fn wasm_stage_lowers_exported_zero_length_array_repeat_scalar_global() {
        if !wasm_stage_available() {
            return;
        }
        let source = "(export wasm) empty := {7; 0}; empty";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 0);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);

        assert_eq!(
            lowered.globals[0].ty,
            IntermediateType::Array {
                element: Box::new(IntermediateType::I32),
                length: 0,
                field_names: vec![],
            }
        );
        match &lowered.globals[0].value {
            IntermediateKind::ArrayLiteral {
                items,
                element_type,
                field_names,
            } => {
                assert_eq!(*element_type, IntermediateType::I32);
                assert!(items.is_empty());
                assert!(field_names.is_empty());
            }
            other => panic!("expected array literal value, got {other:?}"),
        }
    }

    #[test]
    fn wasm_stage_lowers_exported_binary_intrinsic_expression_without_evaluation() {
        if !wasm_stage_available() {
            return;
        }
        let source = "(export wasm) out := 1 + 2; out";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 0);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);

        assert_eq!(lowered.globals[0].name, "out");
        assert_eq!(lowered.globals[0].ty, IntermediateType::I32);
        match &lowered.globals[0].value {
            IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Binary(
                left,
                right,
                operator,
            )) => {
                assert_eq!(*operator, BinaryIntrinsicOperator::I32Add);
                assert!(matches!(
                    left.as_ref(),
                    IntermediateKind::Literal(ExpressionLiteral::Number(1))
                ));
                assert!(matches!(
                    right.as_ref(),
                    IntermediateKind::Literal(ExpressionLiteral::Number(2))
                ));
            }
            other => panic!("expected binary intrinsic output, got {other:?}"),
        }
    }

    #[test]
    fn wasm_stage_lowers_inline_binary_intrinsic_expression_without_materialization() {
        if !wasm_stage_available() {
            return;
        }
        let source = "sum := 1 + 2; sum";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.globals.len(), 0);
        assert_eq!(lowered.exports.len(), 0);
        assert_eq!(lowered.wrappers.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 1);

        match lowered.inline_bindings.get("sum") {
            Some(IntermediateKind::IntrinsicOperation(IntermediateIntrinsicOperation::Binary(
                left,
                right,
                operator,
            ))) => {
                assert_eq!(*operator, BinaryIntrinsicOperator::I32Add);
                assert!(matches!(
                    left.as_ref(),
                    IntermediateKind::Literal(ExpressionLiteral::Number(1))
                ));
                assert!(matches!(
                    right.as_ref(),
                    IntermediateKind::Literal(ExpressionLiteral::Number(2))
                ));
            }
            other => panic!("expected inline binary intrinsic output, got {other:?}"),
        }
    }

    #[test]
    fn wasm_stage_lowers_exported_if_expression_without_evaluation() {
        if !wasm_stage_available() {
            return;
        }
        let source = "(export wasm) out := if true then 1 else 2; out";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 0);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);

        assert_eq!(lowered.globals[0].name, "out");
        assert_eq!(lowered.globals[0].ty, IntermediateType::I32);
        match &lowered.globals[0].value {
            IntermediateKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                assert!(matches!(
                    condition.as_ref(),
                    IntermediateKind::Literal(ExpressionLiteral::Boolean(true))
                ));
                assert!(matches!(
                    then_branch.as_ref(),
                    IntermediateKind::Literal(ExpressionLiteral::Number(1))
                ));
                assert!(matches!(
                    else_branch.as_ref(),
                    IntermediateKind::Literal(ExpressionLiteral::Number(2))
                ));
            }
            other => panic!("expected if expression output, got {other:?}"),
        }
    }

    #[test]
    fn wasm_stage_lowers_inline_block_expression_without_materialization() {
        if !wasm_stage_available() {
            return;
        }
        let source = "value := (1; 2; 3); value";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.globals.len(), 0);
        assert_eq!(lowered.exports.len(), 0);
        assert_eq!(lowered.wrappers.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 1);

        match lowered.inline_bindings.get("value") {
            Some(IntermediateKind::Block(expressions)) => {
                assert_eq!(expressions.len(), 3);
                assert!(matches!(
                    expressions[0],
                    IntermediateKind::Literal(ExpressionLiteral::Number(1))
                ));
                assert!(matches!(
                    expressions[1],
                    IntermediateKind::Literal(ExpressionLiteral::Number(2))
                ));
                assert!(matches!(
                    expressions[2],
                    IntermediateKind::Literal(ExpressionLiteral::Number(3))
                ));
            }
            other => panic!("expected inline block output, got {other:?}"),
        }
    }

    #[test]
    fn wasm_stage_lowers_exported_loop_expression_without_evaluation() {
        if !wasm_stage_available() {
            return;
        }
        let source = "(export wasm) out: i32 := loop (break 7); out";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 0);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);

        assert_eq!(lowered.globals[0].name, "out");
        assert_eq!(lowered.globals[0].ty, IntermediateType::I32);
        match &lowered.globals[0].value {
            IntermediateKind::Loop { body } => {
                assert!(matches!(
                    body.as_ref(),
                    IntermediateKind::Literal(ExpressionLiteral::Number(7))
                ));
            }
            other => panic!("expected loop expression output, got {other:?}"),
        }
    }

    #[test]
    fn wasm_stage_lowers_function_exports() {
        if !wasm_stage_available() {
            return;
        }
        let source = "(export wasm) id := (x: i32) => x; id";
        let context = interpreted_context(source);
        let (lowered, backend) =
            lower_context(&context, source).expect("lower_context should succeed");
        assert_eq!(backend, IntermediateLoweringBackend::SilkWasm);

        assert_eq!(lowered.functions.len(), 1);
        assert_eq!(lowered.globals.len(), 0);
        assert_eq!(lowered.exports.len(), 1);
        assert_eq!(lowered.wrappers.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 0);

        assert_eq!(lowered.exports[0].target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.exports[0].name, "id");
        assert_eq!(
            lowered.exports[0].export_type,
            IntermediateExportType::Function
        );
        assert_eq!(lowered.exports[0].index, 0);
    }

    #[test]
    fn wasm_stage_lowers_function_exports_via_identifier_alias() {
        if !wasm_stage_available() {
            return;
        }
        let source = "id := (x: i32) => x; (export wasm) alias := id; alias";
        let context = interpreted_context(source);
        let (lowered, backend) =
            lower_context(&context, source).expect("lower_context should succeed");
        assert_eq!(backend, IntermediateLoweringBackend::SilkWasm);

        assert_eq!(lowered.functions.len(), 1);
        assert_eq!(lowered.globals.len(), 0);
        assert_eq!(lowered.exports.len(), 1);
        assert_eq!(lowered.wrappers.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 0);

        assert_eq!(lowered.exports[0].target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.exports[0].name, "alias");
        assert_eq!(
            lowered.exports[0].export_type,
            IntermediateExportType::Function
        );
        assert_eq!(lowered.exports[0].index, 0);
    }

    #[test]
    fn wasm_stage_lowers_function_exports_via_identifier_alias_chain() {
        if !wasm_stage_available() {
            return;
        }
        let source = "id := (x: i32) => x; first := id; (export wasm) alias := first; alias";
        let context = interpreted_context(source);
        let (lowered, backend) =
            lower_context(&context, source).expect("lower_context should succeed");
        assert_eq!(backend, IntermediateLoweringBackend::SilkWasm);

        assert_eq!(lowered.functions.len(), 1);
        assert_eq!(lowered.globals.len(), 0);
        assert_eq!(lowered.exports.len(), 1);
        assert_eq!(lowered.wrappers.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 0);

        assert_eq!(lowered.exports[0].target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.exports[0].name, "alias");
        assert_eq!(
            lowered.exports[0].export_type,
            IntermediateExportType::Function
        );
        assert_eq!(lowered.exports[0].index, 0);
    }

    #[test]
    fn wasm_stage_lowers_wrapped_function_exports() {
        if !wasm_stage_available() {
            return;
        }
        let source = "(export wasm) (wrap js) id := (x: i32) => x; id";
        let context = interpreted_context(source);
        let (lowered, backend) =
            lower_context(&context, source).expect("lower_context should succeed");
        assert_eq!(backend, IntermediateLoweringBackend::SilkWasm);

        assert_eq!(lowered.functions.len(), 1);
        assert_eq!(lowered.globals.len(), 0);
        assert_eq!(lowered.exports.len(), 1);
        assert_eq!(lowered.wrappers.len(), 1);
        assert_eq!(lowered.inline_bindings.len(), 0);

        assert_eq!(lowered.wrappers[0].source_target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.wrappers[0].wrap_target, TargetLiteral::JSTarget);
        assert_eq!(lowered.wrappers[0].name, "id");
        assert_eq!(
            lowered.wrappers[0].export_type,
            IntermediateExportType::Function
        );
        assert_eq!(lowered.wrappers[0].index, 0);
    }

    #[test]
    fn wasm_stage_lowers_wrapped_function_exports_via_identifier_alias() {
        if !wasm_stage_available() {
            return;
        }
        let source = "id := (x: i32) => x; (export wasm) (wrap js) alias := id; alias";
        let context = interpreted_context(source);
        let (lowered, backend) =
            lower_context(&context, source).expect("lower_context should succeed");
        assert_eq!(backend, IntermediateLoweringBackend::SilkWasm);

        assert_eq!(lowered.functions.len(), 1);
        assert_eq!(lowered.globals.len(), 0);
        assert_eq!(lowered.exports.len(), 1);
        assert_eq!(lowered.wrappers.len(), 1);
        assert_eq!(lowered.inline_bindings.len(), 0);

        assert_eq!(lowered.exports[0].target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.exports[0].name, "alias");
        assert_eq!(
            lowered.exports[0].export_type,
            IntermediateExportType::Function
        );
        assert_eq!(lowered.exports[0].index, 0);

        assert_eq!(lowered.wrappers[0].source_target, TargetLiteral::WasmTarget);
        assert_eq!(lowered.wrappers[0].wrap_target, TargetLiteral::JSTarget);
        assert_eq!(lowered.wrappers[0].name, "alias");
        assert_eq!(
            lowered.wrappers[0].export_type,
            IntermediateExportType::Function
        );
        assert_eq!(lowered.wrappers[0].index, 0);
    }

    #[test]
    fn wasm_stage_lowers_inline_scalar_binding_without_materialized_outputs() {
        if !wasm_stage_available() {
            return;
        }
        let source = "base := 7; base";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.globals.len(), 0);
        assert_eq!(lowered.exports.len(), 0);
        assert_eq!(lowered.wrappers.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 1);
        assert!(matches!(
            lowered.inline_bindings.get("base"),
            Some(IntermediateKind::Literal(ExpressionLiteral::Number(7)))
        ));
    }

    #[test]
    fn wasm_stage_lowers_inline_struct_binding_without_materialized_outputs() {
        if !wasm_stage_available() {
            return;
        }
        let source = "base := { x = 7, y = 8 }; base";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.globals.len(), 0);
        assert_eq!(lowered.exports.len(), 0);
        assert_eq!(lowered.wrappers.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 1);
        assert!(matches!(
            lowered.inline_bindings.get("base"),
            Some(IntermediateKind::Struct(_))
        ));
    }

    fn interpreted_context(source: &str) -> Context {
        let ast = crate::loader::parse_source_block(source).expect("source should parse");
        let mut context = crate::interpret::intrinsic_context();
        crate::interpret::interpret_program_for_context(ast, &mut context)
            .expect("source should interpret")
    }

    fn wasm_stage_available() -> bool {
        match stage_ready() {
            Ok(true) => true,
            Ok(false) => panic!("wasm-stage test requires the stage to be ready"),
            Err(err) => panic!(
                "wasm-stage test requires the stage to be ready: {}",
                err.message
            ),
        }
    }
}
