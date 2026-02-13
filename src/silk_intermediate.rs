use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::sync::OnceLock;

use wasmtime::{Config, Engine, Instance, Memory, Module, Store};

use crate::diagnostics::{Diagnostic, SourceSpan};
use crate::intermediate::{
    self, IntermediateExport, IntermediateExportType, IntermediateGlobal, IntermediateKind,
    IntermediateResult, IntermediateType, IntermediateWrap,
};
use crate::interpret::{self, Context};
use crate::loader;
use crate::parsing::{ExpressionLiteral, TargetLiteral};
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

const OUTPUT_GLOBAL_TYPE_I32: i32 = 0;
const OUTPUT_GLOBAL_TYPE_U8: i32 = 1;

const OUTPUT_VALUE_TAG_NUMBER: i32 = 1;
const OUTPUT_VALUE_TAG_BOOLEAN: i32 = 2;
const OUTPUT_VALUE_TAG_CHAR: i32 = 3;

static INTERMEDIATE_WASM_BYTES: OnceLock<Result<Vec<u8>, Diagnostic>> = OnceLock::new();
static WASM_ENGINE: OnceLock<Result<Engine, Diagnostic>> = OnceLock::new();
static PARSER_MODULE: OnceLock<Result<Module, Diagnostic>> = OnceLock::new();
static INTERPRETER_MODULE: OnceLock<Result<Module, Diagnostic>> = OnceLock::new();
static INTERMEDIATE_MODULE: OnceLock<Result<Module, Diagnostic>> = OnceLock::new();
static STAGE_READY: OnceLock<Result<bool, Diagnostic>> = OnceLock::new();

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

    match lower_with_wasm(source) {
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
            if std::env::var_os("SILK_DEBUG_WASM_INTERMEDIATE").is_some() {
                eprintln!(
                    "SILK_DEBUG_WASM_INTERMEDIATE fallback_to_rust reason={}",
                    err.message
                );
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
    if fs::metadata(INTERMEDIATE_SOURCE_PATH).is_err() {
        return Ok(None);
    }

    if !stage_ready()? {
        return Ok(None);
    }

    let engine = wasm_engine()?;
    let parser_module = parser_module()?;
    let interpreter_module = interpreter_module()?;
    let module = intermediate_module()?;
    let mut store = Store::new(engine, ());
    let parser_instance = Instance::new(&mut store, parser_module, &[]).map_err(|err| {
        Diagnostic::new(format!("Failed to instantiate parser wasm: {err}"))
    })?;
    let interpreter_instance =
        Instance::new(&mut store, interpreter_module, &[]).map_err(|err| {
            Diagnostic::new(format!("Failed to instantiate interpreter wasm: {err}"))
        })?;
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
        return Err(Diagnostic::new("Parse error while preparing intermediate lowering")
            .with_span(SourceSpan::new(start.min(source.len()), 1)));
    }

    copy_named_memory(&mut store, &parser_instance, &interpreter_instance, "input")?;
    copy_named_memory(&mut store, &parser_instance, &interpreter_instance, "nodes")?;
    copy_named_memory(
        &mut store,
        &parser_instance,
        &interpreter_instance,
        "list_nodes",
    )?;
    copy_named_memory(&mut store, &parser_instance, &interpreter_instance, "state")?;

    let interpret_func = interpreter_instance
        .get_typed_func::<i32, i32>(&mut store, "interpret")
        .map_err(|err| Diagnostic::new(format!("Missing interpreter export `interpret`: {err}")))?;
    let _result = interpret_func
        .call(&mut store, root)
        .map_err(|err| Diagnostic::new(format!("Interpreter call `interpret` failed: {err}")))?;
    let interp_error = interpreter_instance
        .get_typed_func::<(), i32>(&mut store, "get_interp_error")
        .map_err(|err| {
            Diagnostic::new(format!(
                "Missing interpreter export `get_interp_error`: {err}"
            ))
        })?
        .call(&mut store, ())
        .map_err(|err| Diagnostic::new(format!("Interpreter call `get_interp_error` failed: {err}")))?;
    if interp_error != -1 {
        let interp_error_code = interpreter_instance
            .get_typed_func::<(), i32>(&mut store, "get_interp_error_code")
            .ok()
            .and_then(|func| func.call(&mut store, ()).ok())
            .unwrap_or(0);
        let start = interp_error.max(0) as usize;
        return Err(Diagnostic::new(format!(
            "Interpreter error while preparing intermediate lowering: {} (code {interp_error_code})",
            interpreter_error_code_message(interp_error_code)
        ))
        .with_span(SourceSpan::new(start.min(source.len()), 1)));
    }

    copy_named_memory(&mut store, &interpreter_instance, &instance, "input")?;
    copy_named_memory(&mut store, &interpreter_instance, &instance, "nodes")?;
    copy_named_memory(
        &mut store,
        &interpreter_instance,
        &instance,
        "list_nodes",
    )?;
    copy_named_memory(&mut store, &interpreter_instance, &instance, "state")?;

    let lower_func = instance
        .get_typed_func::<i32, i32>(&mut store, "lower_context")
        .map_err(|err| {
            Diagnostic::new(format!("Missing intermediate export `lower_context`: {err}"))
        })?;
    let status = lower_func
        .call(&mut store, root)
        .map_err(|err| {
            Diagnostic::new(format!("Intermediate call `lower_context` failed: {err}"))
        })?;

    match status {
        LOWER_STATUS_OK => {
            let lowered = decode_lowered_output(&mut store, &instance, source.as_bytes())?;
            if std::env::var_os("SILK_DEBUG_WASM_INTERMEDIATE").is_some() {
                eprintln!(
                    "SILK_DEBUG_WASM_INTERMEDIATE status=ok root={root} input_len={}",
                    source.len()
                );
                log_optional_debug_export(&instance, &mut store, "get_lower_header_ok");
                log_optional_debug_export(
                    &instance,
                    &mut store,
                    "get_lower_header_annotated_binding_count",
                );
                log_optional_debug_export(&instance, &mut store, "get_lower_output_len");
                log_optional_debug_export(&instance, &mut store, "get_lower_parsed_binding_count");
                log_optional_debug_export(
                    &instance,
                    &mut store,
                    "get_lower_parsed_annotated_binding_count",
                );
            }
            Ok(Some(lowered))
        }
        LOWER_STATUS_UNIMPLEMENTED => Ok(None),
        LOWER_STATUS_ERROR => {
            let error_code = instance
                .get_typed_func::<(), i32>(&mut store, "get_lower_error_code")
                .ok()
                .and_then(|func| func.call(&mut store, ()).ok())
                .unwrap_or(0);
            if std::env::var_os("SILK_DEBUG_WASM_INTERMEDIATE").is_some() {
                eprintln!(
                    "SILK_DEBUG_WASM_INTERMEDIATE status=error code={error_code} root={root} input_len={}",
                    source.len()
                );
                log_optional_debug_export(&instance, &mut store, "get_lower_input_magic_ok");
                log_optional_debug_export(&instance, &mut store, "get_lower_header_version");
                log_optional_debug_export(&instance, &mut store, "get_lower_header_scope_count");
                log_optional_debug_export(&instance, &mut store, "get_lower_header_binding_count");
                log_optional_debug_export(
                    &instance,
                    &mut store,
                    "get_lower_header_annotated_binding_count",
                );
                log_optional_debug_export(&instance, &mut store, "get_lower_header_ok");
                log_optional_debug_export(&instance, &mut store, "get_lower_parsed_binding_count");
                log_optional_debug_export(
                    &instance,
                    &mut store,
                    "get_lower_parsed_annotated_binding_count",
                );
                log_optional_debug_export(&instance, &mut store, "get_lower_parse_cursor");
                log_optional_debug_export(&instance, &mut store, "get_lower_body_ok");
                log_optional_debug_export(&instance, &mut store, "get_lower_last_scope_count");
                log_optional_debug_export(&instance, &mut store, "get_lower_last_binding_count");
                log_optional_debug_export(&instance, &mut store, "get_lower_last_input_len");
                log_optional_debug_export(&instance, &mut store, "get_lower_output_wrapper_count");
            }
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
    let _parser_instance = Instance::new(&mut store, parser, &[]).map_err(|err| {
        Diagnostic::new(format!("Failed to instantiate parser wasm: {err}"))
    })?;
    let interpreter_instance = Instance::new(&mut store, interpreter, &[]).map_err(|err| {
        Diagnostic::new(format!("Failed to instantiate interpreter wasm: {err}"))
    })?;
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

fn log_optional_debug_export(instance: &Instance, store: &mut Store<()>, export: &str) {
    if let Some(value) = call_optional_i32_export(instance, store, export) {
        eprintln!("SILK_DEBUG_WASM_INTERMEDIATE {export}={value}");
    }
}

fn call_optional_i32_export(
    instance: &Instance,
    store: &mut Store<()>,
    export: &str,
) -> Option<i32> {
    let func = instance
        .get_typed_func::<(), i32>(&mut *store, export)
        .ok()?;
    func.call(&mut *store, ()).ok()
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

fn decode_lowered_output(
    store: &mut Store<()>,
    instance: &Instance,
    input_bytes: &[u8],
) -> Result<IntermediateResult, Diagnostic> {
    let global_count = call_required_i32_export(instance, store, "get_lower_output_global_count")?;
    let export_count = call_required_i32_export(instance, store, "get_lower_output_export_count")?;
    let wrapper_count =
        call_required_i32_export(instance, store, "get_lower_output_wrapper_count")?;

    if global_count < 0 || export_count < 0 || wrapper_count < 0 {
        return Err(Diagnostic::new(format!(
            "Silk intermediate returned negative output counts (globals={global_count}, exports={export_count}, wrappers={wrapper_count})"
        )));
    }

    if global_count == 0 && export_count == 0 && wrapper_count == 0 {
        return Ok(IntermediateResult {
            functions: Vec::new(),
            globals: Vec::new(),
            exports: Vec::new(),
            wrappers: Vec::new(),
            inline_bindings: HashMap::new(),
        });
    }

    let get_global_name_start = required_i32_to_i32_export(
        instance,
        store,
        "get_lower_output_global_name_start",
    )?;
    let get_global_name_length = required_i32_to_i32_export(
        instance,
        store,
        "get_lower_output_global_name_length",
    )?;
    let get_global_type_tag =
        required_i32_to_i32_export(instance, store, "get_lower_output_global_type_tag")?;
    let get_global_value_tag =
        required_i32_to_i32_export(instance, store, "get_lower_output_global_value_tag")?;
    let get_global_value_i32 =
        required_i32_to_i32_export(instance, store, "get_lower_output_global_value_i32")?;

    let mut globals = Vec::with_capacity(global_count as usize);
    for idx in 0..global_count {
        let name_start = get_global_name_start.call(&mut *store, idx).map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `get_lower_output_global_name_start` failed: {err}"
            ))
        })?;
        let name_length = get_global_name_length.call(&mut *store, idx).map_err(|err| {
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

        let ty = decode_output_global_type(ty_tag)?;
        let value = decode_output_literal(value_tag, value_i32).map(IntermediateKind::Literal)?;
        globals.push(IntermediateGlobal { name, ty, value });
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
        let target_tag = get_export_target_tag.call(&mut *store, idx).map_err(|err| {
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
        let name_start = get_export_name_start.call(&mut *store, idx).map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `get_lower_output_export_name_start` failed: {err}"
            ))
        })?;
        let name_length = get_export_name_length.call(&mut *store, idx).map_err(|err| {
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
        if matches!(export_type, IntermediateExportType::Function) {
            return Err(Diagnostic::new(
                "Silk intermediate output includes function exports, but function decoding is not implemented yet",
            ));
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
    let get_wrapper_wrap_target_tag = required_i32_to_i32_export(
        instance,
        store,
        "get_lower_output_wrapper_wrap_target_tag",
    )?;
    let get_wrapper_export_type_tag = required_i32_to_i32_export(
        instance,
        store,
        "get_lower_output_wrapper_export_type_tag",
    )?;
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
        let wrap_target_tag = get_wrapper_wrap_target_tag.call(&mut *store, idx).map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `get_lower_output_wrapper_wrap_target_tag` failed: {err}"
            ))
        })?;
        let export_type_tag = get_wrapper_export_type_tag.call(&mut *store, idx).map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `get_lower_output_wrapper_export_type_tag` failed: {err}"
            ))
        })?;
        let item_index = get_wrapper_index.call(&mut *store, idx).map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `get_lower_output_wrapper_index` failed: {err}"
            ))
        })? as usize;
        let name_start = get_wrapper_name_start.call(&mut *store, idx).map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `get_lower_output_wrapper_name_start` failed: {err}"
            ))
        })?;
        let name_length = get_wrapper_name_length.call(&mut *store, idx).map_err(|err| {
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
        if matches!(export_type, IntermediateExportType::Function) {
            return Err(Diagnostic::new(
                "Silk intermediate output includes function wrappers, but function decoding is not implemented yet",
            ));
        }
        wrappers.push(IntermediateWrap {
            source_target,
            wrap_target,
            name,
            export_type,
            index: item_index,
        });
    }

    Ok(IntermediateResult {
        functions: Vec::new(),
        globals,
        exports,
        wrappers,
        inline_bindings: HashMap::new(),
    })
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

fn interpreter_error_code_message(code: i32) -> &'static str {
    match code {
        0 => "none",
        1 => "generic interpreter error",
        2 => "array index out of range",
        3 => "wrap requires exactly one export target",
        4 => "wrap global only supports wasm-to-js",
        5 => "unbound identifier",
        6 => "if branch type mismatch",
        7 => "match had no matching branch",
        8 => "missing field",
        9 => "trait missing field",
        _ => "unknown interpreter error",
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

fn decode_output_global_type(tag: i32) -> Result<IntermediateType, Diagnostic> {
    match tag {
        OUTPUT_GLOBAL_TYPE_I32 => Ok(IntermediateType::I32),
        OUTPUT_GLOBAL_TYPE_U8 => Ok(IntermediateType::U8),
        _ => Err(Diagnostic::new(format!(
            "Unsupported output global type tag {tag}"
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
        assert_eq!(lowered.inline_bindings.len(), 0);
        assert_eq!(lowered.globals.len(), 0);
        assert_eq!(lowered.exports.len(), 0);
        assert_eq!(lowered.wrappers.len(), 0);
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
        assert_eq!(lowered.inline_bindings.len(), 0);
        assert_eq!(lowered.globals.len(), 1);
        assert_eq!(lowered.exports.len(), 1);

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
    fn wasm_stage_ignores_wrap_without_export_for_inline_literal_binding() {
        if !wasm_stage_available() {
            return;
        }
        let source = "(wrap js) answer := 42; answer";
        let lowered = lower_with_wasm(source)
            .expect("wasm lowering should run")
            .expect("wasm lowering should produce a result");

        assert_eq!(lowered.functions.len(), 0);
        assert_eq!(lowered.inline_bindings.len(), 0);
        assert_eq!(lowered.globals.len(), 0);
        assert_eq!(lowered.exports.len(), 0);
        assert_eq!(lowered.wrappers.len(), 0);
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
    fn wasm_stage_reports_unimplemented_for_non_literal_mut_global() {
        if !wasm_stage_available() {
            return;
        }
        let source = "mut point := { x = 1, y = 2 }; point";
        let lowered = lower_with_wasm(source).expect("wasm lowering should run");
        assert!(
            lowered.is_none(),
            "non-literal mutable globals should still report unimplemented"
        );
    }

    #[test]
    fn wasm_stage_reports_unimplemented_for_function_exports() {
        if !wasm_stage_available() {
            return;
        }
        let source = "(export wasm) id := (x: i32) => x; id";
        let lowered = lower_with_wasm(source).expect("wasm lowering should run");
        assert!(
            lowered.is_none(),
            "function exports should still report unimplemented for now"
        );
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
            Ok(false) => {
                eprintln!("skipping wasm-stage test: stage is not ready");
                false
            }
            Err(err) => {
                eprintln!("skipping wasm-stage test: {}", err.message);
                false
            }
        }
    }

}
