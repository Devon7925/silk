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
use crate::interpret::{self, BindingContext, Context};
use crate::loader;
use crate::parsing::{BindingAnnotation, ExpressionKind, ExpressionLiteral, TargetLiteral};
use crate::wasm;

const INTERMEDIATE_SOURCE_PATH: &str = "silk_src/intermediate.silk";
const TYPES_SOURCE_PATH: &str = "silk_src/types.silk";
const INTERMEDIATE_WASM_PATH: &str = "binaries/intermediate.wasm";

const INTERMEDIATE_MODULE_CACHE_PATH: &str = "target/wasm_cache/intermediate.wasmtime";
const INTERMEDIATE_MODULE_HASH_PATH: &str = "target/wasm_cache/intermediate.wasmtime.hash";
const INTERMEDIATE_WASM_HASH_PATH: &str = "target/wasm_cache/intermediate.wasm.hash";

const LOWER_STATUS_OK: i32 = 0;
const LOWER_STATUS_UNIMPLEMENTED: i32 = 1;
const LOWER_STATUS_ERROR: i32 = 2;

const INPUT_VALUE_TAG_NUMBER: i32 = 0;
const INPUT_VALUE_TAG_BOOLEAN: i32 = 1;
const INPUT_VALUE_TAG_CHAR: i32 = 2;

const TARGET_MASK_JS: i32 = 1;
const TARGET_MASK_WASM: i32 = 2;
const TARGET_MASK_WGSL: i32 = 4;

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

#[derive(Debug, Clone)]
struct EncodedValueSlot {
    tag: i32,
    payload_i32: i32,
}

#[derive(Debug, Clone)]
struct EncodedBindingSlot {
    name_start: i32,
    name_length: i32,
    value_idx: i32,
    is_mut: i32,
    target_mask: i32,
    export_mask: i32,
    wrap_mask: i32,
}

#[derive(Debug, Clone)]
struct EncodedContextSlots {
    input_bytes: Vec<u8>,
    values: Vec<EncodedValueSlot>,
    bindings: Vec<EncodedBindingSlot>,
}

pub(crate) fn lower_context(
    context: &Context,
) -> Result<(IntermediateResult, IntermediateLoweringBackend), Diagnostic> {
    if std::env::var_os("SILK_DISABLE_WASM_INTERMEDIATE").is_some() {
        return Ok((
            intermediate::context_to_intermediate(context),
            IntermediateLoweringBackend::RustFallback,
        ));
    }

    match lower_with_wasm(context) {
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

fn lower_with_wasm(context: &Context) -> Result<Option<IntermediateResult>, Diagnostic> {
    if fs::metadata(INTERMEDIATE_SOURCE_PATH).is_err() {
        return Ok(None);
    }

    if !stage_ready()? {
        return Ok(None);
    }

    let engine = wasm_engine()?;
    let module = intermediate_module()?;
    let mut store = Store::new(engine, ());
    let instance = Instance::new(&mut store, module, &[]).map_err(|err| {
        Diagnostic::new(format!("Failed to instantiate intermediate wasm: {err}"))
    })?;

    let encoded = encode_context_slots(context)?;
    write_input_payload(&mut store, &instance, &encoded.input_bytes)?;
    write_input_slots(&mut store, &instance, &encoded)?;

    let lower_func = instance
        .get_typed_func::<(i32, i32, i32), i32>(&mut store, "lower_context_from_slots")
        .map_err(|err| {
            Diagnostic::new(format!(
                "Missing intermediate export `lower_context_from_slots`: {err}"
            ))
        })?;
    let scope_count = to_i32(
        context.bindings.len(),
        "Context scope count exceeds i32 while lowering with silk intermediate wasm",
    )?;
    let binding_count = to_i32(
        encoded.bindings.len(),
        "Context binding count exceeds i32 while lowering with silk intermediate wasm",
    )?;
    let input_len = to_i32(
        encoded.input_bytes.len(),
        "Serialized intermediate input bytes exceed i32 while lowering with silk intermediate wasm",
    )?;
    let status = lower_func
        .call(&mut store, (scope_count, binding_count, input_len))
        .map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `lower_context_from_slots` failed: {err}"
            ))
        })?;

    match status {
        LOWER_STATUS_OK => {
            let lowered = decode_lowered_output(&mut store, &instance, &encoded.input_bytes)?;
            if std::env::var_os("SILK_DEBUG_WASM_INTERMEDIATE").is_some() {
                eprintln!(
                    "SILK_DEBUG_WASM_INTERMEDIATE status=ok scope_count={scope_count} binding_count={binding_count} input_len={input_len}"
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
                    "SILK_DEBUG_WASM_INTERMEDIATE status=error code={error_code} scope_count={scope_count} binding_count={binding_count} input_len={input_len}"
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
    let module = intermediate_module()?;
    let mut store = Store::new(engine, ());
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
        .get_typed_func::<(i32, i32, i32), i32>(&mut store, "lower_context_from_slots")
        .is_ok()
        && instance
            .get_typed_func::<i32, i32>(&mut store, "lower_context")
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

fn to_i32(value: usize, err: &str) -> Result<i32, Diagnostic> {
    i32::try_from(value).map_err(|_| Diagnostic::new(err))
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

fn write_input_slots(
    store: &mut Store<()>,
    instance: &Instance,
    encoded: &EncodedContextSlots,
) -> Result<(), Diagnostic> {
    let reset = instance
        .get_typed_func::<(), i32>(&mut *store, "reset_input_slots")
        .map_err(|err| {
            Diagnostic::new(format!("Missing intermediate export `reset_input_slots`: {err}"))
        })?;
    let set_counts = instance
        .get_typed_func::<(i32, i32), i32>(&mut *store, "set_input_counts")
        .map_err(|err| {
            Diagnostic::new(format!("Missing intermediate export `set_input_counts`: {err}"))
        })?;
    let set_value = instance
        .get_typed_func::<(i32, i32, i32), i32>(&mut *store, "set_input_value")
        .map_err(|err| {
            Diagnostic::new(format!("Missing intermediate export `set_input_value`: {err}"))
        })?;
    let set_binding = instance
        .get_typed_func::<(i32, i32, i32, i32, i32, i32, i32, i32), i32>(
            &mut *store,
            "set_input_binding",
        )
        .map_err(|err| {
            Diagnostic::new(format!("Missing intermediate export `set_input_binding`: {err}"))
        })?;

    let reset_ok = reset.call(&mut *store, ()).map_err(|err| {
        Diagnostic::new(format!("Intermediate call `reset_input_slots` failed: {err}"))
    })?;
    if reset_ok != 1 {
        return Err(Diagnostic::new(
            "Intermediate call `reset_input_slots` reported failure",
        ));
    }

    let binding_count = to_i32(
        encoded.bindings.len(),
        "Encoded binding count exceeds i32 while writing intermediate input",
    )?;
    let value_count = to_i32(
        encoded.values.len(),
        "Encoded value count exceeds i32 while writing intermediate input",
    )?;
    let counts_ok = set_counts
        .call(&mut *store, (binding_count, value_count))
        .map_err(|err| {
            Diagnostic::new(format!("Intermediate call `set_input_counts` failed: {err}"))
        })?;
    if counts_ok != 1 {
        return Err(Diagnostic::new(
            "Intermediate call `set_input_counts` reported failure",
        ));
    }

    for (idx, value) in encoded.values.iter().enumerate() {
        let idx = to_i32(
            idx,
            "Encoded value index exceeds i32 while writing intermediate input",
        )?;
        let ok = set_value
            .call(&mut *store, (idx, value.tag, value.payload_i32))
            .map_err(|err| {
                Diagnostic::new(format!("Intermediate call `set_input_value` failed: {err}"))
            })?;
        if ok != 1 {
            return Err(Diagnostic::new(format!(
                "Intermediate call `set_input_value` reported failure at index {idx}"
            )));
        }
    }

    for (idx, binding) in encoded.bindings.iter().enumerate() {
        let idx = to_i32(
            idx,
            "Encoded binding index exceeds i32 while writing intermediate input",
        )?;
        let ok = set_binding
            .call(
                &mut *store,
                (
                    idx,
                    binding.name_start,
                    binding.name_length,
                    binding.value_idx,
                    binding.is_mut,
                    binding.target_mask,
                    binding.export_mask,
                    binding.wrap_mask,
                ),
            )
            .map_err(|err| {
                Diagnostic::new(format!(
                    "Intermediate call `set_input_binding` failed: {err}"
                ))
            })?;
        if ok != 1 {
            return Err(Diagnostic::new(format!(
                "Intermediate call `set_input_binding` reported failure at index {idx}"
            )));
        }
    }

    Ok(())
}

fn encode_context_slots(context: &Context) -> Result<EncodedContextSlots, Diagnostic> {
    let mut input_bytes = Vec::new();
    let mut values = Vec::new();
    let mut bindings = Vec::new();

    for scope in &context.bindings {
        let mut entries: Vec<_> = scope.iter().collect();
        entries.sort_by(|(left_id, _), (right_id, _)| {
            left_id
                .unique
                .cmp(&right_id.unique)
                .then(left_id.name.cmp(&right_id.name))
        });

        for (identifier, (binding_context, annotations)) in entries {
            let name_start = to_i32(
                input_bytes.len(),
                "Input name buffer cursor exceeds i32 while encoding intermediate input",
            )?;
            input_bytes.extend_from_slice(identifier.name.as_bytes());
            let name_length = to_i32(
                identifier.name.len(),
                "Binding name length exceeds i32 while encoding intermediate input",
            )?;

            let value_idx = to_i32(
                values.len(),
                "Encoded value index exceeds i32 while encoding intermediate input",
            )?;
            values.push(binding_value_slot(binding_context));

            let (is_mut, target_mask, export_mask, wrap_mask) = annotation_masks(annotations);
            bindings.push(EncodedBindingSlot {
                name_start,
                name_length,
                value_idx,
                is_mut,
                target_mask,
                export_mask,
                wrap_mask,
            });
        }
    }

    Ok(EncodedContextSlots {
        input_bytes,
        values,
        bindings,
    })
}

fn annotation_masks(annotations: &[BindingAnnotation]) -> (i32, i32, i32, i32) {
    let mut is_mut = 0;
    let mut target_mask = 0;
    let mut export_mask = 0;
    let mut wrap_mask = 0;

    for annotation in annotations {
        match annotation {
            BindingAnnotation::Mutable(_) => is_mut = 1,
            BindingAnnotation::Export(expr, _) => {
                if let ExpressionKind::Literal(ExpressionLiteral::Target(target)) = &expr.kind {
                    export_mask |= target_mask_bit(target);
                }
            }
            BindingAnnotation::Target(target, _) => target_mask |= target_mask_bit(target),
            BindingAnnotation::Wrap(target, _) => wrap_mask |= target_mask_bit(target),
        }
    }

    (is_mut, target_mask, export_mask, wrap_mask)
}

fn target_mask_bit(target: &TargetLiteral) -> i32 {
    match target {
        TargetLiteral::JSTarget => TARGET_MASK_JS,
        TargetLiteral::WasmTarget => TARGET_MASK_WASM,
        TargetLiteral::WgslTarget => TARGET_MASK_WGSL,
    }
}

fn binding_value_slot(binding: &BindingContext) -> EncodedValueSlot {
    match binding {
        BindingContext::Bound(expr, _, _) => match &expr.kind {
            ExpressionKind::Literal(ExpressionLiteral::Number(value)) => EncodedValueSlot {
                tag: INPUT_VALUE_TAG_NUMBER,
                payload_i32: *value,
            },
            ExpressionKind::Literal(ExpressionLiteral::Boolean(value)) => EncodedValueSlot {
                tag: INPUT_VALUE_TAG_BOOLEAN,
                payload_i32: if *value { 1 } else { 0 },
            },
            ExpressionKind::Literal(ExpressionLiteral::Char(value)) => EncodedValueSlot {
                tag: INPUT_VALUE_TAG_CHAR,
                payload_i32: *value as i32,
            },
            _ => EncodedValueSlot {
                tag: -1,
                payload_i32: 0,
            },
        },
        _ => EncodedValueSlot {
            tag: -1,
            payload_i32: 0,
        },
    }
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
        1 => "input bytes are out of bounds",
        2 => "input format magic mismatch",
        3 => "input format version mismatch",
        4 => "input binding counts do not match arguments",
        5 => "input slot parse failed",
        6 => "input slot counts do not match header",
        7 => "output payload encoding failed",
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
    fn context_slots_follow_interpreter_binding_and_value_shape() {
        let source = "(target wasm) base := 1; (export wasm) (wrap js) answer := 42; mut flag := true; ch := 'a'; answer";
        let lowered_context = interpreted_context(source);
        let encoded = encode_context_slots(&lowered_context).expect("slots should encode");

        let base = find_binding(&encoded, "base");
        assert_eq!(base.is_mut, 0);
        assert_eq!(base.target_mask, TARGET_MASK_WASM);
        assert_eq!(base.export_mask, 0);
        assert_eq!(base.wrap_mask, 0);

        let answer = find_binding(&encoded, "answer");
        assert_eq!(answer.is_mut, 0);
        assert_eq!(answer.target_mask, 0);
        assert_eq!(answer.export_mask, TARGET_MASK_WASM);
        assert_eq!(answer.wrap_mask, TARGET_MASK_JS);
        let answer_value = &encoded.values[answer.value_idx as usize];
        assert_eq!(answer_value.tag, INPUT_VALUE_TAG_NUMBER);
        assert_eq!(answer_value.payload_i32, 42);

        let flag = find_binding(&encoded, "flag");
        assert_eq!(flag.is_mut, 1);
        let flag_value = &encoded.values[flag.value_idx as usize];
        assert_eq!(flag_value.tag, INPUT_VALUE_TAG_BOOLEAN);
        assert_eq!(flag_value.payload_i32, 1);

        let ch = find_binding(&encoded, "ch");
        let ch_value = &encoded.values[ch.value_idx as usize];
        assert_eq!(ch_value.tag, INPUT_VALUE_TAG_CHAR);
        assert_eq!(ch_value.payload_i32, b'a' as i32);
    }

    #[test]
    fn wasm_stage_accepts_slot_input_and_returns_known_status() {
        if !wasm_stage_available() {
            return;
        }
        let source = "x := 1; y := x + 2; y";
        let lowered_context = interpreted_context(source);
        let encoded = encode_context_slots(&lowered_context).expect("slots should encode");

        let engine = wasm_engine().expect("wasm engine should initialize");
        let module = intermediate_module().expect("intermediate module should initialize");
        let mut store = Store::new(engine, ());
        let instance =
            Instance::new(&mut store, module, &[]).expect("intermediate instance should create");

        write_input_payload(&mut store, &instance, &encoded.input_bytes)
            .expect("input bytes should be written");
        write_input_slots(&mut store, &instance, &encoded).expect("input slots should be written");
        let lower = instance
            .get_typed_func::<(i32, i32, i32), i32>(&mut store, "lower_context_from_slots")
            .expect("lower_context_from_slots export should exist");

        let status = lower
            .call(
                &mut store,
                (
                    lowered_context.bindings.len() as i32,
                    encoded.bindings.len() as i32,
                    encoded.input_bytes.len() as i32,
                ),
            )
            .expect("lower_context_from_slots call should succeed");

        assert_eq!(status, LOWER_STATUS_OK);
        assert_eq!(
            call_optional_i32_export(&instance, &mut store, "get_lower_header_ok"),
            Some(1)
        );
        assert_eq!(
            call_optional_i32_export(&instance, &mut store, "get_lower_error_code"),
            Some(0)
        );
        assert_eq!(
            call_optional_i32_export(&instance, &mut store, "get_lower_output_global_count"),
            Some(0)
        );
        assert_eq!(
            call_optional_i32_export(&instance, &mut store, "get_lower_output_export_count"),
            Some(0)
        );
        assert_eq!(
            call_optional_i32_export(&instance, &mut store, "get_lower_output_wrapper_count"),
            Some(0)
        );
    }

    #[test]
    fn wasm_stage_lowers_exported_literal_global() {
        if !wasm_stage_available() {
            return;
        }
        let source = "(export wasm) answer := 42; answer";
        let lowered_context = interpreted_context(source);

        let lowered = lower_with_wasm(&lowered_context)
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
        let lowered_context = interpreted_context(source);

        let lowered = lower_with_wasm(&lowered_context)
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
        let lowered_context = interpreted_context(source);

        let lowered = lower_with_wasm(&lowered_context)
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
    fn wasm_stage_lowers_non_exported_mut_literal_global() {
        if !wasm_stage_available() {
            return;
        }
        let source = "mut counter := 7; counter";
        let lowered_context = interpreted_context(source);

        let lowered = lower_with_wasm(&lowered_context)
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
    fn wasm_stage_reports_unimplemented_for_non_literal_mut_global() {
        if !wasm_stage_available() {
            return;
        }
        let source = "mut point := { x = 1, y = 2 }; point";
        let lowered_context = interpreted_context(source);

        let lowered = lower_with_wasm(&lowered_context).expect("wasm lowering should run");
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
        let lowered_context = interpreted_context(source);

        let lowered = lower_with_wasm(&lowered_context).expect("wasm lowering should run");
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

    fn find_binding<'a>(encoded: &'a EncodedContextSlots, name: &str) -> &'a EncodedBindingSlot {
        for binding in &encoded.bindings {
            let binding_name = decode_name_from_input(
                &encoded.input_bytes,
                binding.name_start,
                binding.name_length,
            )
            .expect("binding name should decode");
            if binding_name == name {
                return binding;
            }
        }
        panic!("missing binding {name}");
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
