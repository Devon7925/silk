use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::sync::OnceLock;

use wasmtime::{Config, Engine, Instance, Memory, Module, Store};

use crate::diagnostics::{Diagnostic, SourceSpan};
use crate::intermediate::{self, IntermediateResult};
use crate::interpret::{self, BindingContext, Context, PreserveBehavior};
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

const INPUT_PAYLOAD_MAGIC: &[u8; 8] = b"SILKIMD0";
const INPUT_PAYLOAD_VERSION: u32 = 3;
const INPUT_PAYLOAD_HEADER_LEN: usize = 24;

const OUTPUT_PAYLOAD_MAGIC: &[u8; 8] = b"SILKIRD0";
const OUTPUT_PAYLOAD_VERSION: u32 = 1;
const OUTPUT_PAYLOAD_HEADER_LEN: usize = 32;

const ANNOT_MUT: u32 = 1 << 0;
const ANNOT_EXPORT_JS: u32 = 1 << 1;
const ANNOT_EXPORT_WASM: u32 = 1 << 2;
const ANNOT_EXPORT_WGSL: u32 = 1 << 3;
const ANNOT_TARGET_JS: u32 = 1 << 4;
const ANNOT_TARGET_WASM: u32 = 1 << 5;
const ANNOT_TARGET_WGSL: u32 = 1 << 6;
const ANNOT_WRAP_JS: u32 = 1 << 7;
const ANNOT_WRAP_WASM: u32 = 1 << 8;
const ANNOT_WRAP_WGSL: u32 = 1 << 9;

const VALUE_TAG_UNKNOWN: u8 = 0;
const VALUE_TAG_NUMBER: u8 = 1;
const VALUE_TAG_BOOLEAN: u8 = 2;
const VALUE_TAG_CHAR: u8 = 3;

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

    let payload = encode_context_payload(context)?;
    write_input_payload(&mut store, &instance, &payload)?;

    let lower_func = instance
        .get_typed_func::<(i32, i32, i32), i32>(&mut store, "lower_context")
        .map_err(|err| {
            Diagnostic::new(format!(
                "Missing intermediate export `lower_context`: {err}"
            ))
        })?;
    let scope_count = to_i32(
        context.bindings.len(),
        "Context scope count exceeds i32 while lowering with silk intermediate wasm",
    )?;
    let binding_count = to_i32(
        context.bindings.iter().map(|scope| scope.len()).sum(),
        "Context binding count exceeds i32 while lowering with silk intermediate wasm",
    )?;
    let payload_len = to_i32(
        payload.len(),
        "Serialized context payload exceeds i32 while lowering with silk intermediate wasm",
    )?;
    let status = lower_func
        .call(&mut store, (scope_count, binding_count, payload_len))
        .map_err(|err| {
            Diagnostic::new(format!("Intermediate call `lower_context` failed: {err}"))
        })?;

    match status {
        LOWER_STATUS_OK => {
            let lowered = decode_lowered_output(&mut store, &instance)?;
            if std::env::var_os("SILK_DEBUG_WASM_INTERMEDIATE").is_some() {
                eprintln!(
                    "SILK_DEBUG_WASM_INTERMEDIATE status=ok scope_count={scope_count} binding_count={binding_count} payload_len={payload_len}"
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
                    "SILK_DEBUG_WASM_INTERMEDIATE status=error code={error_code} scope_count={scope_count} binding_count={binding_count} payload_len={payload_len}"
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
        .get_typed_func::<(i32, i32, i32), i32>(&mut store, "lower_context")
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
            Diagnostic::new(format!("Failed to compile intermediate.wasm: {err}"))
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
) -> Result<IntermediateResult, Diagnostic> {
    let output_len = instance
        .get_typed_func::<(), i32>(&mut *store, "get_lower_output_len")
        .map_err(|err| {
            Diagnostic::new(format!(
                "Missing intermediate export `get_lower_output_len`: {err}"
            ))
        })?
        .call(&mut *store, ())
        .map_err(|err| {
            Diagnostic::new(format!(
                "Intermediate call `get_lower_output_len` failed: {err}"
            ))
        })?;
    if output_len < 0 {
        return Err(Diagnostic::new(format!(
            "Silk intermediate wasm returned negative output length {output_len}"
        )));
    }
    if output_len == 0 {
        return Ok(IntermediateResult {
            functions: Vec::new(),
            globals: Vec::new(),
            exports: Vec::new(),
            wrappers: Vec::new(),
            inline_bindings: HashMap::new(),
        });
    }

    let output = instance
        .get_memory(&mut *store, "intermediate_output_memory")
        .ok_or_else(|| {
            Diagnostic::new("Intermediate export `intermediate_output_memory` memory not found")
        })?;
    let bytes = output.data(&*store);
    let output_len = output_len as usize;
    if output_len > bytes.len() {
        return Err(Diagnostic::new(format!(
            "Silk intermediate wasm output length {output_len} exceeds output memory size {}",
            bytes.len()
        )));
    }
    if output_len < OUTPUT_PAYLOAD_HEADER_LEN {
        return Err(Diagnostic::new(format!(
            "Silk intermediate wasm output too small ({output_len} bytes)"
        )));
    }
    let output_bytes = &bytes[..output_len];

    if &output_bytes[0..8] != OUTPUT_PAYLOAD_MAGIC {
        return Err(Diagnostic::new(
            "Silk intermediate wasm output magic mismatch",
        ));
    }
    let output_version = read_u32(output_bytes, 8)?;
    if output_version != OUTPUT_PAYLOAD_VERSION {
        return Err(Diagnostic::new(format!(
            "Silk intermediate wasm output version mismatch: expected {}, got {output_version}",
            OUTPUT_PAYLOAD_VERSION
        )));
    }

    let function_count = read_u32(output_bytes, 12)?;
    let global_count = read_u32(output_bytes, 16)?;
    let export_count = read_u32(output_bytes, 20)?;
    let wrapper_count = read_u32(output_bytes, 24)?;
    let inline_binding_count = read_u32(output_bytes, 28)?;
    if function_count != 0
        || global_count != 0
        || export_count != 0
        || wrapper_count != 0
        || inline_binding_count != 0
    {
        return Err(Diagnostic::new(format!(
            "Silk intermediate output decoding for non-empty sections is not implemented yet (functions={function_count}, globals={global_count}, exports={export_count}, wrappers={wrapper_count}, inline_bindings={inline_binding_count})"
        )));
    }

    Ok(IntermediateResult {
        functions: Vec::new(),
        globals: Vec::new(),
        exports: Vec::new(),
        wrappers: Vec::new(),
        inline_bindings: HashMap::new(),
    })
}

fn encode_context_payload(context: &Context) -> Result<Vec<u8>, Diagnostic> {
    let scope_count = context.bindings.len();
    let binding_count: usize = context.bindings.iter().map(|scope| scope.len()).sum();
    let annotated_binding_count = context
        .bindings
        .iter()
        .flat_map(|scope| scope.values())
        .filter(|(_, annotations)| !annotations.is_empty())
        .count();

    let mut out =
        Vec::with_capacity(INPUT_PAYLOAD_HEADER_LEN + 16 + binding_count.saturating_mul(80));
    out.extend_from_slice(INPUT_PAYLOAD_MAGIC);
    push_u32(&mut out, INPUT_PAYLOAD_VERSION);
    push_u32(
        &mut out,
        u32::try_from(scope_count)
            .map_err(|_| Diagnostic::new("Context scope count exceeds u32 while encoding"))?,
    );
    push_u32(
        &mut out,
        u32::try_from(binding_count)
            .map_err(|_| Diagnostic::new("Context binding count exceeds u32 while encoding"))?,
    );
    push_u32(
        &mut out,
        u32::try_from(annotated_binding_count)
            .map_err(|_| Diagnostic::new("Annotated binding count exceeds u32 while encoding"))?,
    );

    for (scope_index, scope) in context.bindings.iter().enumerate() {
        let mut entries: Vec<_> = scope.iter().collect();
        entries.sort_by(|(left_id, _), (right_id, _)| {
            left_id
                .unique
                .cmp(&right_id.unique)
                .then(left_id.name.cmp(&right_id.name))
        });

        push_u32(
            &mut out,
            u32::try_from(scope_index)
                .map_err(|_| Diagnostic::new("Scope index exceeds u32 while encoding"))?,
        );
        push_u32(
            &mut out,
            u32::try_from(entries.len())
                .map_err(|_| Diagnostic::new("Scope entry count exceeds u32 while encoding"))?,
        );

        for (identifier, (binding_context, annotations)) in entries {
            let annotation_mask = annotation_mask(annotations);
            let (value_tag, value_i32) = binding_value_summary(binding_context);
            push_string(&mut out, &identifier.name)?;
            push_string(&mut out, &identifier.unique)?;
            push_u8(&mut out, binding_context_tag(binding_context));
            push_u8(&mut out, binding_context_preserve(binding_context));
            push_u32(
                &mut out,
                u32::try_from(annotations.len())
                    .map_err(|_| Diagnostic::new("Annotation count exceeds u32 while encoding"))?,
            );
            push_u32(&mut out, annotation_mask);
            push_u8(&mut out, value_tag);
            push_i32(&mut out, value_i32);
        }
    }

    Ok(out)
}

fn push_u8(out: &mut Vec<u8>, value: u8) {
    out.push(value);
}

fn push_u32(out: &mut Vec<u8>, value: u32) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn push_i32(out: &mut Vec<u8>, value: i32) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn push_string(out: &mut Vec<u8>, value: &str) -> Result<(), Diagnostic> {
    push_u32(
        out,
        u32::try_from(value.len())
            .map_err(|_| Diagnostic::new("String length exceeds u32 while encoding"))?,
    );
    out.extend_from_slice(value.as_bytes());
    Ok(())
}

fn annotation_mask(annotations: &[BindingAnnotation]) -> u32 {
    let mut mask = 0u32;
    for annotation in annotations {
        match annotation {
            BindingAnnotation::Mutable(_) => {
                mask |= ANNOT_MUT;
            }
            BindingAnnotation::Export(expr, _) => {
                if let ExpressionKind::Literal(ExpressionLiteral::Target(target)) = &expr.kind {
                    mask |= export_target_bit(target);
                }
            }
            BindingAnnotation::Target(target, _) => {
                mask |= target_target_bit(target);
            }
            BindingAnnotation::Wrap(target, _) => {
                mask |= wrap_target_bit(target);
            }
        }
    }
    mask
}

fn export_target_bit(target: &TargetLiteral) -> u32 {
    match target {
        TargetLiteral::JSTarget => ANNOT_EXPORT_JS,
        TargetLiteral::WasmTarget => ANNOT_EXPORT_WASM,
        TargetLiteral::WgslTarget => ANNOT_EXPORT_WGSL,
    }
}

fn target_target_bit(target: &TargetLiteral) -> u32 {
    match target {
        TargetLiteral::JSTarget => ANNOT_TARGET_JS,
        TargetLiteral::WasmTarget => ANNOT_TARGET_WASM,
        TargetLiteral::WgslTarget => ANNOT_TARGET_WGSL,
    }
}

fn wrap_target_bit(target: &TargetLiteral) -> u32 {
    match target {
        TargetLiteral::JSTarget => ANNOT_WRAP_JS,
        TargetLiteral::WasmTarget => ANNOT_WRAP_WASM,
        TargetLiteral::WgslTarget => ANNOT_WRAP_WGSL,
    }
}

fn binding_value_summary(binding: &BindingContext) -> (u8, i32) {
    match binding {
        BindingContext::Bound(expr, _, _) => match &expr.kind {
            ExpressionKind::Literal(ExpressionLiteral::Number(value)) => (VALUE_TAG_NUMBER, *value),
            ExpressionKind::Literal(ExpressionLiteral::Boolean(value)) => {
                (VALUE_TAG_BOOLEAN, if *value { 1 } else { 0 })
            }
            ExpressionKind::Literal(ExpressionLiteral::Char(value)) => {
                (VALUE_TAG_CHAR, *value as i32)
            }
            _ => (VALUE_TAG_UNKNOWN, 0),
        },
        _ => (VALUE_TAG_UNKNOWN, 0),
    }
}

fn binding_context_tag(binding: &BindingContext) -> u8 {
    match binding {
        BindingContext::Bound(_, _, _) => 0,
        BindingContext::UnboundWithType(_) => 1,
        BindingContext::UnboundWithoutType => 2,
    }
}

fn binding_context_preserve(binding: &BindingContext) -> u8 {
    match binding {
        BindingContext::Bound(_, behavior, _) => preserve_behavior_tag(*behavior),
        _ => 255,
    }
}

fn preserve_behavior_tag(behavior: PreserveBehavior) -> u8 {
    match behavior {
        PreserveBehavior::PreserveUsage => 0,
        PreserveBehavior::PreserveUsageInLoops => 1,
        PreserveBehavior::PreserveBinding => 2,
        PreserveBehavior::Inline => 3,
    }
}

fn lower_error_code_message(code: i32) -> &'static str {
    match code {
        0 => "none",
        1 => "input payload too small for header",
        2 => "input payload magic mismatch",
        3 => "input payload version mismatch",
        4 => "input payload header counts do not match arguments",
        5 => "input payload body parse failed",
        6 => "input payload body counts do not match header",
        _ => "unknown lower error",
    }
}

fn read_u32(bytes: &[u8], offset: usize) -> Result<u32, Diagnostic> {
    let end = offset.saturating_add(4);
    if end > bytes.len() {
        return Err(Diagnostic::new(format!(
            "Missing u32 field at offset {offset} in payload of {} bytes",
            bytes.len()
        )));
    }
    let raw: [u8; 4] = bytes[offset..end]
        .try_into()
        .expect("slice length already checked");
    Ok(u32::from_le_bytes(raw))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn context_payload_has_expected_header_counts() {
        let source = "x := 1; y := 2; x + y";
        let ast = crate::loader::parse_source_block(source).expect("source should parse");
        let mut context = crate::interpret::intrinsic_context();
        let lowered_context = crate::interpret::interpret_program_for_context(ast, &mut context)
            .expect("source should interpret");

        let payload = encode_context_payload(&lowered_context).expect("payload should encode");
        assert!(
            payload.len() >= INPUT_PAYLOAD_HEADER_LEN,
            "payload should include header"
        );
        assert_eq!(&payload[0..8], INPUT_PAYLOAD_MAGIC);
        assert_eq!(
            read_u32(&payload, 8).expect("payload version should decode"),
            INPUT_PAYLOAD_VERSION
        );
        assert_eq!(
            read_u32(&payload, 12).expect("scope count should decode"),
            lowered_context.bindings.len() as u32
        );
        assert_eq!(
            read_u32(&payload, 16).expect("binding count should decode"),
            lowered_context
                .bindings
                .iter()
                .map(|scope| scope.len())
                .sum::<usize>() as u32
        );
        assert_eq!(
            read_u32(&payload, 20).expect("annotated binding count should decode"),
            lowered_context
                .bindings
                .iter()
                .flat_map(|scope| scope.values())
                .filter(|(_, annotations)| !annotations.is_empty())
                .count() as u32
        );
    }

    #[test]
    fn preserve_behavior_encoding_is_stable() {
        assert_eq!(preserve_behavior_tag(PreserveBehavior::PreserveUsage), 0);
        assert_eq!(
            preserve_behavior_tag(PreserveBehavior::PreserveUsageInLoops),
            1
        );
        assert_eq!(preserve_behavior_tag(PreserveBehavior::PreserveBinding), 2);
        assert_eq!(preserve_behavior_tag(PreserveBehavior::Inline), 3);
    }

    #[test]
    fn payload_includes_annotation_mask_and_literal_summary() {
        let source = "(export wasm) answer := 42; answer";
        let ast = crate::loader::parse_source_block(source).expect("source should parse");
        let mut context = crate::interpret::intrinsic_context();
        let lowered_context = crate::interpret::interpret_program_for_context(ast, &mut context)
            .expect("source should interpret");

        let payload = encode_context_payload(&lowered_context).expect("payload should encode");
        let records = parse_binding_records(&payload).expect("payload records should parse");
        let answer = records
            .iter()
            .find(|record| record.name == "answer")
            .expect("answer binding should exist");

        assert!(answer.annotation_count > 0);
        assert_ne!(answer.annotation_mask & ANNOT_EXPORT_WASM, 0);
        assert_eq!(answer.value_tag, VALUE_TAG_NUMBER);
        assert_eq!(answer.value_i32, 42);
    }

    #[test]
    fn wasm_stage_accepts_payload_header_and_returns_known_status() {
        let source = "x := 1; y := x + 2; y";
        let ast = crate::loader::parse_source_block(source).expect("source should parse");
        let mut context = crate::interpret::intrinsic_context();
        let lowered_context = crate::interpret::interpret_program_for_context(ast, &mut context)
            .expect("source should interpret");

        let payload = encode_context_payload(&lowered_context).expect("payload should encode");
        let scope_count = lowered_context.bindings.len();
        let binding_count = lowered_context
            .bindings
            .iter()
            .map(|scope| scope.len())
            .sum::<usize>();

        let engine = wasm_engine().expect("wasm engine should initialize");
        let module = intermediate_module().expect("intermediate module should initialize");
        let mut store = Store::new(engine, ());
        let instance =
            Instance::new(&mut store, module, &[]).expect("intermediate instance should create");

        write_input_payload(&mut store, &instance, &payload).expect("payload should be written");
        let lower = instance
            .get_typed_func::<(i32, i32, i32), i32>(&mut store, "lower_context")
            .expect("lower_context export should exist");

        let status = lower
            .call(
                &mut store,
                (
                    scope_count as i32,
                    binding_count as i32,
                    payload.len() as i32,
                ),
            )
            .expect("lower_context call should succeed");

        let annotated_binding_count =
            read_u32(&payload, 20).expect("annotated binding count should decode");
        let expected_status = if annotated_binding_count == 0 {
            LOWER_STATUS_OK
        } else {
            LOWER_STATUS_UNIMPLEMENTED
        };
        assert_eq!(status, expected_status);
        assert_eq!(
            call_optional_i32_export(&instance, &mut store, "get_lower_input_magic_ok"),
            Some(1)
        );
        assert_eq!(
            call_optional_i32_export(&instance, &mut store, "get_lower_header_ok"),
            Some(1)
        );
        assert_eq!(
            call_optional_i32_export(&instance, &mut store, "get_lower_error_code"),
            Some(0)
        );
        if expected_status == LOWER_STATUS_OK {
            let output_len =
                call_optional_i32_export(&instance, &mut store, "get_lower_output_len")
                    .expect("get_lower_output_len should exist");
            assert_eq!(
                output_len, 0,
                "empty-result fast path should not emit payload bytes"
            );
        }
    }

    #[derive(Debug)]
    struct ParsedBindingRecord {
        name: String,
        annotation_count: u32,
        annotation_mask: u32,
        value_tag: u8,
        value_i32: i32,
    }

    fn parse_binding_records(bytes: &[u8]) -> Result<Vec<ParsedBindingRecord>, String> {
        if bytes.len() < INPUT_PAYLOAD_HEADER_LEN {
            return Err("payload too small".to_string());
        }
        if &bytes[0..8] != INPUT_PAYLOAD_MAGIC {
            return Err("payload magic mismatch".to_string());
        }
        let _version = read_u32(bytes, 8).map_err(|err| err.message.clone())?;
        let scope_count = read_u32(bytes, 12).map_err(|err| err.message.clone())?;
        let _binding_count = read_u32(bytes, 16).map_err(|err| err.message.clone())?;
        let _annotated_count = read_u32(bytes, 20).map_err(|err| err.message.clone())?;

        let mut cursor = INPUT_PAYLOAD_HEADER_LEN;
        let mut records = Vec::new();
        for _ in 0..scope_count {
            let _scope_index = read_u32(bytes, cursor).map_err(|err| err.message.clone())?;
            cursor += 4;
            let entry_count = read_u32(bytes, cursor).map_err(|err| err.message.clone())?;
            cursor += 4;

            for _ in 0..entry_count {
                let name = read_len_prefixed_string(bytes, &mut cursor)?;
                let _unique = read_len_prefixed_string(bytes, &mut cursor)?;
                let _binding_context_tag = read_u8(bytes, &mut cursor)?;
                let _preserve_tag = read_u8(bytes, &mut cursor)?;
                let annotation_count =
                    read_u32(bytes, cursor).map_err(|err| err.message.clone())?;
                cursor += 4;
                let annotation_mask = read_u32(bytes, cursor).map_err(|err| err.message.clone())?;
                cursor += 4;
                let value_tag = read_u8(bytes, &mut cursor)?;
                let value_i32 = read_i32(bytes, &mut cursor)?;

                records.push(ParsedBindingRecord {
                    name,
                    annotation_count,
                    annotation_mask,
                    value_tag,
                    value_i32,
                });
            }
        }

        Ok(records)
    }

    fn read_u8(bytes: &[u8], cursor: &mut usize) -> Result<u8, String> {
        if *cursor >= bytes.len() {
            return Err("unexpected end while reading u8".to_string());
        }
        let value = bytes[*cursor];
        *cursor += 1;
        Ok(value)
    }

    fn read_i32(bytes: &[u8], cursor: &mut usize) -> Result<i32, String> {
        if cursor.saturating_add(4) > bytes.len() {
            return Err("unexpected end while reading i32".to_string());
        }
        let raw: [u8; 4] = bytes[*cursor..*cursor + 4]
            .try_into()
            .map_err(|_| "failed to read i32 bytes".to_string())?;
        *cursor += 4;
        Ok(i32::from_le_bytes(raw))
    }

    fn read_len_prefixed_string(bytes: &[u8], cursor: &mut usize) -> Result<String, String> {
        let len = read_u32(bytes, *cursor).map_err(|err| err.message.clone())? as usize;
        *cursor += 4;
        if cursor.saturating_add(len) > bytes.len() {
            return Err("unexpected end while reading string bytes".to_string());
        }
        let text = String::from_utf8(bytes[*cursor..*cursor + len].to_vec())
            .map_err(|err| format!("invalid utf8 in payload string: {err}"))?;
        *cursor += len;
        Ok(text)
    }
}
