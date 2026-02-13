# Silk Intermediate Stage

This document tracks the current host/wasm contract for the silk-based intermediate stage.

## Current State

- Source: `silk_src/intermediate.silk`
- Host bridge: `src/silk_intermediate.rs`
- Build output: `binaries/intermediate.wasm`

The intermediate stage supports two input ABIs:

- Parser/interpreter-compatible AST input (`input`, `nodes`, `list_nodes`, `state` + `lower_context(root)`).
- Legacy slot input for bootstrap/host fallback (`set_input_*` + `lower_context_from_slots(...)`).

## Versions

- `intermediate_stage_version() -> 3`
- `intermediate_payload_version() -> 4`
- `intermediate_output_version() -> 2`

## Input ABI (AST, chainable)

This ABI matches `interpreter.silk` input memory exactly:

- `(export wasm) input: Box({u8; MAX_INPUT})`
- `(export wasm) mut nodes: Box({Node; MAX_NODES})`
- `(export wasm) mut list_nodes: Box({ListNode; MAX_LIST_NODES})`
- `(export wasm) mut state: Box(State)`

Lower call:

- `lower_context(root: i32) -> i32`

## Input ABI (Legacy slots)

The host writes identifier bytes into:

- `(export wasm) input: Box({u8; MAX_INPUT})`

Then populates slot arrays through exports:

- `reset_input_slots() -> i32`
- `set_input_counts(binding_count: i32, value_count: i32) -> i32`
- `set_input_value(idx: i32, tag: i32, payload_i32: i32) -> i32`
- `set_input_binding(idx: i32, name_start: i32, name_length: i32, value_idx: i32, is_mut: i32, target_mask: i32, export_mask: i32, wrap_mask: i32) -> i32`

Input value tags are interpreter-compatible:

- `0`: number
- `1`: boolean
- `2`: char

Target masks are interpreter-compatible:

- `1`: js
- `2`: wasm
- `4`: wgsl

## Output ABI

Lowered output is written into struct slot arrays:

- `(export wasm) mut output_globals: Box({OutputGlobalSlot; ...})`
- `(export wasm) mut output_exports: Box({OutputExportSlot; ...})`
- `(export wasm) mut output_wrappers: Box({OutputWrapperSlot; ...})`

Host reads counts and fields through getters:

- `get_lower_output_global_count()`
- `get_lower_output_export_count()`
- `get_lower_output_wrapper_count()`
- `get_lower_output_global_*`
- `get_lower_output_export_*`
- `get_lower_output_wrapper_*`

No output byte payload memory is used.

## Lower Call (Legacy slots)

- `lower_context_from_slots(scope_count: i32, binding_count: i32, input_len: i32) -> i32`

Status codes:

- `0`: ok
- `1`: unimplemented
- `2`: error

Error code export:

- `get_lower_error_code() -> i32`

## Debug Exports

The stage keeps debug/state getters for header/body/output counters, including:

- `get_lower_last_scope_count`
- `get_lower_last_binding_count`
- `get_lower_last_input_len`
- `get_lower_header_*`
- `get_lower_parsed_*`
- `get_lower_output_len`
- `get_lower_output_*_count`
- `get_lower_lowering_unimplemented`

## Host Flags

- `SILK_DISABLE_WASM_INTERMEDIATE=1`
- `SILK_WASM_INTERMEDIATE_STRICT=1`
- `SILK_DEBUG_WASM_INTERMEDIATE=1`
