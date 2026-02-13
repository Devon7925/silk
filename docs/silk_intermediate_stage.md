# Silk Intermediate Stage

This document tracks the current host/wasm contract for the silk-based intermediate stage.

## Current State

- Source: `silk_src/intermediate.silk`
- Host bridge: `src/silk_intermediate.rs`
- Build output: `binaries/intermediate.wasm`

The intermediate stage now uses a single AST-memory ABI only.
The host parses source with `parser.wasm`, runs `interpreter.wasm` on that AST, then copies
`input` / `nodes` / `list_nodes` / `state` from interpreter memory into the intermediate module
and calls `lower_context(root)`.

## Versions

- `intermediate_stage_version() -> 5`
- `intermediate_payload_version() -> 6`
- `intermediate_output_version() -> 2`

## Input ABI (AST, chainable)

This ABI matches `interpreter.silk` input memory exactly:

- `(export wasm) input: Box({u8; MAX_INPUT})`
- `(export wasm) mut nodes: Box({Node; MAX_NODES})`
- `(export wasm) mut list_nodes: Box({ListNode; MAX_LIST_NODES})`
- `(export wasm) mut state: Box(State)`

Lower call:

- `lower_context(root: i32) -> i32`

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

Status codes:

- `0`: ok
- `1`: unimplemented
- `2`: error

Error code export:

- `get_lower_error_code() -> i32`

## Current Lowering Behavior

- Literal globals are lowered for bindings that are syntactically materialized in AST (`mut` or `export` annotation).
- Global type emission uses AST information (`: u8` hints and char literals map to `u8`; otherwise `i32`).
- Wrap annotations no longer force an `unimplemented` result when no export source exists.
  - For inline literal bindings with only `(wrap ...)`, the stage emits no globals/exports/wrappers.
- Wrappers are emitted for multi-target exports when a wrap target is present.
  - Source target selection is deterministic from the export mask priority (`js`, then `wasm`, then `wgsl`).
- The stage still reports `unimplemented` for unsupported value shapes (for example non-literal mutable globals and function exports/wrappers).

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
