# Silk Intermediate Stage

This document tracks the current host/wasm contract for the silk-based intermediate stage.

## Current State

- Source: `silk_src/intermediate.silk`
- Host bridge: `src/silk_intermediate.rs`
- Build output: `binaries/intermediate.wasm`

The intermediate stage now uses a single AST-memory ABI only.
The host parses source with `parser.wasm`, copies
`input` / `nodes` / `list_nodes` / `state` from parser memory into the intermediate module,
and calls `lower_context(root)`.
`interpreter.wasm` is still validated by the stage-readiness probe so the wasm toolchain contract
stays aligned across parser/interpreter/intermediate stages.

## Versions

- `intermediate_stage_version() -> 8`
- `intermediate_payload_version() -> 6`
- `intermediate_output_version() -> 4`

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
- `(export wasm) mut output_inline_bindings: Box({OutputInlineBindingSlot; ...})`
- `(export wasm) mut output_values: Box({OutputValueSlot; ...})`
- `(export wasm) mut output_value_fields: Box({OutputValueFieldSlot; ...})`

Host reads counts and fields through getters:

- `get_lower_output_global_count()`
- `get_lower_output_export_count()`
- `get_lower_output_wrapper_count()`
- `get_lower_output_inline_binding_count()`
- `get_lower_output_value_count()`
- `get_lower_output_value_field_count()`
- `get_lower_output_global_*`
- `get_lower_output_export_*`
- `get_lower_output_wrapper_*`
- `get_lower_output_inline_binding_*`
- `get_lower_output_value_*`
- `get_lower_output_value_field_*`

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
- Scalar type aliases are recognized for annotation hints when they resolve to `u8` / `i32` / `bool`.
- Scalar value alias chains are now lowered for materialized globals.
  - Example: `base := 42; (export wasm) answer := base` lowers to a concrete global/export entry.
  - Typed alias chains preserve scalar type tags where available (for example `Byte := u8; seed: Byte := 255; (export wasm) out := seed` keeps `out` as `u8`).
- Scalar aliases are tracked via explicit slot tables (`KnownScalarAliasSlot`) and lookup enums (`ScalarValueLookup`) in stage memory.
- General lowered value aliases are now tracked separately (`KnownValueAliasSlot`) so identifier chains can resolve to either scalar literals or structured value refs.
- Structured literal values are now lowered through value-slot tables and decoded on the host into `IntermediateKind` trees.
  - Struct literals can now be emitted for both inline bindings and materialized globals.
  - Example: `mut point := { x = 1, y = 2 }` now lowers as a concrete mutable global instead of returning `unimplemented`.
  - Example: `base := { x = 1, y = 2 }; (export wasm) point := base` now lowers `base` inline and materializes `point` via identifier aliasing.
- Non-materialized scalar bindings are emitted in `inline_bindings` as literal `IntermediateKind` values.
  - Example: `base := 42; (export wasm) answer := base` now lowers `base` into the inline-binding output table while still lowering `answer` as a global/export.
- Wrap annotations no longer force an `unimplemented` result when no export source exists.
  - For inline literal bindings with only `(wrap ...)`, the stage emits no globals/exports/wrappers.
- Wrappers are emitted for multi-target exports when a wrap target is present.
  - Source target selection is deterministic from the export mask priority (`js`, then `wasm`, then `wgsl`).
- The stage still reports `unimplemented` for unsupported value shapes (for example function exports/wrappers and non-data/non-struct mutable globals).
- Bindings with unsupported pattern extraction are now treated as `unimplemented` instead of hard parse failure, preserving fallback behavior.

## State Getters

The stage exposes state getters for header/body/output counters, including:

- `get_lower_last_scope_count`
- `get_lower_last_binding_count`
- `get_lower_last_input_len`
- `get_lower_header_*`
- `get_lower_parsed_*`
- `get_lower_output_len`
- `get_lower_output_*_count`
- `get_lower_lowering_unimplemented`

Output rows are read through:

- `get_lower_output_global_*`
- `get_lower_output_export_*`
- `get_lower_output_wrapper_*`
- `get_lower_output_inline_binding_*`
- `get_lower_output_value_*`
- `get_lower_output_value_field_*`

## Host Flags

- `SILK_DISABLE_WASM_INTERMEDIATE=1`
- `SILK_WASM_INTERMEDIATE_STRICT=1`
