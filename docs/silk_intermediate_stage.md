# Silk Intermediate Stage

This document tracks the current host/wasm contract for the silk-based intermediate stage.

## Current State

- Source: `silk_src/intermediate.silk`
- Host bridge: `src/silk_intermediate.rs`
- Build output: `binaries/intermediate.wasm`
- Cache:
  - `target/wasm_cache/intermediate.wasmtime`
  - `target/wasm_cache/intermediate.wasmtime.hash`
  - `target/wasm_cache/intermediate.wasm.hash`

The host now serializes `Context` metadata into bytes, writes it into wasm `input` memory, and invokes `lower_context(scope_count, binding_count, input_len)` in `intermediate.wasm`.
When the payload is valid and has zero annotated bindings, the silk stage returns `Ok` using an empty-result fast path (`get_lower_output_len() == 0`). Otherwise it currently returns `Unimplemented`, and the host falls back to the Rust lowering path (`src/intermediate.rs`).

## Exported ABI

- `intermediate_stage_version() -> i32`
  - Must be `> 0` for the stage to be considered valid.
- `(export wasm) input: Box({u8; MAX_INPUT})`
  - Input payload buffer written by the host before `lower_context`.
- `(export wasm) intermediate_output_memory: Box({u8; MAX_OUTPUT})`
  - Output payload buffer (currently reserved for future non-empty `Ok` payloads).
- `lower_context(scope_count: i32, binding_count: i32, input_len: i32) -> i32`
  - Status code return value:
    - `0`: `Ok` (currently used by empty-result fast path)
    - `1`: `Unimplemented` (host falls back to Rust lowering)
    - `2`: `Error` (host reads `get_lower_error_code()` and reports a diagnostic)
- `get_lower_error_code() -> i32`
  - Optional extra error code when `lower_context` returns `2`.
  - Current values:
    - `0`: no error
    - `1`: payload too small for header (`input_len < 24`)
    - `2`: bad payload magic
    - `3`: unsupported payload version
    - `4`: header scope/binding counts mismatch call arguments
    - `5`: payload body parse failed (malformed binding records)
    - `6`: payload body parsed counts mismatch header values

Current debug exports in `intermediate.silk`:
- `get_lower_last_scope_count() -> i32`
- `get_lower_last_binding_count() -> i32`
- `get_lower_last_input_len() -> i32`
- `get_lower_input_magic_ok() -> i32`
- `get_lower_header_version() -> i32`
- `get_lower_header_scope_count() -> i32`
- `get_lower_header_binding_count() -> i32`
- `get_lower_header_annotated_binding_count() -> i32`
- `get_lower_header_ok() -> i32`
- `get_lower_parsed_binding_count() -> i32`
- `get_lower_parsed_annotated_binding_count() -> i32`
- `get_lower_parse_cursor() -> i32`
- `get_lower_body_ok() -> i32`
- `get_lower_output_len() -> i32`

## Payload Header

The serialized context payload currently starts with:
- bytes `[0..8)`: ASCII magic `SILKIMD0`
- bytes `[8..12)`: `u32` payload format version (`3`, little-endian)
- bytes `[12..16)`: `u32` scope count
- bytes `[16..20)`: `u32` binding count
- bytes `[20..24)`: `u32` annotated binding count (bindings with non-empty annotation list)

After the header:
- Repeated per scope:
  - `u32` scope index
  - `u32` entry count
- Repeated per binding entry:
  - `u32` name byte length, then name bytes
  - `u32` unique byte length, then unique bytes
  - `u8` binding context tag (`0` bound, `1` unbound-with-type, `2` unbound-without-type)
  - `u8` preserve behavior tag (`0..3`, `255` when not bound)
  - `u32` annotation count
  - `u32` annotation mask (mutable/export/target/wrap target bits)
  - `u8` value tag (`0` unknown, `1` number, `2` boolean, `3` char)
  - `i32` value payload (number/boolean-as-0-or-1/char code)

## Output Header

For future non-empty `Ok` payloads, output payload starts with:
- bytes `[0..8)`: ASCII magic `SILKIRD0`
- bytes `[8..12)`: `u32` output format version (`1`, little-endian)
- bytes `[12..16)`: `u32` function count
- bytes `[16..20)`: `u32` global count
- bytes `[20..24)`: `u32` export count
- bytes `[24..28)`: `u32` wrapper count
- bytes `[28..32)`: `u32` inline binding count

Current fast path note:
- `get_lower_output_len() == 0` means the stage produced an empty `IntermediateResult`.

## Host Flags

- `SILK_DISABLE_WASM_INTERMEDIATE=1`
  - Disable wasm intermediate stage; always use Rust fallback.
- `SILK_WASM_INTERMEDIATE_STRICT=1`
  - Do not fall back; fail when silk intermediate is unavailable/unimplemented/errors.
- `SILK_DEBUG_WASM_INTERMEDIATE=1`
  - Print fallback reason before Rust fallback.

## Next Step

Implement a serialized `IntermediateResult` output format in `silk_src/intermediate.silk` and decode it in `src/silk_intermediate.rs` for `lower_context -> Ok`.
