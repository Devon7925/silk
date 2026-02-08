# Silk Interpreter Gaps (vs Rust Interpreter)

This document tracks known feature gaps between the Rust interpreter (`src/interpret.rs`) and the Silk interpreter (`silk_src/interpreter.silk`) to help plan the replacement.

**Missing Or Incomplete**
- Builtin library prelude fidelity. The Silk interpreter now supports typed `Option(T)` construction and a builtin `Range` type binding, but still does not fully mirror Rust's library-evaluated generic behavior (for example, `Iterator(...)` currently follows Rust's permissive non-type-argument behavior rather than strict generic parameter validation).
- Preserve/inlining metadata. The Rust interpreter computes `PreserveBehavior` and returns a rich `Context` used by `src/intermediate.rs`; the Silk interpreter only returns value indices, so preserve metadata is unavailable.
- Diagnostics. The Rust interpreter reports structured `Diagnostic` messages with spans; the Silk interpreter only tracks a numeric error position in `value_state.error`.

**Recently Closed**
- `use "path"` imports are now supported via a file registry (`register_file`) and cached per interpreter run; path normalization and file resolution are still handled by the host.
- `for` loops now execute iterator implementations correctly, including preserving concrete iterator types across calls.
- Range operators (`start..end`) now iterate correctly through the builtin range `next` helper.
- `Range` type bindings now resolve through the Silk intrinsic prelude (for example, `range: Range := 0..3`).
- `Box(T)` validation now rejects non-type expressions and nested `Box` types.
- Binding annotations (`export`, `target`, `wrap`) are now surfaced via binding metadata in the Silk interpreter.
- `wrap` annotation validation now mirrors Rust interpreter constraints: wrap bindings require exactly one export target, and non-function wraps are limited to wasm exports wrapped to js.
- String indexing via call syntax (for example, `"hi"(1)`) now mirrors Rust interpreter behavior, including out-of-range failure handling.
- Literal pattern matching now works for number/boolean/char/string literals in `match` branches and `while <pattern> := <value>` conditions, including enum payload literal patterns.
- Enum variant pattern matching now compares payload types for generic enums (for example, `Option(bool)::Some(_)` no longer matches `Option(i32)::Some(_)`).
- `Option(...)` now rejects non-type arguments in Silk, matching Rust interpreter behavior.

**Notes**
- This list focuses on functionality visible in the current code. Update it as the Silk interpreter adds or changes behavior.
