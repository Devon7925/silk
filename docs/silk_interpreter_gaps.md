# Silk Interpreter Gaps (vs Rust Interpreter)

This document tracks known feature gaps between the Rust interpreter (`src/interpret.rs`) and the Silk interpreter (`silk_src/interpreter.silk`) to help plan the replacement.

**Missing Or Incomplete**
- `use "path"` imports. The Rust interpreter resolves files, normalizes paths, and caches imports; the Silk interpreter does not handle `UnaryIntrinsicOperator::UseFromString`, so `use` expressions are unsupported.
- Builtin library prelude fidelity. The Silk interpreter now provides `Option`, `Iterator`, and range iteration helpers at runtime, but does not fully mirror the Rust interpreter's library evaluation or type-parameter specialization.
- Target annotation integration. The Silk interpreter now enforces target context for `asm` usage and target-annotated bindings, but it still does not expose annotated bindings (`export`, `target`, `wrap`) for code generation.
- Preserve/inlining metadata. The Rust interpreter computes `PreserveBehavior` and returns a rich `Context` used by `src/intermediate.rs`; the Silk interpreter only returns value indices, so preserve metadata is unavailable.
- Diagnostics. The Rust interpreter reports structured `Diagnostic` messages with spans; the Silk interpreter only tracks a numeric error position in `value_state.error`.

**Recently Closed**
- `for` loops now execute iterator implementations correctly, including preserving concrete iterator types across calls.
- Range operators (`start..end`) now iterate correctly through the builtin range `next` helper.
- `Box(T)` validation now rejects non-type expressions and nested `Box` types.

**Notes**
- This list focuses on functionality visible in the current code. Update it as the Silk interpreter adds or changes behavior.
