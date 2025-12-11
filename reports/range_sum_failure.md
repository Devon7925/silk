# range_sum fixture compilation failure

## Summary
The Bun integration test `runs range_sum module and returns correct value` fails because the `range_sum.silk` fixture does not compile. During WASM lowering the compiler rejects the `while Option(i32)::Some(value) := ...` pattern with the diagnostic `Enum pattern requires struct-backed enum type`.

## Root cause
The `range_sum.silk` fixture defines a generic `Option` enum via a type function and then destructures it directly in a `while` pattern. The WASM backend expects enum destructuring to operate on a struct-backed representation, but generic enum type applications like `Option(i32)` are not normalized into that representation before lowering. As a result, `resolve_type` in `src/wasm.rs` sees the pattern's enum type as non-struct and emits `Enum pattern requires struct-backed enum type`, causing compilation (and therefore the Bun test) to fail.

## Minimal reproduction
A reduced program that hits the same diagnostic without the range machinery:

```silk
Option := (T: type) => (
    enum { Some = T, None = {} }
);

(export wasm) loop_once := {} => (
    mut opt := Option(i32)::Some(0);
    while Option(i32)::Some(value) := opt do (
        opt = Option(i32)::None;
        value
    );
    0
);
```

Running `cargo run -- fixtures/generic_option_while.silk` fails with the same `Enum pattern requires struct-backed enum type` message.
