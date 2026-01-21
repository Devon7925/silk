import { assertEquals } from "@std/asserts";
import { compileToInstance } from "./test_helpers.ts";

Deno.test("let as expression", async () => {
  const silkCode = `
    (export wasm) let_as_expr := (x: i32) => (
        y := x // binding succeeds, so it resolves to true
    );
    {}
    `;
  const exports = (await compileToInstance(silkCode, "if_let_expr"))
    .exports as { let_as_expr: (value: number) => number };
  assertEquals(exports.let_as_expr(5), 1);
});

Deno.test("let in if condition", async () => {
  const silkCode = `
    (export wasm) let_as_expr := (x: i32) => (
        if y := x then (
            y
        ) else (
            0
        )
    );
    {}
    `;
  const exports = (await compileToInstance(silkCode, "if_let_cond"))
    .exports as { let_as_expr: (value: number) => number };
  assertEquals(exports.let_as_expr(5), 5);
});

Deno.test("refutable let in if condition", async () => {
  const silkCode = `
    (export wasm) let_as_expr := (x: i32) => (
        if { 5, y } := { x, x + 5 } then (
            y
        ) else (
            0
        )
    );
    {}
    `;
  const exports = (await compileToInstance(silkCode, "if_let_refutable"))
    .exports as { let_as_expr: (value: number) => number };
  assertEquals(exports.let_as_expr(5), 10);
  assertEquals(exports.let_as_expr(6), 0);
});

Deno.test("let chain with boolean condition", async () => {
  const silkCode = `
    Option := enum { Some = i32, None = {} };
    (export wasm) check := (x: i32) => (
        foo := Option::Some(x);
        if (Option::Some(a) := foo) && a == 5 then (
            1
        ) else (
            0
        )
    );
    {}
    `;
  const exports = (await compileToInstance(silkCode, "if_let_bool_chain"))
    .exports as { check: (value: number) => number };
  assertEquals(exports.check(5), 1);
  assertEquals(exports.check(4), 0);
});

Deno.test("let chain with multiple lets", async () => {
  const silkCode = `
    Level2 := enum { Some = i32, None = {} };
    Level1 := enum { Some = Level2, None = {} };
    
    (export wasm) check := (x: i32) => (
        foo := if x > 0 then (Level1::Some(Level2::Some(x))) else (Level1::None);
        if (Level1::Some(a) := foo) && (Level2::Some(b) := a) then (
            b
        ) else (
            0
        )
    );
    {}
    `;
  const exports = (await compileToInstance(silkCode, "if_let_multi_chain"))
    .exports as { check: (value: number) => number };
  assertEquals(exports.check(10), 10);
  assertEquals(exports.check(-10), 0);
});

Deno.test("if let with multiple unwraps", async () => {
  const silkCode = `
    Level2 := enum { Some = i32, None = {} };
    Level1 := enum { Some = Level2, None = {} };
    
    (export wasm) check := (x: i32) => (
    foo := if x > 0 then (Level1::Some(Level2::Some(x))) else (Level1::None);
        if Level1::Some(Level2::Some(b)) := foo then (
            b
        ) else (
            0
        )
    );
    {}
    `;
  const exports = (await compileToInstance(silkCode, "if_let_multi_unwrap"))
    .exports as { check: (value: number) => number };
  assertEquals(exports.check(10), 10);
  assertEquals(exports.check(-10), 0);
});
