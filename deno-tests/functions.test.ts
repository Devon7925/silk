import { assertEquals } from "@std/asserts";
import { compileToInstance } from "./test_helpers.ts";

Deno.test("functions can be passed as arguments", async () => {
  const silkCode = `
    (export wasm) apply_increment := (x: i32) => (
        apply := { func = func: (i32 -> i32), value = value: i32 } => (
            func value
        );

        increment := (y: i32) => (
            y + 1
        );

        apply { func = increment, value = x }
    );
    `;

  const exports = (await compileToInstance(silkCode, "functions_apply"))
    .exports as { apply_increment: (x: number) => number };
  assertEquals(exports.apply_increment(41), 42);
});

Deno.test("functions can be returned and invoked", async () => {
  const silkCode = `
    (export wasm) apply_offset := (offset: i32) => (
        make_adder := (base: i32) => (
            (y: i32) => (
                base + y
            )
        );

        add_three := make_adder 3;
        add_three offset
    );
    `;

  const exports = (await compileToInstance(silkCode, "functions_return"))
    .exports as { apply_offset: (offset: number) => number };
  assertEquals(exports.apply_offset(7), 10);
});
