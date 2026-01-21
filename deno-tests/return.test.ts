import { assertEquals } from "@std/asserts";
import { compileToInstance } from "./test_helpers.ts";

Deno.test("return exits a function early", async () => {
  const silkCode = `
    (export wasm) early := (x: i32) => (
        if x > 0 then (
            return x + 2;
        ) else (
            x - 100
        )
    );
    {};
    `;

  const exports = (await compileToInstance(silkCode, "return_early"))
    .exports as { early: (value: number) => number };
  assertEquals(exports.early(10), 12);
  assertEquals(exports.early(-5), -105);
});

Deno.test("return inside let binding expressions still exits early", async () => {
  const silkCode = `
    (export wasm) let_return := (x: i32) => (
        y := (
            if x == 0 then (
                return 99;
            ) else (
                x * 2
            )
        );
        y + 1
    );
    {};
    `;

  const exports = (await compileToInstance(silkCode, "return_let"))
    .exports as { let_return: (value: number) => number };
  assertEquals(exports.let_return(0), 99);
  assertEquals(exports.let_return(3), 7);
});

Deno.test("return doesn't break branch type matching", async () => {
  const silkCode = `
    (export wasm) branch_types := (flag: bool) => (
        value := if flag then (
            return 5;
        ) else (
            42
        );
        value + 1
    );
    {};
    `;

  const exports = (await compileToInstance(silkCode, "return_branch"))
    .exports as { branch_types: (flag: boolean) => number };
  assertEquals(exports.branch_types(true), 5);
  assertEquals(exports.branch_types(false), 43);
});

Deno.test("returning a complex expression works", async () => {
  const silkCode = `
    (export wasm) complex := ({ x: i32, y: i32 }) => (
        return (x + y) * (y - x) + (x * x) - (y / 2);
    );
    {};
    `;

  const exports = (await compileToInstance(silkCode, "return_complex"))
    .exports as { complex: (x: number, y: number) => number };
  assertEquals(exports.complex(6, 4), 14);
  assertEquals(exports.complex(2, 10), 95);
});
