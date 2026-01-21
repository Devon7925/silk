import { assertEquals } from "@std/asserts";
import { compileToInstance } from "./test_helpers.ts";

Deno.test("loop can compute factorial", async () => {
  const silkCode = `
    (export wasm) factorial := (limit: i32) => (
        mut acc := 1;
        mut iter := limit;
        loop (
            acc = acc * iter;
            iter = iter - 1;
            if iter <= 0 then (
                return acc;
            )
        );
        0  // Unreachable
    );
    {};
    `;

  const exports = (await compileToInstance(silkCode, "loop_factorial"))
    .exports as { factorial: (limit: number) => number };
  assertEquals(exports.factorial(5), 120);
});

Deno.test("loop break returns value", async () => {
  const silkCode = `
    (export wasm) first_non_positive := (start: i32) => (
        mut current := start;
        loop (
            if current <= 0 then (
                break current;
            );
            current = current - 1;
        )
    );
    {};
    `;

  const exports = (await compileToInstance(silkCode, "loop_break_value"))
    .exports as { first_non_positive: (start: number) => number };
  assertEquals(exports.first_non_positive(3), 0);
  assertEquals(exports.first_non_positive(-2), -2);
});
