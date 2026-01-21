import { assertEquals } from "https://deno.land/std/testing/asserts.ts";
import { compileToInstance } from "./test_helpers.ts";

Deno.test("while loops accumulate until reaching limit", async () => {
  const silkCode = `
    (export wasm) sum_until := (limit: i32) => (
        mut acc := 0;
        mut iter := 0;
        while iter < limit do (
            acc = acc + iter;
            iter = iter + 1;
        );
        acc
    );
    {};
    `;

  const { sum_until } = (await compileToInstance(silkCode, "while_accumulate"))
    .exports as any;
  assertEquals(sum_until(5), 10);
});

Deno.test("while loops stop when the guard is false", async () => {
  const silkCode = `
    (export wasm) decrement_to_zero := (start: i32) => (
        mut value := start;
        while value > 0 do (
            value = value - 1;
        );
        value
    );
    {};
    `;

  const { decrement_to_zero } =
    (await compileToInstance(silkCode, "while_decrement")).exports as any;
  assertEquals(decrement_to_zero(0), 0);
  assertEquals(decrement_to_zero(3), 0);
});

Deno.test("while with let expression stops when the pattern fails", async () => {
  const silkCode = `
    Option := enum { Some = i32, None = {} };
    (export wasm) sum_until_none := (limit: i32) => (
        mut iter := 0;
        mut acc := 0;
        while Option::Some(value) := (if iter < limit then Option::Some(iter) else Option::None) do (
            acc = acc + value;
            iter = iter + 1;
        );
        acc
    );
    {};
    `;

  const { sum_until_none } =
    (await compileToInstance(silkCode, "while_let_binding")).exports as any;
  assertEquals(sum_until_none(0), 0);
  assertEquals(sum_until_none(4), 6);
});

Deno.test("while break without an argument exits immediately", async () => {
  const silkCode = `
    (export wasm) break_without_value := (limit: i32) => (
        mut counter := 0;
        mut sum := 0;
        while counter < limit do (
            if counter == 2 then (
                break
            );
            sum = sum + counter;
            counter = counter + 1;
        );
        sum
    );
    {};
    `;

  const { break_without_value } =
    (await compileToInstance(silkCode, "while_break_without_value"))
      .exports as any;
  assertEquals(break_without_value(5), 1);
  assertEquals(break_without_value(1), 0);
});
