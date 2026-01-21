import { assertEquals } from "https://deno.land/std/testing/asserts.ts";
import { compileToInstance } from "./test_helpers.ts";

Deno.test("range operator iterates start inclusive and end exclusive", async () => {
  const silkCode = `
    (export wasm) sum_range := ({start: i32, end: i32}) => (
        mut acc := 0;
        for value in start..end do (
            acc = acc + value;
        );
        acc
    );
    {};
    `;

  const { sum_range } = (await compileToInstance(silkCode, "range_sum"))
    .exports as any;
  assertEquals(sum_range(0, 5), 10);
  assertEquals(sum_range(2, 5), 9);
  assertEquals(sum_range(5, 5), 0);
  assertEquals(sum_range(5, 3), 0);
});

Deno.test("range operator respects addition precedence on the end value", async () => {
  const silkCode = `
    (export wasm) sum_to := (end: i32) => (
        mut acc := 0;
        for value in 0..end + 1 do (
            acc = acc + value;
        );
        acc
    );
    {};
    `;

  const { sum_to } = (await compileToInstance(silkCode, "range_precedence"))
    .exports as any;
  assertEquals(sum_to(0), 0);
  assertEquals(sum_to(3), 6);
});
