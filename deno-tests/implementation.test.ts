import { assertEquals } from "@std/asserts";
import { compileToInstance } from "./test_helpers.ts";

Deno.test("user implementations work on type aliases", async () => {
  const silkCode = `
    Meters := i32 @ {
        square = (self: i32) => self * self,
    };

    (export wasm) square_meters := (value: i32) => (
        meters: Meters := value;
        meters.square
    );
    `;

  const exports = (await compileToInstance(silkCode, "impl_alias"))
    .exports as { square_meters: (value: number) => number };
  assertEquals(exports.square_meters(5), 25);
});

Deno.test("user implementations work on struct types", async () => {
  const silkCode = `
    Pair := { first = i32, second = i32 } @ {
        sum = (self: { first = i32, second = i32 }) => self.first + self.second,
    };

    (export wasm) sum_pair := (first: i32) => (
        pair: Pair := { first = first, second = 4 };
        pair.sum
    );
    `;

  const exports = (await compileToInstance(silkCode, "impl_struct"))
    .exports as { sum_pair: (value: number) => number };
  assertEquals(exports.sum_pair(3), 7);
});
