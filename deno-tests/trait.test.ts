import { assertEquals } from "@std/asserts";
import { compileToInstance } from "./test_helpers.ts";

Deno.test("trait constraints enforce implementations for generic helpers", async () => {
  const silkCode = `
    Adder := (T: type) => (
        { add = (T) -> ((T) -> T) }
    );

    Meters := i32 @ {
        add = (self: i32) => ((other: i32) => self + other),
    };

    add_pair := (T: type @ Adder) => (
        ({ left: T, right: T }) => left.add(right)
    );

    (export wasm) add_meter_pair := (left: i32) => (
        add_pair(Meters){ left, 4 }
    );
    `;

  const exports = (await compileToInstance(silkCode, "trait_generic"))
    .exports as { add_meter_pair: (left: number) => number };
  assertEquals(exports.add_meter_pair(3), 7);
});

Deno.test("trait implementations work for multiple types and structs", async () => {
  const silkCode = `
    Adder := (T: type) => (
        { add = (T) -> ((T) -> T) }
    );

    Meters := i32 @ {
        add = (self: i32) => ((other: i32) => self + other),
    };

    Seconds := i32 @ {
        add = (self: i32) => ((other: i32) => self + other),
    };

    Pair := { left = i32, right = i32 } @ {
        add = (self: { left = i32, right = i32 }) => (
            (other: { left = i32, right = i32 }) => (
                { left = self.left + other.left, right = self.right + other.right }
            )
        ),
    };

    add_pair := (T: type @ Adder) => (
        ({ left: T, right: T }) => left.add(right)
    );

    (export wasm) add_meter := (left: i32) => (
        add_pair(Meters){ left, 4 }
    );

    (export wasm) add_second := (left: i32) => (
        add_pair(Seconds){ left, 9 }
    );

    (export wasm) add_pair_struct := (left: i32) => (
        adder := add_pair(Pair);
        result := adder{
            { left = left, right = 2 },
            { left = 3, right = 4 },
        };
        result.left
    );
    `;

  const exports = (await compileToInstance(silkCode, "trait_multi"))
    .exports as {
      add_meter: (left: number) => number;
      add_second: (left: number) => number;
      add_pair_struct: (left: number) => number;
    };
  assertEquals(exports.add_meter(3), 7);
  assertEquals(exports.add_second(5), 14);
  assertEquals(exports.add_pair_struct(1), 4);
});
