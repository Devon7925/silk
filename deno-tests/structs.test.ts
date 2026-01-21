import { assertEquals } from "https://deno.land/std/testing/asserts.ts";
import { compileToInstance } from "./test_helpers.ts";

Deno.test("compiles struct-heavy wasm export", async () => {
  const silkCode = `
Pair: type := {
    first = i32,
    second = i32,
};
rotate_pair := (point: Pair) => (
    {
        first = 0 - point.second,
        second = point.first,
    }
);
len_squared := (point: Pair) => (
    point.first * point.first + point.second * point.second
);
(export wasm) len_rotated_squared := (x:i32) => (
    len_squared(rotate_pair{
        first = x,
        second = 2,
    })
);
`;

  const { len_rotated_squared } =
    (await compileToInstance(silkCode, "structs_len")).exports as any;
  assertEquals(len_rotated_squared(3), 13);
});

Deno.test("allows structs to flow through wasm boundaries", async () => {
  const silkCode = `
Point: type := {
    x = i32,
    y = i32,
};
normalize := (input: Point) => (
    {
        x = input.x - input.y,
        y = input.y - input.x,
    }
);
(export wasm) sum_coords := (value: i32) => (
    normalized := normalize{
        x = value + 1,
        y = value - 1,
    };
    normalized.x + normalized.y
);
`;

  const { sum_coords } = (await compileToInstance(silkCode, "structs_flow"))
    .exports as any;
  assertEquals(sum_coords(10), 0);
});
