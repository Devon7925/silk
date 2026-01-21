import { assertEquals } from "https://deno.land/std/testing/asserts.ts";
import { compileToInstance } from "./test_helpers.ts";

const iteratorHelpers = `
RangeIter := { value = i32, limit = i32 } @ {
    iter_ty = i32,
    next = (mut self: { value = i32, limit = i32 }) => (
        if self.value < self.limit then (
            current := self.value;
            self.value = self.value + 1;
            Option(i32)::Some(current)
        ) else Option(i32)::None
    ),
};

make_range := (limit: i32) => (
    range: RangeIter := { value = 0, limit = limit };
    range
);
`;

Deno.test("for loops sum ranges", async () => {
  const silkCode = `
    ${iteratorHelpers}
    (export wasm) sum_range := (limit: i32) => (
        mut acc := 0;
        for value in make_range(limit) do (
            acc = acc + value;
        );
        acc
    );
    {};
    `;

  const { sum_range } = (await compileToInstance(silkCode, "for_sum_range"))
    .exports as any;
  assertEquals(sum_range(0), 0);
  assertEquals(sum_range(5), 10);
});

Deno.test("for loops support struct patterns", async () => {
  const silkCode = `
    PairIter := { value = i32, limit = i32 } @ {
        iter_ty = { left = i32, right = i32 },
        next = (mut self: { value = i32, limit = i32 }) => (
            if self.value < self.limit then (
                current := self.value;
                self.value = self.value + 1;
                Option({ left = i32, right = i32 })::Some({ left = current, right = current + 1 })
            ) else Option({ left = i32, right = i32 })::None
        ),
    };

    make_pairs := (limit: i32) => (
        pairs: PairIter := { value = 0, limit = limit };
        pairs
    );

    (export wasm) sum_pairs := (limit: i32) => (
        mut acc := 0;
        for { left: i32, right: i32 } in make_pairs(limit) do (
            acc = acc + left + right;
        );
        acc
    );
    {};
    `;

  const { sum_pairs } =
    (await compileToInstance(silkCode, "for_struct_patterns")).exports as any;
  assertEquals(sum_pairs(0), 0);
  assertEquals(sum_pairs(3), 9);
});

Deno.test("for loops parse iterator calls with arguments", async () => {
  const silkCode = `
    ${iteratorHelpers}
    (export wasm) sum_plus_one := (limit: i32) => (
        mut acc := 0;
        for value in make_range(limit + 1) do (
            acc = acc + value;
        );
        acc
    );
    {};
    `;

  const { sum_plus_one } =
    (await compileToInstance(silkCode, "for_call_argument")).exports as any;
  assertEquals(sum_plus_one(0), 0);
  assertEquals(sum_plus_one(4), 10);
});
