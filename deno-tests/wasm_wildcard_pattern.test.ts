import { assertEquals } from "@std/asserts";
import { compileToInstance } from "./test_helpers.ts";

Deno.test("wasm: wildcard enum payloads are independent", async () => {
  const program = `
Thing := enum {
    A = { x = i32 },
    B = i32,
};

make := (flag: i32) => (
    if flag == 0 then Thing::A({ x = 1 }) else Thing::B(2)
);

(export wasm) check := (flag: i32) => (
    value := make(flag);
    if Thing::A(_) := value then 1 else (
        if Thing::B(_) := value then 2 else 3
    )
);

{}`;

  const instance = await compileToInstance(program, "wasm_wildcard_pattern");
  const exports = instance.exports as {
    check: (flag: number) => number;
  };

  assertEquals(exports.check(0), 1);
  assertEquals(exports.check(1), 2);
});
