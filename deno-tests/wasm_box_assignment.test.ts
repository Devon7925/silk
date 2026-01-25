import { assertEquals } from "@std/asserts";
import { compileToInstance } from "./test_helpers.ts";

Deno.test("wasm: allows boxed struct field assignment", async () => {
  const program = `
State := { a = i32, b = i32 };

(export wasm) mut state: Box(State) := { a = 1, b = 2 };

(export wasm) set_b := (value: i32) => (
    state.b = value;
    state.b
);

{}`;

  const instance = await compileToInstance(program, "wasm_box_assignment");
  const exports = instance.exports as {
    state: WebAssembly.Memory;
    set_b: (value: number) => number;
  };

  const result = exports.set_b(42);
  assertEquals(result, 42);

  const view = new DataView(exports.state.buffer);
  assertEquals(view.getInt32(4, true), 42);
});
