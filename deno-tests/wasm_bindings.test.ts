import {
  assertEquals,
  assertInstanceOf,
  assertStringIncludes,
} from "https://deno.land/std/testing/asserts.ts";
import {
  cleanup,
  compileSilk,
  compileToInstance,
  compileToWasm,
  tempBase,
} from "./test_helpers.ts";

Deno.test("compiles and runs wasm with bindings", async () => {
  const silkCode = `
    (export wasm) double_add := (x: i32) => (
        y := x * 2;
        y + y
    );
    {}
  `;

  const { double_add } =
    (await compileToInstance(silkCode, "wasm_bindings_double")).exports as any;
  assertEquals(double_add(5), 20);
});

Deno.test("supports mutable assignments in wasm exports", async () => {
  const silkCode = `
    (export wasm) increment_twice := (x: i32) => (
        mut total := x;
        total = total + 1;
        total = total + 1;
        total
    );
    {}
  `;

  const { increment_twice } =
    (await compileToInstance(silkCode, "wasm_bindings_mut")).exports as any;
  assertEquals(increment_twice(10), 12);
});

Deno.test("supports boxed values in wasm exports", async () => {
  const silkCode = `
    Point := { x = Box(i32), y = i32 };
    (export wasm) sum_boxed := {} => (
        boxed: Box(i32) := 10;
        p: Point := { x = 7, y = 5 };
        boxed + p.x + p.y
    );
    {}
  `;

  const { sum_boxed } =
    (await compileToInstance(silkCode, "wasm_bindings_boxed")).exports as any;
  assertEquals(sum_boxed(), 22);
});

Deno.test("rejects runtime box allocations in wasm exports", async () => {
  const silkCode = `
    (export wasm) bad_box := (x: i32) => (
        boxed: Box(i32) := x;
        boxed + 1
    );
    {}
  `;

  const basePath = tempBase("wasm_bindings_bad_box");
  const outputPath = basePath + ".wasm";
  const { code, stderr, silkPath } = await compileSilk(silkCode, outputPath);
  try {
    assertEquals(code, 1);
    assertStringIncludes(stderr, "Box values must be compile-time constants");
  } finally {
    await cleanup([silkPath, outputPath]);
  }
});

Deno.test("exports boxed globals as distinct memories", async () => {
  const silkCode = `
    (export wasm) box_a: Box(i32) := 11;
    (export wasm) box_b: Box(i32) := 42;
    {}
  `;

  const { wasmPath, silkPath } = await compileToWasm(
    silkCode,
    "wasm_bindings_multi",
  );
  try {
    const bytes = await Deno.readFile(wasmPath);
    const { instance } = await WebAssembly.instantiate(bytes);
    const { box_a, box_b } = instance.exports as any;
    assertInstanceOf(box_a, WebAssembly.Memory);
    assertInstanceOf(box_b, WebAssembly.Memory);
    if (box_a.buffer === box_b.buffer) {
      throw new Error("Expected distinct memory buffers");
    }
    const viewA = new DataView(box_a.buffer);
    const viewB = new DataView(box_b.buffer);
    assertEquals(viewA.getInt32(0, true), 11);
    assertEquals(viewB.getInt32(0, true), 42);
  } finally {
    await cleanup([silkPath, wasmPath]);
  }
});
