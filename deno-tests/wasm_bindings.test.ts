import {
  assertEquals,
  assertInstanceOf,
  assertStringIncludes,
} from "@std/asserts";
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

  const { double_add } = (await compileToInstance(
    silkCode,
    "wasm_bindings_double",
  )).exports as { double_add: (value: number) => number };
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

  const { increment_twice } = (await compileToInstance(
    silkCode,
    "wasm_bindings_mut",
  )).exports as { increment_twice: (value: number) => number };
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

  const { sum_boxed } = (await compileToInstance(
    silkCode,
    "wasm_bindings_boxed",
  )).exports as { sum_boxed: () => number };
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
    const { box_a, box_b } = instance.exports as {
      box_a: WebAssembly.Memory;
      box_b: WebAssembly.Memory;
    };
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

Deno.test("copies boxed u8 array host data to boxed u32 array", async () => {
  const silkCode = `
    (export wasm) source: Box({u8; 4}) := {0; 4};
    (export wasm) mut target: Box({i32; 4}) := {0; 4};
    (export wasm) copy_box := {} => (
      for i in 0..4 do (
        target(i) = source(i);
      )
    );
    {}
  `;

  const { wasmPath, silkPath } = await compileToWasm(
    silkCode,
    "wasm_bindings_box_copy",
  );
  try {
    const bytes = await Deno.readFile(wasmPath);
    const { instance } = await WebAssembly.instantiate(bytes);
    const { source, target, copy_box } = instance.exports as {
      source: WebAssembly.Memory;
      target: WebAssembly.Memory;
      copy_box: () => void;
    };
    assertInstanceOf(source, WebAssembly.Memory);
    assertInstanceOf(target, WebAssembly.Memory);

    const sourceView = new Uint8Array(source.buffer, 0, 4);
    sourceView.set([5, 6, 7, 8]);

    copy_box();

    const targetView = new Uint32Array(target.buffer, 0, 4);
    assertEquals([...targetView], [5, 6, 7, 8]);
  } finally {
    await cleanup([silkPath, wasmPath]);
  }
});

Deno.test("copies boxed u8 array host data to boxed u32 struct array individually", async () => {
  const silkCode = `
    (export wasm) source: Box({u8; 4}) := {0; 4};
    (export wasm) mut target: Box({{a = i32, b = i32}; 2}) := {{a = 0, b = 0}; 2};
    (export wasm) copy_box := {} => (
      for i in 0..2 do (
        target(i).a = source(2*i);
        target(i).b = source(2*i + 1);
      )
    );
    {}
  `;

  const { wasmPath, silkPath } = await compileToWasm(
    silkCode,
    "wasm_bindings_box_copy",
  );
  try {
    const bytes = await Deno.readFile(wasmPath);
    const { instance } = await WebAssembly.instantiate(bytes);
    const { source, target, copy_box } = instance.exports as {
      source: WebAssembly.Memory;
      target: WebAssembly.Memory;
      copy_box: () => void;
    };
    assertInstanceOf(source, WebAssembly.Memory);
    assertInstanceOf(target, WebAssembly.Memory);

    const sourceView = new Uint8Array(source.buffer, 0, 4);
    sourceView.set([5, 6, 7, 8]);

    copy_box();

    const targetView = new Uint32Array(target.buffer, 0, 4);
    assertEquals([...targetView], [5, 6, 7, 8]);
  } finally {
    await cleanup([silkPath, wasmPath]);
  }
});

Deno.test("copies boxed u8 array host data to boxed u32 struct array as group", async () => {
  const silkCode = `
    (export wasm) source: Box({u8; 4}) := {0; 4};
    (export wasm) mut target: Box({{a = i32, b = i32}; 2}) := {{a = 0, b = 0}; 2};
    (export wasm) copy_box := {} => (
      for i in 0..2 do (
        target(i) = {a = source(2*i), b = source(2*i + 1)};
      )
    );
    {}
  `;

  const { wasmPath, silkPath } = await compileToWasm(
    silkCode,
    "wasm_bindings_box_copy",
  );
  try {
    const bytes = await Deno.readFile(wasmPath);
    const { instance } = await WebAssembly.instantiate(bytes);
    const { source, target, copy_box } = instance.exports as {
      source: WebAssembly.Memory;
      target: WebAssembly.Memory;
      copy_box: () => void;
    };
    assertInstanceOf(source, WebAssembly.Memory);
    assertInstanceOf(target, WebAssembly.Memory);

    const sourceView = new Uint8Array(source.buffer, 0, 4);
    sourceView.set([5, 6, 7, 8]);

    copy_box();

    const targetView = new Uint32Array(target.buffer, 0, 4);
    assertEquals([...targetView], [5, 6, 7, 8]);
  } finally {
    await cleanup([silkPath, wasmPath]);
  }
});

Deno.test("copies boxed u32 struct array host data to boxed u8 array", async () => {
  const silkCode = `
    (export wasm) source: Box({{a = i32, b = i32}; 2}) := {{a = 0, b = 0}; 2};
    (export wasm) mut target: Box({u8; 4}) := {0; 4};
    (export wasm) copy_box := {} => (
      for i in 0..2 do (
        target(2*i) = source(i).a;
        target(2*i + 1) = source(i).b;
      )
    );
    {}
  `;

  const { wasmPath, silkPath } = await compileToWasm(
    silkCode,
    "wasm_bindings_box_copy",
  );
  try {
    const bytes = await Deno.readFile(wasmPath);
    const { instance } = await WebAssembly.instantiate(bytes);
    const { source, target, copy_box } = instance.exports as {
      source: WebAssembly.Memory;
      target: WebAssembly.Memory;
      copy_box: () => void;
    };
    assertInstanceOf(source, WebAssembly.Memory);
    assertInstanceOf(target, WebAssembly.Memory);

    const sourceView = new Uint32Array(source.buffer, 0, 4);
    sourceView.set([5, 6, 7, 8]);

    copy_box();

    const targetView = new Uint8Array(target.buffer, 0, 4);
    assertEquals([...targetView], [5, 6, 7, 8]);
  } finally {
    await cleanup([silkPath, wasmPath]);
  }
});
