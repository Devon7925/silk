import { assertEquals } from "@std/asserts";
import { cleanup, compileToWasm } from "./test_helpers.ts";

async function compileToInstance(code: string, prefix: string) {
  const { wasmPath, silkPath } = await compileToWasm(code, prefix);
  const bytes = await Deno.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(bytes);
  return { instance, wasmPath, silkPath };
}

Deno.test("allows indexing mutable arrays with dynamic indices", async () => {
  const silkCode = `
(export wasm) array_value := (idx: i32) => (
    mut values := {10, 20, 30};
    values(idx) = values(idx) + 7;
    values(idx)
);
`;

  const { instance, wasmPath, silkPath } = await compileToInstance(
    silkCode,
    "arrays_dynamic_indices",
  );
  try {
    const { array_value } = instance.exports as {
      array_value: (idx: number) => number;
    };
    assertEquals(array_value(1), 27);
  } finally {
    await cleanup([silkPath, wasmPath]);
  }
});

Deno.test("supports constant index reads", async () => {
  const silkCode = `
(export wasm) read_const := {} => (
    values := {4, 8, 15, 16, 23, 42};
    values(3)
);
`;

  const { instance, wasmPath, silkPath } = await compileToInstance(
    silkCode,
    "arrays_const_index",
  );
  try {
    const { read_const } = instance.exports as { read_const: () => number };
    assertEquals(read_const(), 16);
  } finally {
    await cleanup([silkPath, wasmPath]);
  }
});

Deno.test("allows arrays of tuples", async () => {
  const silkCode = `
(export wasm) tuple_at := (idx: i32) => (
    pairs := {{1, 2}, {3, 4}, {5, 6}};
    pairs(idx).0
);
`;

  const { instance, wasmPath, silkPath } = await compileToInstance(
    silkCode,
    "arrays_tuples",
  );
  try {
    const { tuple_at } = instance.exports as {
      tuple_at: (idx: number) => number;
    };
    assertEquals(tuple_at(2), 5);
  } finally {
    await cleanup([silkPath, wasmPath]);
  }
});

Deno.test("supports nested array indexing", async () => {
  const silkCode = `
(export wasm) matrix_get := {row = row: i32, col = col: i32} => (
    matrix := {{1, 2}, {3, 4}};
    matrix(row)(col)
);
`;

  const { instance, wasmPath, silkPath } = await compileToInstance(
    silkCode,
    "arrays_nested",
  );
  try {
    const { matrix_get } = instance.exports as {
      matrix_get: (row: number, col: number) => number;
    };
    assertEquals(matrix_get(1, 0), 3);
  } finally {
    await cleanup([silkPath, wasmPath]);
  }
});
