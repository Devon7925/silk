import { assertEquals, assertStringIncludes } from "@std/asserts";
import {
  cleanupBase,
  compileToBase,
  importJsModule,
  loadWasm,
} from "./test_helpers.ts";

async function compileBaseOrThrow(silkCode: string, prefix: string) {
  const { basePath, code, stderr, silkPath } = await compileToBase(
    silkCode,
    prefix,
  );
  if (code !== 0) {
    await cleanupBase(basePath, [".silk", ".js", ".wasm", ".wgsl"]);
    throw new Error(`Compilation failed:\n${stderr}`);
  }
  return { basePath, silkPath };
}

Deno.test("wraps wasm export for js", async () => {
  const silkCode = `
    (export wasm) (wrap js) add_one := (x: i32) => (
        x + 1
    );
    (export wasm) (wrap js) sum_pair := {x: i32, y: i32} => (
        x + y
    );
    {}
  `;

  const { basePath } = await compileBaseOrThrow(silkCode, "wrap_js");
  const jsPath = basePath + ".js";
  try {
    const module = await importJsModule(jsPath);
    assertEquals(await module.add_one(41), 42);
    assertEquals(await module.sum_pair({ x: 10, y: 32 }), 42);
  } finally {
    await cleanupBase(basePath, [".silk", ".js"]);
  }
});

Deno.test("wraps wasm globals for js", async () => {
  const silkCode = `
    (export wasm) (wrap js) answer: i32 := 42;
    {}
  `;

  const { basePath } = await compileBaseOrThrow(silkCode, "wrap_js_global");
  const jsPath = basePath + ".js";
  try {
    const module = await importJsModule(jsPath);
    assertEquals(await module.answer(), 42);
  } finally {
    await cleanupBase(basePath, [".silk", ".js"]);
  }
});

Deno.test("wraps js export for wasm", async () => {
  const silkCode = `
    (export js) (wrap wasm) add_one := (x: i32) => (
        x + 1
    );
    {}
  `;

  const { basePath } = await compileBaseOrThrow(silkCode, "wrap_wasm");
  const jsPath = basePath + ".js";
  const wasmPath = basePath + ".wasm";
  try {
    const module = await importJsModule(jsPath);
    const imports = module.__silk_wasm_imports
      ? module.__silk_wasm_imports()
      : {};
    const exports = (await loadWasm(wasmPath, imports))
      .exports as { add_one: (value: number) => number };
    assertEquals(exports.add_one(41), 42);
  } finally {
    await cleanupBase(basePath, [".silk", ".js", ".wasm"]);
  }
});

Deno.test("rejects wgsl wrap target", async () => {
  const silkCode = `
    (export wasm) (wrap wgsl) bad := {} => (
        1
    );
    {}
  `;

  const { basePath, code, stderr } = await compileToBase(
    silkCode,
    "wrap_invalid",
  );
  await cleanupBase(basePath, [".silk", ".js", ".wasm", ".wgsl"]);
  assertEquals(code, 1);
  assertStringIncludes(stderr, "wrap annotation does not support wgsl target");
});
