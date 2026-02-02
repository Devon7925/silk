import { assertEquals } from "@std/asserts";
import {
  cleanup,
  cleanupBase,
  compileToBase,
  importJsModule,
  loadWasm,
  runCommand,
} from "./test_helpers.ts";

const HAS_GPU = Boolean(await navigator.gpu?.requestAdapter());

async function compileBaseOrThrow(silkCode: string, prefix: string) {
  const { basePath, code, stderr } = await compileToBase(silkCode, prefix);
  if (code !== 0) {
    await cleanupBase(basePath, [".silk", ".js", ".wasm", ".wgsl"]);
    throw new Error(`Compilation failed:\n${stderr}`);
  }
  return basePath;
}

async function runWgslJsWrapper(
  jsPath: string,
  fnName: string,
  arg: unknown,
) {
  const runnerPath = jsPath.replace(/\.js$/, ".runner.js");
  const moduleUrl = "file:///" + jsPath.replaceAll("\\", "/");
  const code = `
const mod = await import(${JSON.stringify(moduleUrl)});
const result = await mod.${fnName}(${JSON.stringify(arg)});
const normalized = result instanceof Int32Array ? Array.from(result) : result;
console.log(JSON.stringify(normalized));
`;
  await Deno.writeTextFile(runnerPath, code);
  try {
    const { code: exitCode, stdout, stderr } = await runCommand(
      "deno",
      ["run", "--allow-read", runnerPath],
      { stdout: "piped", stderr: "piped" },
    );
    if (exitCode !== 0) {
      throw new Error(stderr || stdout);
    }
    const output = stdout.trim();
    return output ? JSON.parse(output) : undefined;
  } finally {
    await cleanup([runnerPath]);
  }
}

Deno.test({
  name: "wraps wgsl export for js",
  ignore: !HAS_GPU,
  fn: async () => {
    const silkCode = `
    (export wgsl) (wrap js) add_one := (x: i32) => (
        x + 1
    );
    {}
  `;

    const basePath = await compileBaseOrThrow(silkCode, "wrap_wgsl_js");
    const jsPath = basePath + ".js";
    try {
      const result = await runWgslJsWrapper(jsPath, "add_one", 41);
      if (result !== 42) {
        throw new Error(`Expected 42, got ${result}`);
      }
    } finally {
      await cleanupBase(basePath, [".silk", ".js", ".wasm", ".wgsl"]);
    }
  },
  sanitizeOps: false,
  sanitizeResources: false,
});

Deno.test({
  name: "wraps wgsl export for js with struct io",
  ignore: !HAS_GPU,
  fn: async () => {
    const silkCode = `
    Point: type := { x = i32, y = i32 };
    Result: type := { sum = i32, diff = i32 };
    (export wgsl) (wrap js) sum_pair := (p: Point) => (
        { sum = p.x + p.y, diff = p.x - p.y }
    );
    {}
  `;

    const basePath = await compileBaseOrThrow(silkCode, "wrap_wgsl_js_struct");
    const jsPath = basePath + ".js";
    try {
      const result = await runWgslJsWrapper(jsPath, "sum_pair", { x: 40, y: 2 });
      assertEquals(result, { sum: 42, diff: 38 });
    } finally {
      await cleanupBase(basePath, [".silk", ".js", ".wasm", ".wgsl"]);
    }
  },
  sanitizeOps: false,
  sanitizeResources: false,
});

Deno.test({
  name: "wraps wgsl export for js with array io",
  ignore: !HAS_GPU,
  fn: async () => {
    const silkCode = `
    (export wgsl) (wrap js) offset_vec := (values: {i32, i32, i32}) => (
        { values(0) + 1, values(1) + 2, values(2) + 3 }
    );
    {}
  `;

    const basePath = await compileBaseOrThrow(silkCode, "wrap_wgsl_js_array");
    const jsPath = basePath + ".js";
    try {
      const result = await runWgslJsWrapper(jsPath, "offset_vec", [1, 2, 3]);
      assertEquals(result, [2, 4, 6]);
    } finally {
      await cleanupBase(basePath, [".silk", ".js", ".wasm", ".wgsl"]);
    }
  },
  sanitizeOps: false,
  sanitizeResources: false,
});

Deno.test({
  name: "wraps wgsl export for wasm",
  ignore: !HAS_GPU,
  fn: async () => {
    const silkCode = `
    (export wgsl) (wrap wasm) add_one := (x: i32) => (
        x + 1
    );
    {}
  `;

    const basePath = await compileBaseOrThrow(silkCode, "wrap_wgsl_wasm");
    const jsPath = basePath + ".js";
    const wasmPath = basePath + ".wasm";
    try {
      const module = await importJsModule(jsPath);
      const imports = module.__silk_wasm_imports
        ? module.__silk_wasm_imports()
        : {};
      const exports = (await loadWasm(wasmPath, imports))
        .exports as { add_one: (value: number) => number };
      const result = exports.add_one(41);
      if (result !== 42) {
        throw new Error(`Expected 42, got ${result}`);
      }
    } finally {
      await cleanupBase(basePath, [".silk", ".js", ".wasm", ".wgsl"]);
    }
  },
  sanitizeOps: false,
  sanitizeResources: false,
});
