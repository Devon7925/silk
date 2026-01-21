import { assertEquals } from "https://deno.land/std/assert/assert_equals.ts";
import { cleanupBase, compileToBase, importJsModule, loadWasm } from "./test_helpers.ts";

const HAS_GPU = Boolean(await navigator.gpu?.requestAdapter());

async function compileBaseOrThrow(silkCode: string, prefix: string) {
    const { basePath, code, stderr } = await compileToBase(silkCode, prefix);
    if (code !== 0) {
        await cleanupBase(basePath, [".silk", ".js", ".wasm", ".wgsl"]);
        throw new Error(`Compilation failed:\n${stderr}`);
    }
    return basePath;
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
            const module = await importJsModule(jsPath);
            const result = await module.add_one(41);
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
            const module = await importJsModule(jsPath);
            const result = await module.sum_pair({ x: 40, y: 2 });
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
            const module = await importJsModule(jsPath);
            const result = await module.offset_vec([1, 2, 3]);
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
            const imports = module.__silk_wasm_imports ? module.__silk_wasm_imports() : {};
            const exports = (await loadWasm(wasmPath, imports)).exports as any;
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
