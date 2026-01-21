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
