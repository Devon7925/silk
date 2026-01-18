import { join, fromFileUrl, toFileUrl } from "https://deno.land/std/path/mod.ts";

const ROOT_DIR = fromFileUrl(new URL("..", import.meta.url));
const FIXTURES_DIR = join(ROOT_DIR, "fixtures");
const TEMP_SILK = join(FIXTURES_DIR, "temp_wrap_wgsl.silk");
const HAS_GPU = Boolean(await navigator.gpu?.requestAdapter());

async function compileToBase(silkCode: string, baseName: string) {
    const outputBase = join(FIXTURES_DIR, baseName);
    await Deno.writeTextFile(TEMP_SILK, silkCode);
    const cmd = new Deno.Command("cargo", {
        args: ["run", "--", TEMP_SILK, "-o", outputBase],
        cwd: ROOT_DIR,
        stderr: "piped",
        stdout: "null",
    });
    const result = await cmd.output();
    const stderr = new TextDecoder().decode(result.stderr);
    return { exitCode: result.code, stderr, outputBase };
}

async function cleanupArtifacts(basePath: string) {
    for (const suffix of [".js", ".wasm", ".wgsl"]) {
        try {
            await Deno.remove(basePath + suffix);
        } catch (err) {
            if (!(err instanceof Deno.errors.NotFound)) {
                throw err;
            }
        }
    }
    await Deno.remove(TEMP_SILK);
}

async function importJsModule(jsPath: string) {
    const moduleUrl = toFileUrl(jsPath).href;
    return await import(moduleUrl);
}

async function loadWasm(wasmPath: string, imports: Record<string, unknown>) {
    const bytes = await Deno.readFile(wasmPath);
    const { instance } = await WebAssembly.instantiate(
        bytes,
        imports as WebAssembly.Imports,
    );
    return instance.exports as any;
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

        const baseName = `wrap_wgsl_js_${Date.now()}_${Math.random().toString().slice(2)}`;
        const { exitCode, stderr, outputBase } = await compileToBase(silkCode, baseName);
        if (exitCode !== 0) {
            throw new Error(`Compilation failed:\n${stderr}`);
        }

        const jsPath = outputBase + ".js";
        try {
            const module = await importJsModule(jsPath);
            const result = await module.add_one(41);
            if (result !== 42) {
                throw new Error(`Expected 42, got ${result}`);
            }
        } finally {
            await cleanupArtifacts(outputBase);
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

        const baseName = `wrap_wgsl_wasm_${Date.now()}_${Math.random().toString().slice(2)}`;
        const { exitCode, stderr, outputBase } = await compileToBase(silkCode, baseName);
        if (exitCode !== 0) {
            throw new Error(`Compilation failed:\n${stderr}`);
        }

        const jsPath = outputBase + ".js";
        const wasmPath = outputBase + ".wasm";
        try {
            const module = await importJsModule(jsPath);
            const imports = module.__silk_wasm_imports ? module.__silk_wasm_imports() : {};
            const exports = await loadWasm(wasmPath, imports);
            const result = exports.add_one(41);
            if (result !== 42) {
                throw new Error(`Expected 42, got ${result}`);
            }
        } finally {
            await cleanupArtifacts(outputBase);
        }
    },
    sanitizeOps: false,
    sanitizeResources: false,
});
