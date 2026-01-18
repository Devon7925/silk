import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync, existsSync, readdirSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_SILK = join(FIXTURES_DIR, "temp_wrap_bindings.silk");
const TEST_TIMEOUT_MS = 20000;

async function compileToBase(silkCode: string, baseName: string) {
    const outputBase = join(FIXTURES_DIR, baseName);
    writeFileSync(TEMP_SILK, silkCode);

    const proc = Bun.spawn(["cargo", "run", "--", TEMP_SILK, "-o", outputBase], {
        cwd: ROOT_DIR,
        stderr: "pipe",
        stdin: "ignore",
        stdout: "ignore",
        env: process.env,
    });

    const exitCode = await proc.exited;
    const stderr = await new Response(proc.stderr).text();
    return { exitCode, stderr, outputBase };
}

async function importJsModule(jsPath: string) {
    const module = await import(jsPath);
    return module as any;
}

async function loadWasm(wasmPath: string, imports: any) {
    const wasmBuffer = await Bun.file(wasmPath).arrayBuffer();
    const { instance } = await WebAssembly.instantiate(wasmBuffer, imports);
    return instance.exports as any;
}

test("wraps wasm export for js", async () => {
    const silkCode = `
    (export wasm) (wrap js) add_one := (x: i32) => (
        x + 1
    );
    (export wasm) (wrap js) sum_pair := {x: i32, y: i32} => (
        x + y
    );
    {}
  `;

    const baseName = `wrap_js_${Date.now()}_${Math.random().toString().slice(2)}`;
    const { exitCode, stderr, outputBase } = await compileToBase(silkCode, baseName);
    if (exitCode !== 0) {
        throw new Error(`Compilation failed:\n${stderr}`);
    }

    const jsPath = outputBase + ".js";
    const module = await importJsModule(jsPath);
    expect(await module.add_one(41)).toBe(42);
    expect(await module.sum_pair({ x: 10, y: 32 })).toBe(42);
}, TEST_TIMEOUT_MS);

test("wraps wasm globals for js", async () => {
    const silkCode = `
    (export wasm) (wrap js) answer: i32 := 42;
    {}
  `;

    const baseName = `wrap_js_global_${Date.now()}_${Math.random().toString().slice(2)}`;
    const { exitCode, stderr, outputBase } = await compileToBase(silkCode, baseName);
    if (exitCode !== 0) {
        throw new Error(`Compilation failed:\n${stderr}`);
    }

    const jsPath = outputBase + ".js";
    const module = await importJsModule(jsPath);
    expect(await module.answer()).toBe(42);
}, TEST_TIMEOUT_MS);

test("wraps js export for wasm", async () => {
    const silkCode = `
    (export js) (wrap wasm) add_one := (x: i32) => (
        x + 1
    );
    {}
  `;

    const baseName = `wrap_wasm_${Date.now()}_${Math.random().toString().slice(2)}`;
    const { exitCode, stderr, outputBase } = await compileToBase(silkCode, baseName);
    if (exitCode !== 0) {
        throw new Error(`Compilation failed:\n${stderr}`);
    }

    const jsPath = outputBase + ".js";
    const wasmPath = outputBase + ".wasm";
    const module = await importJsModule(jsPath);
    const imports = module.__silk_wasm_imports ? module.__silk_wasm_imports() : {};
    const exports = await loadWasm(wasmPath, imports);
    expect(exports.add_one(41)).toBe(42);
}, TEST_TIMEOUT_MS);

test("rejects wgsl wrap target", async () => {
    const silkCode = `
    (export wasm) (wrap wgsl) bad := {} => (
        1
    );
    {}
  `;

    const baseName = `wrap_invalid_${Date.now()}_${Math.random().toString().slice(2)}`;
    const { exitCode, stderr } = await compileToBase(silkCode, baseName);
    expect(exitCode).toBe(1);
    expect(stderr).toContain("wrap annotation does not support wgsl target");
}, TEST_TIMEOUT_MS);

afterAll(() => {
    try {
        unlinkSync(TEMP_SILK);
    } catch (e) {}
    if (!existsSync(FIXTURES_DIR)) {
        return;
    }
    for (const entry of ["wrap_js", "wrap_js_global", "wrap_wasm", "wrap_invalid"]) {
        // Clean up any leftover artifacts from failed tests (best-effort).
        try {
            const pattern = entry;
            const files = readdirSync(FIXTURES_DIR);
            for (const file of files) {
                if (!file.startsWith(pattern)) {
                    continue;
                }
                if (file.endsWith(".js") || file.endsWith(".wasm") || file.endsWith(".wgsl")) {
                    unlinkSync(join(FIXTURES_DIR, file));
                }
            }
        } catch (e) {}
    }
});
