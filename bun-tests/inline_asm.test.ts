import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync, existsSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_SILK = join(FIXTURES_DIR, "inline_asm.silk");
const TEST_TIMEOUT_MS = 20000;

async function compileToBase(silkCode: string, outputBase: string) {
    writeFileSync(TEMP_SILK, silkCode);

    const proc = Bun.spawn(["cargo", "run", "--", TEMP_SILK, "-o", outputBase], {
        cwd: ROOT_DIR,
        stderr: "pipe",
        stdin: "ignore",
        stdout: "ignore",
        env: process.env,
    });

    const exitCode = await proc.exited;
    if (exitCode !== 0) {
        const stderr = await new Response(proc.stderr).text();
        throw new Error(`Compilation failed:\n${stderr}`);
    }
}

function cleanupPaths(paths: string[]) {
    for (const path of paths) {
        try {
            if (existsSync(path)) unlinkSync(path);
        } catch (e) {}
    }
}

test("inline asm works in js exports", async () => {
    const id = Date.now() + "_" + Math.random().toString().slice(2);
    const outputBase = join(FIXTURES_DIR, `inline_asm_js_${id}`);
    const jsPath = outputBase + ".js";

    const silkCode = `
    (target js) (export js) answer := {} => (
        asm(js)("1 + 41")
    );
    `;

    await compileToBase(silkCode, outputBase);
    const module = await import(jsPath);
    expect(module.answer()).toBe(42);
    cleanupPaths([jsPath]);
}, TEST_TIMEOUT_MS);

test("inline asm works in wasm exports", async () => {
    const id = Date.now() + "_" + Math.random().toString().slice(2);
    const outputBase = join(FIXTURES_DIR, `inline_asm_wasm_${id}`);
    const wasmPath = outputBase + ".wasm";

    const silkCode = `
    (target wasm) (export wasm) answer := {} => (
        asm(wasm)("i32.const 42")
    );
    `;

    await compileToBase(silkCode, outputBase);
    const wasmBuffer = await Bun.file(wasmPath).arrayBuffer();
    const { instance } = await WebAssembly.instantiate(wasmBuffer);
    expect((instance.exports as any).answer()).toBe(42);
    cleanupPaths([wasmPath]);
}, TEST_TIMEOUT_MS);

test("inline asm supports multi-instruction arithmetic in wasm exports", async () => {
    const id = Date.now() + "_" + Math.random().toString().slice(2);
    const outputBase = join(FIXTURES_DIR, `inline_asm_wasm_math_${id}`);
    const wasmPath = outputBase + ".wasm";

    const silkCode = `
    (target wasm) (export wasm) compute := {} => (
        asm(wasm)("i32.const 7; i32.const 5; i32.add; i32.const 3; i32.mul")
    );
    `;

    await compileToBase(silkCode, outputBase);
    const wasmBuffer = await Bun.file(wasmPath).arrayBuffer();
    const { instance } = await WebAssembly.instantiate(wasmBuffer);
    expect((instance.exports as any).compute()).toBe(36);
    cleanupPaths([wasmPath]);
}, TEST_TIMEOUT_MS);

test("inline asm supports comparisons in wasm exports", async () => {
    const id = Date.now() + "_" + Math.random().toString().slice(2);
    const outputBase = join(FIXTURES_DIR, `inline_asm_wasm_cmp_${id}`);
    const wasmPath = outputBase + ".wasm";

    const silkCode = `
    (target wasm) (export wasm) is_seven := {} => (
        asm(wasm)("i32.const 10; i32.const 3; i32.sub; i32.const 7; i32.eq")
    );
    `;

    await compileToBase(silkCode, outputBase);
    const wasmBuffer = await Bun.file(wasmPath).arrayBuffer();
    const { instance } = await WebAssembly.instantiate(wasmBuffer);
    expect((instance.exports as any).is_seven()).toBe(1);
    cleanupPaths([wasmPath]);
}, TEST_TIMEOUT_MS);

test("inline asm can participate in argument arithmetic", async () => {
    const id = Date.now() + "_" + Math.random().toString().slice(2);
    const outputBase = join(FIXTURES_DIR, `inline_asm_wasm_arg_${id}`);
    const wasmPath = outputBase + ".wasm";

    const silkCode = `
    (target wasm) (export wasm) add_one := (x: i32) => (
        x + asm(wasm)("i32.const 1")
    );
    `;

    await compileToBase(silkCode, outputBase);
    const wasmBuffer = await Bun.file(wasmPath).arrayBuffer();
    const { instance } = await WebAssembly.instantiate(wasmBuffer);
    expect((instance.exports as any).add_one(41)).toBe(42);
    cleanupPaths([wasmPath]);
}, TEST_TIMEOUT_MS);

test("inline asm supports local.get and local.set", async () => {
    const id = Date.now() + "_" + Math.random().toString().slice(2);
    const outputBase = join(FIXTURES_DIR, `inline_asm_wasm_local_${id}`);
    const wasmPath = outputBase + ".wasm";

    const silkCode = `
    (target wasm) (export wasm) add_one_asm := (x: i32) => (
        y := 0;
        asm(wasm)("local.get 0; i32.const 1; i32.add; local.set 1; local.get 1")
    );
    `;

    await compileToBase(silkCode, outputBase);
    const wasmBuffer = await Bun.file(wasmPath).arrayBuffer();
    const { instance } = await WebAssembly.instantiate(wasmBuffer);
    expect((instance.exports as any).add_one_asm(41)).toBe(42);
    cleanupPaths([wasmPath]);
}, TEST_TIMEOUT_MS);

test("js output can call wasm output from the same silk file", async () => {
    const id = Date.now() + "_" + Math.random().toString().slice(2);
    const outputBase = join(FIXTURES_DIR, `inline_asm_combo_${id}`);
    const jsPath = outputBase + ".js";
    const wasmPath = outputBase + ".wasm";
    const wasmPathLiteral = wasmPath
        .replaceAll("\\", "/")
        .replaceAll("'", "\\'");

    const silkCode = `
    (target wasm) (export wasm) wasm_answer := {} => (
        asm(wasm)("i32.const 42")
    );

    (target js) (export js) call_wasm := {} => (
        asm(js)("((async () => { if (!globalThis.__silk_wasm_instance) { const bytes = await Bun.file('${wasmPathLiteral}').arrayBuffer(); const { instance } = await WebAssembly.instantiate(bytes); globalThis.__silk_wasm_instance = instance; } return globalThis.__silk_wasm_instance.exports.wasm_answer(); })())")
    );
    `;

    await compileToBase(silkCode, outputBase);

    const module = await import(jsPath);
    expect(await module.call_wasm()).toBe(42);
    cleanupPaths([jsPath, wasmPath]);
}, TEST_TIMEOUT_MS);

afterAll(() => {
    cleanupPaths([TEMP_SILK]);
    try {
        delete (globalThis as any).__silk_wasm_instance;
    } catch (e) {}
});
