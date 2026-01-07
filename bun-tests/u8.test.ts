import { test, expect } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "temp_u8.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "temp_u8.silk");

const TEST_TIMEOUT_MS = 20000;

async function compileAndRun(code: string, callback: (instance: WebAssembly.Instance) => Promise<void>) {
    writeFileSync(TEMP_SILK, code);
    const proc = Bun.spawn(["cargo", "run", "--", TEMP_SILK, "-o", TEMP_WASM], {
        cwd: ROOT_DIR,
        stderr: "pipe",
    });
    const exitCode = await proc.exited;
    if (exitCode !== 0) {
        const stderr = await new Response(proc.stderr).text();
        console.error(stderr);
    }
    expect(exitCode).toBe(0);
    const wasmBuffer = await Bun.file(TEMP_WASM).arrayBuffer();
    const { instance } = await WebAssembly.instantiate(wasmBuffer);
    await callback(instance);
}

test("u8 values", async () => {
    const code = `
    (export wasm) add_one := (value: u8) => ( value + 1 );
    (export wasm) add_pair := (left: u8) => ( (right: u8) => left + right );
    {}
    `;

    await compileAndRun(code, async (instance) => {
        const exports = instance.exports as any;
        expect(exports.add_one(41)).toBe(42);
        expect(exports.add_pair(12)(34)).toBe(46);
    });

    try { unlinkSync(TEMP_SILK); unlinkSync(TEMP_WASM); } catch (e) { }
}, TEST_TIMEOUT_MS);
