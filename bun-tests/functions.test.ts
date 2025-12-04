import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "higher_order_functions.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "higher_order_functions.silk");
const TEST_TIMEOUT_MS = 20000;

async function compileAndLoad(silkCode: string) {
    writeFileSync(TEMP_SILK, silkCode);

    const proc = Bun.spawn(["cargo", "run", "--", TEMP_SILK, "-o", TEMP_WASM], {
        cwd: ROOT_DIR,
        stderr: "pipe",
    });

    const exitCode = await proc.exited;
    if (exitCode !== 0) {
        const stderr = await new Response(proc.stderr).text();
        throw new Error(`Compilation failed:\n${stderr}`);
    }

    const wasmBuffer = await Bun.file(TEMP_WASM).arrayBuffer();
    const { instance } = await WebAssembly.instantiate(wasmBuffer);
    return instance.exports as any;
}

test("functions can be passed as arguments", async () => {
    const silkCode = `
    (export wasm) apply_increment := (x: i32) => (
        apply := { func = func: (i32 -> i32), value = value: i32 } => (
            func value
        );

        increment := (y: i32) => (
            y + 1
        );

        apply { func = increment, value = x }
    );
    {};
    `;

    const exports = await compileAndLoad(silkCode);
    expect(exports.apply_increment(41)).toBe(42);
}, TEST_TIMEOUT_MS);

test("functions can be returned and invoked", async () => {
    const silkCode = `
    (export wasm) apply_offset := (offset: i32) => (
        make_adder := (base: i32) => (
            (y: i32) => (
                base + y
            )
        );

        add_three := make_adder 3;
        add_three offset
    );
    {};
    `;

    const exports = await compileAndLoad(silkCode);
    expect(exports.apply_offset(7)).toBe(10);
}, TEST_TIMEOUT_MS);

// Cleanup after the test suite to avoid leaving temporary files behind.
afterAll(() => {
    try {
        unlinkSync(TEMP_SILK);
    } catch (e) { }
    try {
        unlinkSync(TEMP_WASM);
    } catch (e) { }
});
