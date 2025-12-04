import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "temp_bindings.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "temp_bindings.silk");
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

test("compiles and runs wasm with bindings", async () => {
    const silkCode = `
    (export wasm) double_add := (x: i32) => (
        y := x * 2;
        y + y
    );
    {}
  `;

    const { double_add } = await compileAndLoad(silkCode);
    expect(double_add(5)).toBe(20); // (5 * 2) + (5 * 2) = 10 + 10 = 20
}, TEST_TIMEOUT_MS);

test("supports mutable assignments in wasm exports", async () => {
    const silkCode = `
    (export wasm) increment_twice := (x: i32) => (
        mut total := x;
        total = total + 1;
        total = total + 1;
        total
    );
    {}
  `;

    const { increment_twice } = await compileAndLoad(silkCode);
    expect(increment_twice(10)).toBe(12);
}, TEST_TIMEOUT_MS);

afterAll(() => {
    try {
        unlinkSync(TEMP_SILK);
    } catch (e) { }
    try {
        unlinkSync(TEMP_WASM);
    } catch (e) { }
});
