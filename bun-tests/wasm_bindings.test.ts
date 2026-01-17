import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "temp_bindings.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "temp_bindings.silk");
const TEST_TIMEOUT_MS = 20000;

async function compileToWasm(silkCode: string) {
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

    return Bun.file(TEMP_WASM).arrayBuffer();
}

async function compileAndLoad(silkCode: string) {
    const wasmBuffer = await compileToWasm(silkCode);
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

test("supports boxed values in wasm exports", async () => {
    const silkCode = `
    Point := { x = Box(i32), y = i32 };
    (export wasm) sum_boxed := {} => (
        boxed: Box(i32) := 10;
        p: Point := { x = 7, y = 5 };
        boxed + p.x + p.y
    );
    {}
  `;

    const { sum_boxed } = await compileAndLoad(silkCode);
    expect(sum_boxed()).toBe(22);
}, TEST_TIMEOUT_MS);

test("rejects runtime box allocations in wasm exports", async () => {
    const silkCode = `
    (export wasm) bad_box := (x: i32) => (
        boxed: Box(i32) := x;
        boxed + 1
    );
    {}
  `;

    writeFileSync(TEMP_SILK, silkCode);
    const proc = Bun.spawn(["cargo", "run", "--", TEMP_SILK, "-o", TEMP_WASM], {
        cwd: ROOT_DIR,
        stdin: "ignore",
        stderr: "pipe",
        stdout: "ignore",
        env: process.env,
    });
    const exitCode = await proc.exited;
    expect(exitCode).toBe(1);
    const stderr = await new Response(proc.stderr).text();
    expect(stderr).toContain("Box values must be compile-time constants");
}, TEST_TIMEOUT_MS);

test("exports boxed globals as distinct memories", async () => {
    const silkCode = `
    (export wasm) box_a: Box(i32) := 11;
    (export wasm) box_b: Box(i32) := 42;
    {}
  `;

    const wasmBuffer = await compileToWasm(silkCode);
    // Hack: Bun doesn't support multi-memory yet, so we spawn Node to validate exports.
    const nodeScript = `
        const fs = require("fs");
        const bytes = fs.readFileSync(process.argv[1]);
        WebAssembly.instantiate(bytes).then(({ instance }) => {
            const { box_a, box_b } = instance.exports;
            if (!(box_a instanceof WebAssembly.Memory) || !(box_b instanceof WebAssembly.Memory)) {
                throw new Error("Expected memory exports for box_a and box_b");
            }
            if (box_a.buffer === box_b.buffer) {
                throw new Error("Expected distinct memory buffers");
            }
            const viewA = new DataView(box_a.buffer);
            const viewB = new DataView(box_b.buffer);
            if (viewA.getInt32(0, true) !== 11) {
                throw new Error("box_a value mismatch");
            }
            if (viewB.getInt32(0, true) !== 42) {
                throw new Error("box_b value mismatch");
            }
        }).catch((err) => {
            console.error(err);
            process.exit(1);
        });
    `;
    const proc = Bun.spawn(["node", "-e", nodeScript, TEMP_WASM], {
        cwd: ROOT_DIR,
        stderr: "pipe",
    });
    const exitCode = await proc.exited;
    if (exitCode !== 0) {
        const stderr = await new Response(proc.stderr).text();
        console.error(stderr);
    }
    expect(exitCode).toBe(0);
}, TEST_TIMEOUT_MS);

afterAll(() => {
    try {
        unlinkSync(TEMP_SILK);
    } catch (e) { }
    try {
        unlinkSync(TEMP_WASM);
    } catch (e) { }
});
