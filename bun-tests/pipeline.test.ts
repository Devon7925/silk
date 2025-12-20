import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "pipeline.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "pipeline.silk");
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

test("pipeline forwards values into functions", async () => {
    const silkCode = `
    (export wasm) pipe_simple := (value: i32) => (
        increment := (x: i32) => (
            x + 1
        );

        value |> increment
    );
    `;

    const exports = await compileAndLoad(silkCode);
    expect(exports.pipe_simple(41)).toBe(42);
}, TEST_TIMEOUT_MS);

test("pipeline composes multiple functions", async () => {
    const silkCode = `
    (export wasm) pipe_chain := (value: i32) => (
        increment := (x: i32) => (
            x + 1
        );
        double := (x: i32) => (
            x * 2
        );

        value |> increment |> double
    );
    `;

    const exports = await compileAndLoad(silkCode);
    expect(exports.pipe_chain(5)).toBe(12);
}, TEST_TIMEOUT_MS);

test("pipeline accepts inline function literals", async () => {
    const silkCode = `
    (export wasm) pipe_literal := (value: i32) => (
        value |> ((y: i32) => (
            y * y
        ))
    );
    `;

    const exports = await compileAndLoad(silkCode);
    expect(exports.pipe_literal(6)).toBe(36);
}, TEST_TIMEOUT_MS);

afterAll(() => {
    try {
        unlinkSync(TEMP_SILK);
    } catch (e) { }
    try {
        unlinkSync(TEMP_WASM);
    } catch (e) { }
});
