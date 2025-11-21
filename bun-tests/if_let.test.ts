import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "if_let.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "if_let.silk");
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

test("let as expression", async () => {
    const silkCode = `
    let export(wasm) let_as_expr = fn(x: i32) -> bool (
        let y = x // binding succeeds, so it resolves to true
    );
    {}
    `;
    const exports = await compileAndLoad(silkCode);
    expect(exports.let_as_expr(5)).toBe(1);
}, TEST_TIMEOUT_MS);

test("let in if condition", async () => {
    const silkCode = `
    let export(wasm) let_as_expr = fn(x: i32) -> i32 (
        if let y = x (
            y
        ) else (
            0
        )
    );
    {}
    `;
    const exports = await compileAndLoad(silkCode);
    expect(exports.let_as_expr(5)).toBe(5);
}, TEST_TIMEOUT_MS);

test("refutable let in if condition", async () => {
    const silkCode = `
    let export(wasm) let_as_expr = fn(x: i32) -> i32 (
        if let { 5, y } = { x, x + 5 } (
            y
        ) else (
            0
        )
    );
    {}
    `;
    const exports = await compileAndLoad(silkCode);
    expect(exports.let_as_expr(5)).toBe(10);
    expect(exports.let_as_expr(6)).toBe(0);
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