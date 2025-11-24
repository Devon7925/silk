import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "return_statement.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "return_statement.silk");
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

test("return exits a function early", async () => {
    const silkCode = `
    let export(wasm) early = fn(x: i32) -> i32 (
        if x > 0 (
            return x + 2;
        ) else (
            x - 100
        )
    );
    {};
    `;

    const exports = await compileAndLoad(silkCode);
    expect(exports.early(10)).toBe(12);
    expect(exports.early(-5)).toBe(-105);
}, TEST_TIMEOUT_MS);

test("return inside let binding expressions still exits early", async () => {
    const silkCode = `
    let export(wasm) let_return = fn(x: i32) -> i32 (
        let y = (
            if x == 0 (
                return 99;
            ) else (
                x * 2
            )
        );
        y + 1
    );
    {};
    `;

    const exports = await compileAndLoad(silkCode);
    expect(exports.let_return(0)).toBe(99);
    expect(exports.let_return(3)).toBe(7);
}, TEST_TIMEOUT_MS);

test("return doesn't break branch type matching", async () => {
    const silkCode = `
    let export(wasm) branch_types = fn(flag: bool) -> i32 (
        let value = if flag (
            return 5;
        ) else (
            42
        );
        value + 1
    );
    {};
    `;

    const exports = await compileAndLoad(silkCode);
    expect(exports.branch_types(true)).toBe(5);
    expect(exports.branch_types(false)).toBe(43);
}, TEST_TIMEOUT_MS);

test("returning a complex expression works", async () => {
    const silkCode = `
    let export(wasm) complex = fn({ x: i32, y: i32 }) -> i32 (
        return (x + y) * (y - x) + (x * x) - (y / 2);
    );
    {};
    `;

    const exports = await compileAndLoad(silkCode);
    expect(exports.complex(6, 4)).toBe(14);
    expect(exports.complex(2, 10)).toBe(95);
}, TEST_TIMEOUT_MS);

afterAll(() => {
    try {
        unlinkSync(TEMP_SILK);
    } catch (e) {}
    try {
        unlinkSync(TEMP_WASM);
    } catch (e) {}
});

