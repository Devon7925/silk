import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "edge_cases.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "edge_cases.silk");
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

test("shadowing in block", async () => {
    const silkCode = `
    (export wasm) shadow_test := (x: i32) => (
        y := 10;
        (
            y := 20;
            x + y
        )
    );
    {}
    `;
    const exports = await compileAndLoad(silkCode);
    // x + 20. If x=5, result=25.
    expect(exports.shadow_test(5)).toBe(25);
}, TEST_TIMEOUT_MS);

test("multiple exports", async () => {
    const silkCode = `
    (export wasm) add := (x: i32) => ( x + 1 );
    (export wasm) sub := (x: i32) => ( x - 1 );
    {}
    `;
    const exports = await compileAndLoad(silkCode);
    expect(exports.add(10)).toBe(11);
    expect(exports.sub(10)).toBe(9);
}, TEST_TIMEOUT_MS);

test("arithmetic edge cases", async () => {
    const silkCode = `
    (export wasm) div_test := (x: i32) => ( 100 / x );
    {}
    `;
    const exports = await compileAndLoad(silkCode);
    expect(exports.div_test(2)).toBe(50);
    expect(() => exports.div_test(0)).toThrow(); // Division by zero should trap
}, TEST_TIMEOUT_MS);

test("deeply nested bindings", async () => {
    const silkCode = `
    (export wasm) nested_test := (x: i32) => (
        a := x + 1;
        b := a * 2;
        c := b + a; 
        c
    );
    {}
    `;
    // x=2
    // a = 3
    // b = 6
    // c = 6 + 3 = 9
    const exports = await compileAndLoad(silkCode);
    expect(exports.nested_test(2)).toBe(9);
}, TEST_TIMEOUT_MS);

test("if expressions evaluate and type check", async () => {
    const silkCode = `
    (export wasm) choose := (flag: bool) => (
        if flag then 10 else 20
    );
    (export wasm) ladder := (flag: bool) => (
        if false then 1 else if flag then 2 else 3
    );
    {} 
    `;
    const exports = await compileAndLoad(silkCode);
    expect(exports.choose(1)).toBe(10);
    expect(exports.choose(0)).toBe(20);
    expect(exports.ladder(1)).toBe(2);
    expect(exports.ladder(0)).toBe(3);
}, TEST_TIMEOUT_MS);

test("if branch type mismatches are reported", async () => {
    const silkCode = `
    (export wasm) bad := {} => (
        if true then ( 1; ) else ( 2 )
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
    expect(stderr).toContain("Type mismatch between if branches");
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