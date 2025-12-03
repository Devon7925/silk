import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "mut_bindings.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "mut_bindings.silk");
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

test("supports mutable assignments in wasm exports", async () => {
    const silkCode = `
    let (export wasm) increment_twice = (x: i32) => (
        let mut total = x;
        total = total + 1;
        total = total + 1;
        total
    );
    {}
  `;

    const { increment_twice } = await compileAndLoad(silkCode);
    expect(increment_twice(10)).toBe(12);
}, TEST_TIMEOUT_MS);

test("propagates mutability through destructured wasm bindings", async () => {
    const silkCode = `
    let (export wasm) destructure_mut = {} => (
        let mut { first = a, second = b } = { first = 3, second = 4 };
        a = a + b;
        a
    );
    {}
  `;

    const { destructure_mut } = await compileAndLoad(silkCode);
    expect(destructure_mut()).toBe(7);
}, TEST_TIMEOUT_MS);

test("struct props can be mutated", async () => {
    const silkCode = `
    let (export wasm) destructure_mut = {} => (
        let mut foo = { first = 3, second = 4 };
        foo.first = foo.first + foo.second;
        foo.first
    );
    {}
  `;

    const { destructure_mut } = await compileAndLoad(silkCode);
    expect(destructure_mut()).toBe(7);
}, TEST_TIMEOUT_MS);

test("nested dependent struct and tuple props can be mutated", async () => {
    const silkCode = `
    let (export wasm) destructure_mut = (bar: i32) => (
        let mut foo = { first = {bar, 3}, second = 4 };
        foo.first.0 = foo.first.0 + foo.second;
        foo.first.0
    );
    {}
  `;

    const { destructure_mut } = await compileAndLoad(silkCode);
    expect(destructure_mut(3)).toBe(7);
}, TEST_TIMEOUT_MS);

test("partial mutability in destructured wasm bindings", async () => {
    const silkCode = `
    let (export wasm) destructure_mut = {} => (
        let { mut a, b } = { 3, 4 };
        a = a + b;
        a
    );
    {}
  `;

    const { destructure_mut } = await compileAndLoad(silkCode);
    expect(destructure_mut()).toBe(7);
}, TEST_TIMEOUT_MS);

test("allows struct field updates via rebinding", async () => {
    const silkCode = `
    let (export wasm) update_struct = {} => (
        let mut pair = { first = 2, second = 5 };
        pair = { first = pair.first * 2, second = pair.second };
        pair.first + pair.second
    );
    {}
  `;

    const { update_struct } = await compileAndLoad(silkCode);
    expect(update_struct()).toBe(9);
}, TEST_TIMEOUT_MS);

afterAll(() => {
    try {
        unlinkSync(TEMP_SILK);
    } catch (e) { }
    try {
        unlinkSync(TEMP_WASM);
    } catch (e) { }
});
