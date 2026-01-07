import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(ROOT_DIR, "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "arrays_temp.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "arrays_temp.silk");
const TEST_TIMEOUT_MS = 20000;

async function compileToInstance(code: string) {
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
    return instance;
}

test("allows indexing mutable arrays with dynamic indices", async () => {
    const silkCode = `
(export wasm) array_value := (idx: i32) => (
    mut values := {10, 20, 30};
    values(idx) = values(idx) + 7;
    values(idx)
);
`;

    const instance = await compileToInstance(silkCode);
    const { array_value } = instance.exports as any;

    expect(array_value(1)).toBe(27);
}, TEST_TIMEOUT_MS);

test("supports constant index reads", async () => {
    const silkCode = `
(export wasm) read_const := {} => (
    values := {4, 8, 15, 16, 23, 42};
    values(3)
);
`;

    const instance = await compileToInstance(silkCode);
    const { read_const } = instance.exports as any;

    expect(read_const()).toBe(16);
}, TEST_TIMEOUT_MS);

test("allows arrays of tuples", async () => {
    const silkCode = `
(export wasm) tuple_at := (idx: i32) => (
    pairs := {{1, 2}, {3, 4}, {5, 6}};
    pairs(idx).0
);
`;

    const instance = await compileToInstance(silkCode);
    const { tuple_at } = instance.exports as any;

    expect(tuple_at(2)).toBe(5);
}, TEST_TIMEOUT_MS);

test("supports nested array indexing", async () => {
    const silkCode = `
(export wasm) matrix_get := {row = row: i32, col = col: i32} => (
    matrix := {{1, 2}, {3, 4}};
    matrix(row)(col)
);
`;

    const instance = await compileToInstance(silkCode);
    const { matrix_get } = instance.exports as any;

    expect(matrix_get(1, 0)).toBe(3);
}, TEST_TIMEOUT_MS);

afterAll(() => {
    try {
        unlinkSync(TEMP_SILK);
    } catch (e) { }
    try {
        unlinkSync(TEMP_WASM);
    } catch (e) { }
});
