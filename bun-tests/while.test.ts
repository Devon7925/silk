import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(ROOT_DIR, "fixtures");
const TEMP_FILES = new Set<string>();
const TEST_TIMEOUT_MS = 20000;

async function compileAndLoad(silkCode: string, basename: string) {
    const wasmPath = join(FIXTURES_DIR, `${basename}.wasm`);
    const silkPath = join(FIXTURES_DIR, `${basename}.silk`);
    TEMP_FILES.add(wasmPath);
    TEMP_FILES.add(silkPath);

    writeFileSync(silkPath, silkCode);

    const proc = Bun.spawn(["cargo", "run", "--", silkPath, "-o", wasmPath], {
        cwd: ROOT_DIR,
        stderr: "pipe",
    });

    const exitCode = await proc.exited;
    if (exitCode !== 0) {
        const stderr = await new Response(proc.stderr).text();
        throw new Error(`Compilation failed:\n${stderr}`);
    }

    const wasmBuffer = await Bun.file(wasmPath).arrayBuffer();
    const { instance } = await WebAssembly.instantiate(wasmBuffer);
    return instance.exports as any;
}

test("while loops accumulate until reaching limit", async () => {
    const silkCode = `
    (export wasm) sum_until := (limit: i32) => (
        mut acc := 0;
        mut iter := 0;
        while iter < limit do (
            acc = acc + iter;
            iter = iter + 1;
        );
        acc
    );
    {};
    `;

    const { sum_until } = await compileAndLoad(silkCode, "while_accumulate");
    expect(sum_until(5)).toBe(10);
}, TEST_TIMEOUT_MS);

test("while loops stop when the guard is false", async () => {
    const silkCode = `
    (export wasm) decrement_to_zero := (start: i32) => (
        mut value := start;
        while value > 0 do (
            value = value - 1;
        );
        value
    );
    {};
    `;

    const { decrement_to_zero } = await compileAndLoad(silkCode, "while_decrement");
    expect(decrement_to_zero(0)).toBe(0);
    expect(decrement_to_zero(3)).toBe(0);
}, TEST_TIMEOUT_MS);

test("while with let expression stops when the pattern fails", async () => {
    const silkCode = `
    Option := enum { Some = i32, None = {} };
    (export wasm) sum_until_none := (limit: i32) => (
        mut iter := 0;
        mut acc := 0;
        while Option::Some(value) := (if iter < limit then Option::Some(iter) else Option::None) do (
            acc = acc + value;
            iter = iter + 1;
        );
        acc
    );
    {};
    `;

    const { sum_until_none } = await compileAndLoad(silkCode, "while_let_binding");
    expect(sum_until_none(0)).toBe(0);
    expect(sum_until_none(4)).toBe(6);
}, TEST_TIMEOUT_MS);

test("while break without an argument exits immediately", async () => {
    const silkCode = `
    (export wasm) break_without_value := (limit: i32) => (
        mut counter := 0;
        mut sum := 0;
        while counter < limit do (
            if counter == 2 then (
                break
            );
            sum = sum + counter;
            counter = counter + 1;
        );
        sum
    );
    {};
    `;

    const { break_without_value } = await compileAndLoad(silkCode, "while_break_without_value");
    expect(break_without_value(5)).toBe(1);
    expect(break_without_value(1)).toBe(0);
}, TEST_TIMEOUT_MS);

afterAll(() => {
    for (const file of TEMP_FILES) {
        try {
            unlinkSync(file);
        } catch (e) { }
    }
});
