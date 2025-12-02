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
    let (export wasm) sum_until = (limit: i32) => i32 (
        let mut acc = 0;
        let mut iter = 0;
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
    let (export wasm) decrement_to_zero = (start: i32) => i32 (
        let mut value = start;
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

afterAll(() => {
    for (const file of TEMP_FILES) {
        try {
            unlinkSync(file);
        } catch (e) { }
    }
});
