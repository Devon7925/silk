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

test("loop can compute factorial", async () => {
    const silkCode = `
    (export wasm) factorial := (limit: i32) => (
        mut acc := 1;
        mut iter := limit;
        loop (
            acc = acc * iter;
            iter = iter - 1;
            if iter <= 0 then (
                return acc;
            )
        );
        0  // Unreachable
    );
    {};
    `;

    const exports = await compileAndLoad(silkCode, "loop_factorial");
    expect(exports.factorial(5)).toBe(120);
}, TEST_TIMEOUT_MS);

test("loop break returns value", async () => {
    const silkCode = `
    (export wasm) first_non_positive := (start: i32) => (
        mut current := start;
        loop (
            if current <= 0 then (
                break current;
            );
            current = current - 1;
        )
    );
    {};
    `;

    const exports = await compileAndLoad(silkCode, "loop_break_value");
    expect(exports.first_non_positive(3)).toBe(0);
    expect(exports.first_non_positive(-2)).toBe(-2);
}, TEST_TIMEOUT_MS);

afterAll(() => {
    for (const file of TEMP_FILES) {
        try {
            unlinkSync(file);
        } catch (e) { }
    }
});
