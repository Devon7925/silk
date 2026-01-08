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

test("range operator iterates start-inclusive end-exclusive", async () => {
    const silkCode = `
    (export wasm) sum_range := (start: i32, end: i32) => (
        mut acc := 0;
        for value in start .. end do (
            acc = acc + value;
        );
        acc
    );
    {};
    `;

    const { sum_range } = await compileAndLoad(silkCode, "range_sum");
    expect(sum_range(2, 5)).toBe(9);
    expect(sum_range(5, 5)).toBe(0);
}, TEST_TIMEOUT_MS);

test("range operator uses lower precedence than addition", async () => {
    const silkCode = `
    (export wasm) sum_to := (limit: i32) => (
        mut acc := 0;
        for value in 0 .. limit + 1 do (
            acc = acc + value;
        );
        acc
    );
    {};
    `;

    const { sum_to } = await compileAndLoad(silkCode, "range_precedence");
    expect(sum_to(0)).toBe(0);
    expect(sum_to(3)).toBe(6);
}, TEST_TIMEOUT_MS);

afterAll(() => {
    for (const file of TEMP_FILES) {
        try {
            unlinkSync(file);
        } catch (e) {}
    }
});
