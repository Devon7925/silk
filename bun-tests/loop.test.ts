import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(ROOT_DIR, "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "loop_factorial.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "loop_factorial.silk");
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

test("loop can compute factorial", async () => {
    const silkCode = `
    let export(wasm) factorial = fn(limit: i32) -> i32 (
        let mut acc = 1;
        let mut iter = limit;
        loop (
            acc = acc * iter;
            iter = iter - 1;
            if iter <= 0 (
                return acc;
            )
        )
    );
    {};
    `;

    const exports = await compileAndLoad(silkCode);
    expect(exports.factorial(5)).toBe(120);
}, TEST_TIMEOUT_MS);

afterAll(() => {
    try {
        unlinkSync(TEMP_SILK);
    } catch (e) {}
    try {
        unlinkSync(TEMP_WASM);
    } catch (e) {}
});
