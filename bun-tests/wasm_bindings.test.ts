import { test, expect } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "temp_bindings.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "temp_bindings.silk");

test("compiles and runs wasm with bindings", async () => {
    const silkCode = `
    let export(wasm) double_add = fn(x: i32) -> i32 (
        let y = x * 2;
        y + y
    );
    {}
  `;
    writeFileSync(TEMP_SILK, silkCode);

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
    const { double_add } = instance.exports as any;

    expect(double_add(5)).toBe(20); // (5 * 2) + (5 * 2) = 10 + 10 = 20

    // Cleanup
    try {
        unlinkSync(TEMP_SILK);
        unlinkSync(TEMP_WASM);
    } catch (e) { }
});
