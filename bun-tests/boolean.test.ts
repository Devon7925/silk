import { test, expect } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "temp_boolean.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "temp_boolean.silk");

async function compileAndRun(code: string, callback: (instance: WebAssembly.Instance) => Promise<void>) {
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
    await callback(instance);
}

test("boolean literals", async () => {
    const code = `
    let export(wasm) get_true = fn{} -> bool ( true );
    let export(wasm) get_false = fn{} -> bool ( false );
    {}
    `;
    await compileAndRun(code, async (instance) => {
        const exports = instance.exports as any;
        expect(exports.get_true()).toBe(1);
        expect(exports.get_false()).toBe(0);
    });

    // Cleanup
    try { unlinkSync(TEMP_SILK); unlinkSync(TEMP_WASM); } catch (e) { }
});

test("boolean comparisons", async () => {
    await compileAndRun(
        `
    let export(wasm) is_true = fn(a: bool) -> bool ( a == true );
    let export(wasm) is_false = fn(a: bool) -> bool ( a == false );
    let export(wasm) check_eq_5 = fn(a: i32) -> bool ( a == 5 );
    let export(wasm) check_neq_5 = fn(a: i32) -> bool ( a != 5 );
    let export(wasm) check_lt_10 = fn(a: i32) -> bool ( a < 10 );
    let export(wasm) check_gt_10 = fn(a: i32) -> bool ( a > 10 );
    let export(wasm) check_le_10 = fn(a: i32) -> bool ( a <= 10 );
    let export(wasm) check_ge_10 = fn(a: i32) -> bool ( a >= 10 );
        `,
        async (instance) => {
            const exports = instance.exports as any;
            expect(exports.is_true(1)).toBe(1);
            expect(exports.is_true(0)).toBe(0);
            expect(exports.is_false(0)).toBe(1);
            expect(exports.is_false(1)).toBe(0);

            expect(exports.check_eq_5(5)).toBe(1);
            expect(exports.check_eq_5(4)).toBe(0);

            expect(exports.check_neq_5(5)).toBe(0);
            expect(exports.check_neq_5(6)).toBe(1);

            expect(exports.check_lt_10(9)).toBe(1);
            expect(exports.check_lt_10(10)).toBe(0);

            expect(exports.check_gt_10(11)).toBe(1);
            expect(exports.check_gt_10(10)).toBe(0);

            expect(exports.check_le_10(10)).toBe(1);
            expect(exports.check_le_10(11)).toBe(0);

            expect(exports.check_ge_10(10)).toBe(1);
            expect(exports.check_ge_10(9)).toBe(0);
        }
    );

    // Cleanup
    try { unlinkSync(TEMP_SILK); unlinkSync(TEMP_WASM); } catch (e) { }
});
