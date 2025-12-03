import { test, expect } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "temp_boolean.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "temp_boolean.silk");

const TEST_TIMEOUT_MS = 20000;

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
    let (export wasm) get_true = {} => true;
    let (export wasm) get_false = {} => ( false );
    {}
    `;
    await compileAndRun(code, async (instance) => {
        const exports = instance.exports as any;
        expect(exports.get_true()).toBe(1);
        expect(exports.get_false()).toBe(0);
    });

    // Cleanup
    try { unlinkSync(TEMP_SILK); unlinkSync(TEMP_WASM); } catch (e) { }
}, TEST_TIMEOUT_MS);

test("boolean comparisons", async () => {
    await compileAndRun(
        `
    let (export wasm) is_true = (a: bool) => ( a == true );
    let (export wasm) is_false = (a: bool) => ( a == false );
    let (export wasm) check_eq_5 = (a: i32) => ( a == 5 );
    let (export wasm) check_neq_5 = (a: i32) => ( a != 5 );
    let (export wasm) check_lt_10 = (a: i32) => ( a < 10 );
    let (export wasm) check_gt_10 = (a: i32) => ( a > 10 );
    let (export wasm) check_le_10 = (a: i32) => ( a <= 10 );
    let (export wasm) check_ge_10 = (a: i32) => ( a >= 10 );
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
}, TEST_TIMEOUT_MS);

test("boolean operators", async () => {
    await compileAndRun(
        `
    let (export wasm) and_true = (a: bool) => ( a && true );
    let (export wasm) and_false = (a: bool) => ( a && false );
    let (export wasm) or_true = (a: bool) => ( a || true );
    let (export wasm) or_false = (a: bool) => ( a || false );
    let (export wasm) xor_true = (a: bool) => ( a ^ true );
    let (export wasm) xor_false = (a: bool) => ( a ^ false );
        `,
        async (instance) => {
            const exports = instance.exports as any;
            expect(exports.and_true(1)).toBe(1);
            expect(exports.and_true(0)).toBe(0);
            expect(exports.and_false(1)).toBe(0);
            expect(exports.and_false(0)).toBe(0);

            expect(exports.or_true(0)).toBe(1);
            expect(exports.or_true(1)).toBe(1);
            expect(exports.or_false(0)).toBe(0);
            expect(exports.or_false(1)).toBe(1);

            expect(exports.xor_true(1)).toBe(0);
            expect(exports.xor_true(0)).toBe(1);
            expect(exports.xor_false(1)).toBe(1);
            expect(exports.xor_false(0)).toBe(0);
        }
    );

    // Cleanup
    try { unlinkSync(TEMP_SILK); unlinkSync(TEMP_WASM); } catch (e) { }
}, TEST_TIMEOUT_MS);

test("boolean operator chaining", async () => {
    await compileAndRun(
        `
    let (export wasm) all_true = {} => ( true && true && true );
    let (export wasm) short_circuit_false = {} => ( true && true && false );
    let (export wasm) any_true = {} => ( false || false || true );
    let (export wasm) all_false = {} => ( false || false || false );
    let (export wasm) odd_true = {} => ( true ^ true ^ true );
    let (export wasm) odd_false = {} => ( true ^ false ^ true );
        `,
        async (instance) => {
            const exports = instance.exports as any;

            expect(exports.all_true()).toBe(1);
            expect(exports.short_circuit_false()).toBe(0);

            expect(exports.any_true()).toBe(1);
            expect(exports.all_false()).toBe(0);

            expect(exports.odd_true()).toBe(1);
            expect(exports.odd_false()).toBe(0);
        }
    );

    // Cleanup
    try { unlinkSync(TEMP_SILK); unlinkSync(TEMP_WASM); } catch (e) { }
}, TEST_TIMEOUT_MS);
