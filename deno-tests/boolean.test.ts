import { assertEquals } from "https://deno.land/std/testing/asserts.ts";
import { cleanup, compileToWasm } from "./test_helpers.ts";

async function compileAndRun(
    code: string,
    prefix: string,
    callback: (instance: WebAssembly.Instance) => Promise<void>,
) {
    const { wasmPath, silkPath } = await compileToWasm(code, prefix);
    const wasmBuffer = await Deno.readFile(wasmPath);
    const { instance } = await WebAssembly.instantiate(wasmBuffer);
    try {
        await callback(instance);
    } finally {
        await cleanup([silkPath, wasmPath]);
    }
}

Deno.test("boolean literals", async () => {
    const code = `
    (export wasm) get_true := {} => true;
    (export wasm) get_false := {} => ( false );
    {}
    `;
    await compileAndRun(code, "boolean_literals", async (instance) => {
        const exports = instance.exports as any;
        assertEquals(exports.get_true(), 1);
        assertEquals(exports.get_false(), 0);
    });
});

Deno.test("boolean comparisons", async () => {
    await compileAndRun(
        `
    (export wasm) is_true := (a: bool) => ( a == true );
    (export wasm) is_false := (a: bool) => ( a == false );
    (export wasm) check_eq_5 := (a: i32) => ( a == 5 );
    (export wasm) check_neq_5 := (a: i32) => ( a != 5 );
    (export wasm) check_lt_10 := (a: i32) => ( a < 10 );
    (export wasm) check_gt_10 := (a: i32) => ( a > 10 );
    (export wasm) check_le_10 := (a: i32) => ( a <= 10 );
    (export wasm) check_ge_10 := (a: i32) => ( a >= 10 );
        `,
        "boolean_comparisons",
        async (instance) => {
            const exports = instance.exports as any;
            assertEquals(exports.is_true(1), 1);
            assertEquals(exports.is_true(0), 0);
            assertEquals(exports.is_false(0), 1);
            assertEquals(exports.is_false(1), 0);

            assertEquals(exports.check_eq_5(5), 1);
            assertEquals(exports.check_eq_5(4), 0);

            assertEquals(exports.check_neq_5(5), 0);
            assertEquals(exports.check_neq_5(6), 1);

            assertEquals(exports.check_lt_10(9), 1);
            assertEquals(exports.check_lt_10(10), 0);

            assertEquals(exports.check_gt_10(11), 1);
            assertEquals(exports.check_gt_10(10), 0);

            assertEquals(exports.check_le_10(10), 1);
            assertEquals(exports.check_le_10(11), 0);

            assertEquals(exports.check_ge_10(10), 1);
            assertEquals(exports.check_ge_10(9), 0);
        },
    );
});

Deno.test("boolean operators", async () => {
    await compileAndRun(
        `
    (export wasm) and_true := (a: bool) => ( a && true );
    (export wasm) and_false := (a: bool) => ( a && false );
    (export wasm) or_true := (a: bool) => ( a || true );
    (export wasm) or_false := (a: bool) => ( a || false );
    (export wasm) xor_true := (a: bool) => ( a ^ true );
    (export wasm) xor_false := (a: bool) => ( a ^ false );
        `,
        "boolean_ops",
        async (instance) => {
            const exports = instance.exports as any;
            assertEquals(exports.and_true(1), 1);
            assertEquals(exports.and_true(0), 0);
            assertEquals(exports.and_false(1), 0);
            assertEquals(exports.and_false(0), 0);

            assertEquals(exports.or_true(0), 1);
            assertEquals(exports.or_true(1), 1);
            assertEquals(exports.or_false(0), 0);
            assertEquals(exports.or_false(1), 1);

            assertEquals(exports.xor_true(1), 0);
            assertEquals(exports.xor_true(0), 1);
            assertEquals(exports.xor_false(1), 1);
            assertEquals(exports.xor_false(0), 0);
        },
    );
});

Deno.test("boolean operator chaining", async () => {
    await compileAndRun(
        `
    (export wasm) all_true := {} => ( true && true && true );
    (export wasm) short_circuit_false := {} => ( true && true && false );
    (export wasm) any_true := {} => ( false || false || true );
    (export wasm) all_false := {} => ( false || false || false );
    (export wasm) odd_true := {} => ( true ^ true ^ true );
    (export wasm) odd_false := {} => ( true ^ false ^ true );
        `,
        "boolean_chain",
        async (instance) => {
            const exports = instance.exports as any;

            assertEquals(exports.all_true(), 1);
            assertEquals(exports.short_circuit_false(), 0);

            assertEquals(exports.any_true(), 1);
            assertEquals(exports.all_false(), 0);

            assertEquals(exports.odd_true(), 1);
            assertEquals(exports.odd_false(), 0);
        },
    );
});
