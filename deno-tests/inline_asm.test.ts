import { assertEquals } from "https://deno.land/std/testing/asserts.ts";
import {
    cleanup,
    cleanupBase,
    compileSilk,
    compileToBase,
    importJsModule,
    loadWasm,
    tempBase,
} from "./test_helpers.ts";

async function compileBaseOrThrow(silkCode: string, prefix: string) {
    const result = await compileToBase(silkCode, prefix);
    if (result.code !== 0) {
        throw new Error(`Compilation failed:\n${result.stderr}`);
    }
    return result;
}

Deno.test("inline asm works in js exports", async () => {
    const silkCode = `
    (target js) (export js) answer := {} => (
        asm(js)("1 + 41")
    );
    `;

    const { basePath } = await compileBaseOrThrow(silkCode, "inline_asm_js");
    const jsPath = basePath + ".js";
    try {
        const module = await importJsModule(jsPath);
        assertEquals(module.answer(), 42);
    } finally {
        await cleanupBase(basePath, [".silk", ".js"]);
    }
});

Deno.test("inline asm works in wasm exports", async () => {
    const silkCode = `
    (target wasm) (export wasm) answer := {} => (
        asm(wasm)("i32.const 42")
    );
    `;

    const { basePath } = await compileBaseOrThrow(silkCode, "inline_asm_wasm");
    const wasmPath = basePath + ".wasm";
    try {
        const exports = (await loadWasm(wasmPath)).exports as any;
        assertEquals(exports.answer(), 42);
    } finally {
        await cleanupBase(basePath, [".silk", ".wasm"]);
    }
});

Deno.test("inline asm supports multi-instruction arithmetic in wasm exports", async () => {
    const silkCode = `
    (target wasm) (export wasm) compute := {} => (
        asm(wasm)("i32.const 7; i32.const 5; i32.add; i32.const 3; i32.mul")
    );
    `;

    const { basePath } = await compileBaseOrThrow(silkCode, "inline_asm_wasm_math");
    const wasmPath = basePath + ".wasm";
    try {
        const exports = (await loadWasm(wasmPath)).exports as any;
        assertEquals(exports.compute(), 36);
    } finally {
        await cleanupBase(basePath, [".silk", ".wasm"]);
    }
});

Deno.test("inline asm supports comparisons in wasm exports", async () => {
    const silkCode = `
    (target wasm) (export wasm) is_seven := {} => (
        asm(wasm)("i32.const 10; i32.const 3; i32.sub; i32.const 7; i32.eq")
    );
    `;

    const { basePath } = await compileBaseOrThrow(silkCode, "inline_asm_wasm_cmp");
    const wasmPath = basePath + ".wasm";
    try {
        const exports = (await loadWasm(wasmPath)).exports as any;
        assertEquals(exports.is_seven(), 1);
    } finally {
        await cleanupBase(basePath, [".silk", ".wasm"]);
    }
});

Deno.test("inline asm can participate in argument arithmetic", async () => {
    const silkCode = `
    (target wasm) (export wasm) add_one := (x: i32) => (
        x + asm(wasm)("i32.const 1")
    );
    `;

    const { basePath } = await compileBaseOrThrow(silkCode, "inline_asm_wasm_arg");
    const wasmPath = basePath + ".wasm";
    try {
        const exports = (await loadWasm(wasmPath)).exports as any;
        assertEquals(exports.add_one(41), 42);
    } finally {
        await cleanupBase(basePath, [".silk", ".wasm"]);
    }
});

Deno.test("inline asm supports local.get and local.set", async () => {
    const silkCode = `
    (target wasm) (export wasm) add_one_asm := (x: i32) => (
        y := 0;
        asm(wasm)("local.get 0; i32.const 1; i32.add; local.set 1; local.get 1")
    );
    `;

    const { basePath } = await compileBaseOrThrow(silkCode, "inline_asm_wasm_local");
    const wasmPath = basePath + ".wasm";
    try {
        const exports = (await loadWasm(wasmPath)).exports as any;
        assertEquals(exports.add_one_asm(41), 42);
    } finally {
        await cleanupBase(basePath, [".silk", ".wasm"]);
    }
});

Deno.test("js output can call wasm output from the same silk file", async () => {
    const basePath = tempBase("inline_asm_combo");
    const jsPath = basePath + ".js";
    const wasmPath = basePath + ".wasm";
    const wasmPathLiteral = wasmPath.replaceAll("\\", "/").replaceAll("'", "\\'");

    const silkCode = `
    (target wasm) (export wasm) wasm_answer := {} => (
        asm(wasm)("i32.const 42")
    );

    (target js) (export js) call_wasm := {} => (
        asm(js)("((async () => { if (!globalThis.__silk_wasm_instance) { const bytes = await Deno.readFile('${wasmPathLiteral}'); const { instance } = await WebAssembly.instantiate(bytes); globalThis.__silk_wasm_instance = instance; } return globalThis.__silk_wasm_instance.exports.wasm_answer(); })())")
    );
    `;

    const { code, stderr, silkPath } = await compileSilk(silkCode, basePath);
    if (code !== 0) {
        await cleanup([silkPath, wasmPath, jsPath]);
        throw new Error(`Compilation failed:\n${stderr}`);
    }

    try {
        const module = await importJsModule(jsPath);
        assertEquals(await module.call_wasm(), 42);
    } finally {
        await cleanup([silkPath, jsPath, wasmPath]);
        try {
            delete (globalThis as any).__silk_wasm_instance;
        } catch {
            // ignore
        }
    }
});
