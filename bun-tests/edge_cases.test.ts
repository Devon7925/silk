import { test, expect } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "edge_cases.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "edge_cases.silk");

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

test("shadowing in block", async () => {
    const silkCode = `
    let export(wasm) shadow_test = fn(x: i32) -> i32 (
        let y = 10;
        (
            let y = 20;
            x + y
        )
    );
    {}
    `;
    const exports = await compileAndLoad(silkCode);
    // x + 20. If x=5, result=25.
    expect(exports.shadow_test(5)).toBe(25);
});

test("multiple exports", async () => {
    const silkCode = `
    let export(wasm) add = fn(x: i32) -> i32 ( x + 1 );
    let export(wasm) sub = fn(x: i32) -> i32 ( x - 1 );
    {}
    `;
    const exports = await compileAndLoad(silkCode);
    expect(exports.add(10)).toBe(11);
    expect(exports.sub(10)).toBe(9);
});

test("arithmetic edge cases", async () => {
    const silkCode = `
    let export(wasm) div_test = fn(x: i32) -> i32 ( 100 / x );
    {}
    `;
    const exports = await compileAndLoad(silkCode);
    expect(exports.div_test(2)).toBe(50);
    expect(() => exports.div_test(0)).toThrow(); // Division by zero should trap
});

test("deeply nested bindings", async () => {
    const silkCode = `
    let export(wasm) nested_test = fn(x: i32) -> i32 (
        let a = x + 1;
        let b = a * 2;
        let c = b + a; 
        c
    );
    {}
    `;
    // x=2
    // a = 3
    // b = 6
    // c = 6 + 3 = 9
    const exports = await compileAndLoad(silkCode);
    expect(exports.nested_test(2)).toBe(9);
});
