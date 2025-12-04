import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(ROOT_DIR, "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "generic_enum_temp.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "generic_enum_temp.silk");
const TEST_TIMEOUT_MS = 20000;

async function compileToInstance(code: string) {
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
    return instance;
}

test("generic enum: Option<T>", async () => {
    const source = `
    Option := (T: type) => (
        enum { Some = T, None = {} }
    );

    (export wasm) unwrap_or_zero := (x: i32) => (
        val: Option(i32) := if x > 0 then (
            Option(i32)::Some(x)
        ) else (
            Option(i32)::None
        );

        if Option(i32)::Some(v) := val then (
            v
        ) else (
            0
        )
    );
    `;

    const instance = await compileToInstance(source);
    const exports = instance.exports as any;

    expect(exports.unwrap_or_zero(10)).toBe(10);
    expect(exports.unwrap_or_zero(-5)).toBe(0);
}, TEST_TIMEOUT_MS);

test("generic enum: nested generics", async () => {
    const source = `
    Option := (T: type) => (
        enum { Some = T, None = {} }
    );
    Container := (T: type) => (
        enum { Wrapped = Option(T), Empty = {} }
    );

    (export wasm) check := (x: i32) => (
        c := Container(i32)::Wrapped(Option(i32)::Some(x));
        
        if Container(i32)::Wrapped(opt) := c then (
            if Option(i32)::Some(val) := opt then (
                val
            ) else ( 0 )
        ) else ( 0 )
    );
    `;

    const instance = await compileToInstance(source);
    const exports = instance.exports as any;

    expect(exports.check(42)).toBe(42);
}, TEST_TIMEOUT_MS);

afterAll(() => {
    try {
        unlinkSync(TEMP_SILK);
    } catch (e) { }
    try {
        unlinkSync(TEMP_WASM);
    } catch (e) { }
});
