import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "enum_demo.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "enum_demo.silk");
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

async function compileExpectError(silkCode: string) {
    writeFileSync(TEMP_SILK, silkCode);

    const proc = Bun.spawn(["cargo", "run", "--", TEMP_SILK, "-o", TEMP_WASM], {
        cwd: ROOT_DIR,
        stderr: "pipe",
    });

    const exitCode = await proc.exited;
    const stderr = await new Response(proc.stderr).text();
    return { exitCode, stderr };
}

test("enum construction and matching", async () => {
    const silkCode = `
    IntOption := enum { Some = i32, None = {} };
    (export wasm) unwrap_or_zero := (x: i32) => (
        value := if x > 0 then (
            IntOption::Some(x)
        ) else (
            IntOption::None
        );

        if IntOption::Some(v) := value then (
            v
        ) else (
            0
        )
    );
    {} 
    `;

    const exports = await compileAndLoad(silkCode);
    expect(exports.unwrap_or_zero(3)).toBe(3);
    expect(exports.unwrap_or_zero(-2)).toBe(0);
}, TEST_TIMEOUT_MS);

test("enum rejects value payloads", async () => {
    const silkCode = `
    Bad := enum { Value = 1 };
    {};
    `;

    const { exitCode, stderr } = await compileExpectError(silkCode);
    expect(exitCode).not.toBe(0);
    expect(stderr).toContain("Enum variant payload must be a type");
}, TEST_TIMEOUT_MS);

test("enum intrinsic can be aliased", async () => {
    const silkCode = `
    EnumFactory := enum;
    Flag := EnumFactory { On = {}, Off = {} };
    (export wasm) as_bool := {flag: i32} => (
        value := if flag > 0 then (
            Flag::On
        ) else (
            Flag::Off
        );

        if Flag::On := value then (
            1
        ) else (
            0
        )
    );
    {};
    `;

    const exports = await compileAndLoad(silkCode);
    expect(exports.as_bool(1)).toBe(1);
    expect(exports.as_bool(-1)).toBe(0);
}, TEST_TIMEOUT_MS);

test("enum patterns require defined enum types", async () => {
    const silkCode = `
    Opt := enum { Some = i32, None = {} };
    (export wasm) demo := {} => (
        value := Opt::Some(1);
        if Missing::Some(v) := value then ( v ) else ( 0 )
    );
    {};
    `;

    const { exitCode, stderr } = await compileExpectError(silkCode);
    expect(exitCode).not.toBe(0);
    expect(stderr).toContain("Enum pattern references unknown type: Missing");
}, TEST_TIMEOUT_MS);

test("enum patterns respect variant enum types", async () => {
    const silkCode = `
    First := enum { Some = i32, None = {} };
    Second := enum { Some = {}, None = {} };
    (export wasm) check := {} => (
        value := First::Some(3);
        if Second::Some := value then ( 1 ) else ( 0 )
    );
    {};
    `;

    const exports = await compileAndLoad(silkCode);
    expect(exports.check()).toBe(0);
}, TEST_TIMEOUT_MS);

afterAll(() => {
    try {
        unlinkSync(TEMP_SILK);
    } catch { }
    try {
        unlinkSync(TEMP_WASM);
    } catch { }
});
