import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "target_annotations.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "target_annotations.silk");
const TEST_TIMEOUT_MS = 20000;

async function compileToWasm(silkCode: string) {
    writeFileSync(TEMP_SILK, silkCode);

    const proc = Bun.spawn(["cargo", "run", "--", TEMP_SILK, "-o", TEMP_WASM], {
        cwd: ROOT_DIR,
        stderr: "pipe",
        stdin: "ignore",
        stdout: "ignore",
        env: process.env,
    });

    const exitCode = await proc.exited;
    const stderr = await new Response(proc.stderr).text();
    return { exitCode, stderr };
}

async function compileAndLoad(silkCode: string) {
    const { exitCode, stderr } = await compileToWasm(silkCode);
    if (exitCode !== 0) {
        throw new Error(`Compilation failed:\n${stderr}`);
    }

    const wasmBuffer = await Bun.file(TEMP_WASM).arrayBuffer();
    const { instance } = await WebAssembly.instantiate(wasmBuffer);
    return instance.exports as any;
}

test("allows matching target bindings", async () => {
    const silkCode = `
    (target wasm) base := 40;
    (export wasm) (target wasm) add_two := {} => (
        base + 2
    );
    {}
  `;

    const { add_two } = await compileAndLoad(silkCode);
    expect(add_two()).toBe(42);
}, TEST_TIMEOUT_MS);

test("allows bindings with fewer target annotations to use shared bindings", async () => {
    const silkCode = `
    (target wasm) (target js) shared := 21;
    (export wasm) (target wasm) double_shared := {} => (
        shared + shared
    );
    {}
  `;

    const { double_shared } = await compileAndLoad(silkCode);
    expect(double_shared()).toBe(42);
}, TEST_TIMEOUT_MS);

test("rejects unannotated bindings using target bindings", async () => {
    const silkCode = `
    (target wasm) secret := 7;
    (export wasm) reveal := {} => (
        secret
    );
    {}
  `;

    const { exitCode, stderr } = await compileToWasm(silkCode);
    expect(exitCode).toBe(1);
    expect(stderr).toContain(
        "Cannot use target binding secret without a target annotation"
    );
}, TEST_TIMEOUT_MS);

test("rejects bindings with extra target annotations using narrower bindings", async () => {
    const silkCode = `
    (target wasm) only_wasm := 5;
    (export wasm) (target wasm) (target js) bad := {} => (
        only_wasm
    );
    {}
  `;

    const { exitCode, stderr } = await compileToWasm(silkCode);
    expect(exitCode).toBe(1);
    expect(stderr).toContain("Binding only_wasm requires target annotation(s): wasm");
}, TEST_TIMEOUT_MS);

afterAll(() => {
    try {
        unlinkSync(TEMP_SILK);
    } catch (e) {}
    try {
        unlinkSync(TEMP_WASM);
    } catch (e) {}
});
