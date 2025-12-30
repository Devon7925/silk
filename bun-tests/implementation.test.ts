import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "implementation.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "implementation.silk");
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

test("user implementations work on type aliases", async () => {
    const silkCode = `
    Meters := i32 @ {
        square = (self: i32) => self * self,
    };

    (export wasm) square_meters := (value: i32) => (
        meters: Meters := value;
        meters.square
    );
    `;

    const exports = await compileAndLoad(silkCode);
    expect(exports.square_meters(5)).toBe(25);
}, TEST_TIMEOUT_MS);

test("user implementations work on struct types", async () => {
    const silkCode = `
    Pair := { first = i32, second = i32 } @ {
        sum = (self: { first = i32, second = i32 }) => self.first + self.second,
    };

    (export wasm) sum_pair := (first: i32) => (
        pair: Pair := { first = first, second = 4 };
        pair.sum
    );
    `;

    const exports = await compileAndLoad(silkCode);
    expect(exports.sum_pair(3)).toBe(7);
}, TEST_TIMEOUT_MS);

afterAll(() => {
    try {
        unlinkSync(TEMP_SILK);
    } catch (e) { }
    try {
        unlinkSync(TEMP_WASM);
    } catch (e) { }
});
