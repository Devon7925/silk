import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(ROOT_DIR, "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "structs_temp.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "structs_temp.silk");
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

test("compiles struct-heavy wasm export", async () => {
    const silkCode = `
let Pair: type = {
    first = i32,
    second = i32,
};
let rotate_pair = (point: Pair) => Pair (
    {
        first = 0 - point.second,
        second = point.first,
    }
);
let len_squared = (point: Pair) => i32 (
    point.first * point.first + point.second * point.second
);
let (export wasm) len_rotated_squared = (x:i32) => i32 (
    len_squared(rotate_pair{
        first = x,
        second = 2,
    })
);
`;

    const instance = await compileToInstance(silkCode);
    const { len_rotated_squared } = instance.exports as any;

    expect(len_rotated_squared(3)).toBe(13);
}, TEST_TIMEOUT_MS);

test("allows structs to flow through wasm boundaries", async () => {
    const silkCode = `
let Point: type = {
    x = i32,
    y = i32,
};
let normalize = (input: Point) => Point (
    {
        x = input.x - input.y,
        y = input.y - input.x,
    }
);
let (export wasm) sum_coords = (value: i32) => i32 (
    let normalized = normalize{
        x = value + 1,
        y = value - 1,
    };
    normalized.x + normalized.y
);
`;

    const instance = await compileToInstance(silkCode);
    const { sum_coords } = instance.exports as any;

    expect(sum_coords(10)).toBe(0);
}, TEST_TIMEOUT_MS);

// Cleanup after the test suite to avoid leaving temporary files behind.
afterAll(() => {
    try {
        unlinkSync(TEMP_SILK);
    } catch (e) { }
    try {
        unlinkSync(TEMP_WASM);
    } catch (e) { }
});
