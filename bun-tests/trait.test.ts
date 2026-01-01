import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "trait.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "trait.silk");
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

test("trait constraints enforce implementations for generic helpers", async () => {
    const silkCode = `
    Adder := (T: type) => (
        { add = (T) -> ((T) -> T) }
    );

    Meters := i32 @ {
        add = (self: i32) => ((other: i32) => self + other),
    };

    add_pair := (T: type @ Adder) => (
        ({ left: T, right: T }) => left.add(right)
    );

    (export wasm) add_meter_pair := (left: i32) => (
        add_pair(Meters){ left, 4 }
    );
    `;

    const exports = await compileAndLoad(silkCode);
    expect(exports.add_meter_pair(3)).toBe(7);
}, TEST_TIMEOUT_MS);

test("trait implementations work for multiple types and structs", async () => {
    const silkCode = `
    Adder := (T: type) => (
        { add = (T) -> ((T) -> T) }
    );

    Meters := i32 @ {
        add = (self: i32) => ((other: i32) => self + other),
    };

    Seconds := i32 @ {
        add = (self: i32) => ((other: i32) => self + other),
    };

    Pair := { left = i32, right = i32 } @ {
        add = (self: { left = i32, right = i32 }) => (
            (other: { left = i32, right = i32 }) => (
                { left = self.left + other.left, right = self.right + other.right }
            )
        ),
    };

    add_pair := (T: type @ Adder) => (
        ({ left: T, right: T }) => left.add(right)
    );

    (export wasm) add_meter := (left: i32) => (
        add_pair(Meters){ left, 4 }
    );

    (export wasm) add_second := (left: i32) => (
        add_pair(Seconds){ left, 9 }
    );

    (export wasm) add_pair_struct := (left: i32) => (
        adder := add_pair(Pair);
        result := adder{
            { left = left, right = 2 },
            { left = 3, right = 4 },
        };
        result.left
    );
    `;

    const exports = await compileAndLoad(silkCode);
    expect(exports.add_meter(3)).toBe(7);
    expect(exports.add_second(5)).toBe(14);
    expect(exports.add_pair_struct(1)).toBe(4);
}, TEST_TIMEOUT_MS);

afterAll(() => {
    try {
        unlinkSync(TEMP_SILK);
    } catch (e) { }
    try {
        unlinkSync(TEMP_WASM);
    } catch (e) { }
});
