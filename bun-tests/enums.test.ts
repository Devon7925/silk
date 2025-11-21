import { expect, test } from "bun:test";
import { join } from "path";
import { writeFileSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "enum_example.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "enum_example.silk");
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

test("rust style enums can be constructed and matched", async () => {
    const silkCode = `
    let IntOption = enum {
    Some = { i32 },
    None = {},
};
let calculate_positive_sum = fn{first: i32, second: i32} -> i32 (
    if first + second <= 0 (
        IntOption::None // allow trailing comment
    ) else (
        IntOption::Some{first + second}
    )
);
    let export(wasm) main = fn{} -> i32 (
        let a = calculate_positive_sum{0 - 1, 2};
        let b = calculate_positive_sum{0 - 3, 2};
        if let IntOption::Some{b_result} = b ( 1 )
        else if let IntOption::Some{a_result} = a ( 0 ) else ( 1 )
    );
    main{}
    `;

    const exports = await compileAndLoad(silkCode);
    expect(exports.main()).toBe(0);
}, TEST_TIMEOUT_MS);

