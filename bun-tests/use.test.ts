import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync, existsSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "use_demo.wasm");
const TEMP_MAIN = join(FIXTURES_DIR, "use_demo.silk");
const TEMP_LIB = join(FIXTURES_DIR, "use_module.silk");
const TEST_TIMEOUT_MS = 20000;

async function compileAndLoad() {
    const proc = Bun.spawn(["cargo", "run", "--", TEMP_MAIN, "-o", TEMP_WASM], {
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

afterAll(() => {
    for (const path of [TEMP_WASM, TEMP_MAIN, TEMP_LIB]) {
        if (existsSync(path)) {
            unlinkSync(path);
        }
    }
});

test(
    "use imports block expressions from other files",
    async () => {
        writeFileSync(
            TEMP_MAIN,
            `
        lib := use "use_module.silk";
        (export wasm) answer := {} => ( lib.answer );
        {}
        `,
        );
        writeFileSync(
            TEMP_LIB,
            `
        {
            answer = 40 + 2
        }
        `,
        );

        const exports = await compileAndLoad();
        expect(exports.answer()).toBe(42);
    },
    TEST_TIMEOUT_MS,
);

test(
    "use imports functions from other files",
    async () => {
        writeFileSync(
            TEMP_MAIN,
            `
        lib := use "use_module.silk";
        (export wasm) double_answer := {} => ( lib.double 21 );
        {}
        `,
        );
        writeFileSync(
            TEMP_LIB,
            `
        {
            double = (value: i32) => ( value * 2 )
        }
        `,
        );

        const exports = await compileAndLoad();
        expect(exports.double_answer()).toBe(42);
    },
    TEST_TIMEOUT_MS,
);
