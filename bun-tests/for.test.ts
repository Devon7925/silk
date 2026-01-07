import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(ROOT_DIR, "fixtures");
const TEMP_FILES = new Set<string>();
const TEST_TIMEOUT_MS = 20000;

async function compileAndLoad(silkCode: string, basename: string) {
    const wasmPath = join(FIXTURES_DIR, `${basename}.wasm`);
    const silkPath = join(FIXTURES_DIR, `${basename}.silk`);
    TEMP_FILES.add(wasmPath);
    TEMP_FILES.add(silkPath);

    writeFileSync(silkPath, silkCode);

    const proc = Bun.spawn(["cargo", "run", "--", silkPath, "-o", wasmPath], {
        cwd: ROOT_DIR,
        stderr: "pipe",
    });

    const exitCode = await proc.exited;
    if (exitCode !== 0) {
        const stderr = await new Response(proc.stderr).text();
        throw new Error(`Compilation failed:\n${stderr}`);
    }

    const wasmBuffer = await Bun.file(wasmPath).arrayBuffer();
    const { instance } = await WebAssembly.instantiate(wasmBuffer);
    return instance.exports as any;
}

const iteratorHelpers = `
RangeIter := { value = i32, limit = i32 } @ {
    iter_ty = i32,
    next = (mut self: { value = i32, limit = i32 }) => (
        if self.value < self.limit then (
            self.value = self.value + 1;
            Option(i32)::Some(self.value)
        ) else Option(i32)::None
    ),
};

make_range := (limit: i32) => (
    range: RangeIter := { value = 0, limit = limit };
    range
);
`;

test("for loops sum ranges", async () => {
    const silkCode = `
    ${iteratorHelpers}
    (export wasm) sum_range := (limit: i32) => (
        mut acc := 0;
        for value in make_range(limit) do (
            acc = acc + value;
        );
        acc
    );
    {};
    `;

    const { sum_range } = await compileAndLoad(silkCode, "for_sum_range");
    expect(sum_range(0)).toBe(0);
    expect(sum_range(5)).toBe(10);
}, TEST_TIMEOUT_MS);

test("for loops support struct patterns", async () => {
    const silkCode = `
    PairIter := { value = i32, limit = i32 } @ {
        iter_ty = { left = i32, right = i32 },
        next = (mut self: { value = i32, limit = i32 }) => (
            if self.value < self.limit then (
                current := self.value;
                self.value = self.value + 1;
                Option({ left = i32, right = i32 })::Some({ left = current, right = current + 1 })
            ) else Option({ left = i32, right = i32 })::None
        ),
    };

    make_pairs := (limit: i32) => (
        pairs: PairIter := { value = 0, limit = limit };
        pairs
    );

    (export wasm) sum_pairs := (limit: i32) => (
        mut acc := 0;
        for { left, right } in make_pairs(limit) do (
            acc = acc + left + right;
        );
        acc
    );
    {};
    `;

    const { sum_pairs } = await compileAndLoad(silkCode, "for_struct_patterns");
    expect(sum_pairs(0)).toBe(0);
    expect(sum_pairs(3)).toBe(9);
}, TEST_TIMEOUT_MS);

test("for loops parse iterator calls with arguments", async () => {
    const silkCode = `
    ${iteratorHelpers}
    (export wasm) sum_plus_one := (limit: i32) => (
        mut acc := 0;
        for value in make_range(limit + 1) do (
            acc = acc + value;
        );
        acc
    );
    {};
    `;

    const { sum_plus_one } = await compileAndLoad(silkCode, "for_call_argument");
    expect(sum_plus_one(0)).toBe(0);
    expect(sum_plus_one(4)).toBe(10);
}, TEST_TIMEOUT_MS);

afterAll(() => {
    for (const file of TEMP_FILES) {
        try {
            unlinkSync(file);
        } catch (e) { }
    }
});
