import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync, existsSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_SILK = join(FIXTURES_DIR, "temp_bindings_js.silk");
const TEST_TIMEOUT_MS = 20000;

async function compileAndLoad(silkCode: string) {
    writeFileSync(TEMP_SILK, silkCode);

    // Use unique filename to avoid import cache
    const id = Date.now() + "_" + Math.random().toString().slice(2);
    const outputBase = join(FIXTURES_DIR, `temp_js_${id}`);
    const expectedJs = outputBase + ".js";

    const proc = Bun.spawn(["cargo", "run", "--", TEMP_SILK, "-o", outputBase], {
        cwd: ROOT_DIR,
        stderr: "pipe",
    });

    const exitCode = await proc.exited;
    if (exitCode !== 0) {
        const stderr = await new Response(proc.stderr).text();
        console.error("FULL STDERR:", stderr);
        throw new Error(`Compilation failed:\n${stderr}`);
    }

    try {
        const module = await import(expectedJs);
        return {
            module, cleanup: () => {
                if (existsSync(expectedJs)) unlinkSync(expectedJs);
            }
        };
    } catch (e) {
        if (existsSync(expectedJs)) unlinkSync(expectedJs);
        throw e;
    }
}

const SILK_CODE = `
    (export js) answer := 42;
    (export js) add := {x: i32, y: i32} => (
        x + y
    );
    (export js) calculate := (x: i32) => (
        val := x * 2;
        val + 1
    );
`;

test("compiles and runs js export", async () => {
    const { module, cleanup } = await compileAndLoad(SILK_CODE);
    expect(module.answer).toBe(42);
    expect(module.add(10, 20)).toBe(30);
    expect(module.calculate(10)).toBe(21);
    cleanup();
}, TEST_TIMEOUT_MS);

test("supports if expressions", async () => {
    const SILK_CODE_IF = `
    (export js) check := (x: i32) => (
        if (x > 10) then 1 else 0
    );
    `;
    const { module, cleanup } = await compileAndLoad(SILK_CODE_IF);
    expect(module.check(11)).toBe(1);
    expect(module.check(10)).toBe(0);
    cleanup();
}, TEST_TIMEOUT_MS);

test("supports struct objects", async () => {
    const SILK_CODE_STRUCT = `
    (export js) make_point := {x: i32, y: i32} => (
        { x = x, y = y }
    );
    `;
    const { module, cleanup } = await compileAndLoad(SILK_CODE_STRUCT);
    const p = module.make_point(10, 20);
    expect(p.x).toBe(10);
    expect(p.y).toBe(20);
    cleanup();
}, TEST_TIMEOUT_MS);

test("exported function can call internal function", async () => {
    const SILK_CODE_INTERNAL = `
    double := (x: i32) => (x * 2);
    (export js) quadruple := (x: i32) => (double(double(x)));
    `;
    const { module, cleanup } = await compileAndLoad(SILK_CODE_INTERNAL);
    expect(module.quadruple(5)).toBe(20);
    cleanup();
}, TEST_TIMEOUT_MS);

test("supports struct property access", async () => {
    const SILK_CODE_PROP = `
    (export js) get_x := (val: i32) => (
        p := { x = val, y = 100 };
        p.x
    );
    `;
    const { module, cleanup } = await compileAndLoad(SILK_CODE_PROP);
    expect(module.get_x(42)).toBe(42);
    cleanup();
}, TEST_TIMEOUT_MS);

test("supports tagged union struct (enum pattern)", async () => {
    const SILK_CODE_ENUM = `
    Option := (T: type) => (
        enum { Some = T, None = {} }
    );
    (export js) is_some := (val: i32) => (
        opt := if val > 0 then Option(i32)::Some(val) else Option(i32)::None;
        if Option(i32)::Some(v) := opt then 1 else 0
    );
    `;
    const { module, cleanup } = await compileAndLoad(SILK_CODE_ENUM);
    expect(module.is_some(42)).toBe(1);
    expect(module.is_some(-1)).toBe(0);
    cleanup();
}, TEST_TIMEOUT_MS);

test("supports mutable assignment", async () => {
    const SILK_CODE_ASSIGN = `
    (export js) test_assign := (val: i32) => (
        mut p := { x = 1, y = 2 };
        p.x = val;
        p.x
    );
    `;
    const { module, cleanup } = await compileAndLoad(SILK_CODE_ASSIGN);
    expect(module.test_assign(42)).toBe(42);
    cleanup();
}, TEST_TIMEOUT_MS);

afterAll(() => {
    try {
        if (existsSync(TEMP_SILK)) unlinkSync(TEMP_SILK);
    } catch (e) { }
});
