import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(ROOT_DIR, "fixtures");
const TEMP_WASM = join(FIXTURES_DIR, "structref_temp.wasm");
const TEMP_SILK = join(FIXTURES_DIR, "structref_temp.silk");
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

test("wasm structref: passing and returning structs", async () => {
    const source = `
    let Point: type = { x = i32, y = i32 };
    
    let (export wasm) create_point = ({x: i32, y: i32}) => Point (
        { x = x, y = y }
    );

    let (export wasm) get_x = (p: Point) => i32 (
        p.x
    );

    let (export wasm) get_y = (p: Point) => i32 (
        p.y
    );
    `;

    const instance = await compileToInstance(source);
    const exports = instance.exports as any;

    // Create a point
    const p = exports.create_point(10, 20);

    // Verify it's an object (structref)
    expect(p).toBeTruthy();

    // Verify fields by passing back to WASM
    expect(exports.get_x(p)).toBe(10);
    expect(exports.get_y(p)).toBe(20);
}, TEST_TIMEOUT_MS);

test("wasm structref: nested structs", async () => {
    const source = `
    let Point: type = { x = i32, y = i32 };
    let Rect: type = { top_left = Point, bottom_right = Point };

    let (export wasm) create_rect = ({x1: i32, y1: i32, x2: i32, y2: i32}) => Rect (
        {
            top_left = { x = x1, y = y1 },
            bottom_right = { x = x2, y = y2 },
        }
    );

    let (export wasm) get_width = (r: Rect) => i32 (
        r.bottom_right.x - r.top_left.x
    );
    `;

    const instance = await compileToInstance(source);
    const exports = instance.exports as any;

    const r = exports.create_rect(10, 10, 30, 20);
    expect(exports.get_width(r)).toBe(20);
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
