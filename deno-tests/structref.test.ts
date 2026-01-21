import { assert, assertEquals } from "https://deno.land/std/testing/asserts.ts";
import { compileToInstance } from "./test_helpers.ts";

Deno.test("wasm structref: passing and returning structs", async () => {
    const source = `
    Point: type := { x = i32, y = i32 };
    
    (export wasm) create_point := ({x: i32, y: i32}) => (
        { x = x, y = y }
    );

    (export wasm) get_x := (p: Point) => (
        p.x
    );

    (export wasm) get_y := (p: Point) => (
        p.y
    );
    `;

    const exports = (await compileToInstance(source, "structref_point")).exports as any;
    const p = exports.create_point(10, 20);
    assert(p);

    assertEquals(exports.get_x(p), 10);
    assertEquals(exports.get_y(p), 20);
});

Deno.test("wasm structref: nested structs", async () => {
    const source = `
    Point: type := { x = i32, y = i32 };
    Rect: type := { top_left = Point, bottom_right = Point };

    (export wasm) create_rect := ({x1: i32, y1: i32, x2: i32, y2: i32}) => (
        {
            top_left = { x = x1, y = y1 },
            bottom_right = { x = x2, y = y2 },
        }
    );

    (export wasm) get_width := (r: Rect) => (
        r.bottom_right.x - r.top_left.x
    );
    `;

    const exports = (await compileToInstance(source, "structref_rect")).exports as any;
    const r = exports.create_rect(10, 10, 30, 20);
    assertEquals(exports.get_width(r), 20);
});
