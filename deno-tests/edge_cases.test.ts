import {
    assertEquals,
    assertStringIncludes,
    assertThrows,
} from "https://deno.land/std/testing/asserts.ts";
import {
    cleanup,
    compileSilk,
    compileToInstance,
    tempBase,
} from "./test_helpers.ts";

Deno.test("shadowing in block", async () => {
    const silkCode = `
    (export wasm) shadow_test := (x: i32) => (
        y := 10;
        (
            y := 20;
            x + y
        )
    );
    {}
    `;
    const exports = (await compileToInstance(silkCode, "edge_shadow")).exports as any;
    assertEquals(exports.shadow_test(5), 25);
});

Deno.test("multiple exports", async () => {
    const silkCode = `
    (export wasm) add := (x: i32) => ( x + 1 );
    (export wasm) sub := (x: i32) => ( x - 1 );
    {}
    `;
    const exports = (await compileToInstance(silkCode, "edge_exports")).exports as any;
    assertEquals(exports.add(10), 11);
    assertEquals(exports.sub(10), 9);
});

Deno.test("arithmetic edge cases", async () => {
    const silkCode = `
    (export wasm) div_test := (x: i32) => ( 100 / x );
    {}
    `;
    const exports = (await compileToInstance(silkCode, "edge_arithmetic")).exports as any;
    assertEquals(exports.div_test(2), 50);
    assertThrows(() => exports.div_test(0));
});

Deno.test("deeply nested bindings", async () => {
    const silkCode = `
    (export wasm) nested_test := (x: i32) => (
        a := x + 1;
        b := a * 2;
        c := b + a; 
        c
    );
    {}
    `;
    const exports = (await compileToInstance(silkCode, "edge_nested")).exports as any;
    assertEquals(exports.nested_test(2), 9);
});

Deno.test("if expressions evaluate and type check", async () => {
    const silkCode = `
    (export wasm) choose := (flag: bool) => (
        if flag then 10 else 20
    );
    (export wasm) ladder := (flag: bool) => (
        if false then 1 else if flag then 2 else 3
    );
    {} 
    `;
    const exports = (await compileToInstance(silkCode, "edge_if")).exports as any;
    assertEquals(exports.choose(1), 10);
    assertEquals(exports.choose(0), 20);
    assertEquals(exports.ladder(1), 2);
    assertEquals(exports.ladder(0), 3);
});

Deno.test("if branch type mismatches are reported", async () => {
    const silkCode = `
    (export wasm) bad := {} => (
        if true then ( 1; ) else ( 2 )
    );
    {}
    `;

    const basePath = tempBase("edge_bad_if");
    const outputPath = basePath + ".wasm";
    const { code, stderr, silkPath } = await compileSilk(silkCode, outputPath);
    try {
        assertEquals(code, 1);
        assertStringIncludes(stderr, "Type mismatch between if branches");
    } finally {
        await cleanup([silkPath, outputPath]);
    }
});
