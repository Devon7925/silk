import { assertEquals } from "https://deno.land/std/testing/asserts.ts";
import { compileToInstance } from "./test_helpers.ts";

Deno.test("supports mutable assignments in wasm exports", async () => {
    const silkCode = `
    (export wasm) increment_twice := (x: i32) => (
        mut total := x;
        total = total + 1;
        total = total + 1;
        total
    );
    {}
  `;

    const { increment_twice } = (await compileToInstance(silkCode, "mut_increment")).exports as any;
    assertEquals(increment_twice(10), 12);
});

Deno.test("propagates mutability through destructured wasm bindings", async () => {
    const silkCode = `
    (export wasm) destructure_mut := {} => (
        mut { first = a, second = b } := { first = 3, second = 4 };
        a = a + b;
        a
    );
    {}
  `;

    const { destructure_mut } = (await compileToInstance(silkCode, "mut_destructure")).exports as any;
    assertEquals(destructure_mut(), 7);
});

Deno.test("struct props can be mutated", async () => {
    const silkCode = `
    (export wasm) destructure_mut := {} => (
        mut foo := { first = 3, second = 4 };
        foo.first = foo.first + foo.second;
        foo.first
    );
    {}
  `;

    const { destructure_mut } = (await compileToInstance(silkCode, "mut_struct_props")).exports as any;
    assertEquals(destructure_mut(), 7);
});

Deno.test("nested dependent struct and tuple props can be mutated", async () => {
    const silkCode = `
    (export wasm) destructure_mut := (bar: i32) => (
        mut foo := { first = {bar, 3}, second = 4 };
        foo.first.0 = foo.first.0 + foo.second;
        foo.first.0
    );
    {}
  `;

    const { destructure_mut } = (await compileToInstance(silkCode, "mut_nested")).exports as any;
    assertEquals(destructure_mut(3), 7);
});

Deno.test("partial mutability in destructured wasm bindings", async () => {
    const silkCode = `
    (export wasm) destructure_mut := {} => (
        { mut a, b } := { 3, 4 };
        a = a + b;
        a
    );
    {}
  `;

    const { destructure_mut } = (await compileToInstance(silkCode, "mut_partial")).exports as any;
    assertEquals(destructure_mut(), 7);
});

Deno.test("allows struct field updates via rebinding", async () => {
    const silkCode = `
    (export wasm) update_struct := {} => (
        mut pair := { first = 2, second = 5 };
        pair = { first = pair.first * 2, second = pair.second };
        pair.first + pair.second
    );
    {}
  `;

    const { update_struct } = (await compileToInstance(silkCode, "mut_rebinding")).exports as any;
    assertEquals(update_struct(), 9);
});

Deno.test("allows dynamic binding annotations for mutability", async () => {
    const silkCode = `
    dyn_annotation := (flag: bool) => (
        if flag then mut else (export wasm)
    );
    (export wasm) set_value := {} => (
        (dyn_annotation true) x := 5;
        x = 10;
        x
    );
    {}
  `;

    const { set_value } = (await compileToInstance(silkCode, "mut_dynamic")).exports as any;
    assertEquals(set_value(), 10);
});
