import { assertEquals } from "https://deno.land/std/testing/asserts.ts";
import { compileToInstance } from "./test_helpers.ts";

Deno.test("u8 values", async () => {
    const code = `
    (export wasm) add_one := (value: u8) => ( value + 1 );
    (export wasm) add_pair := ({left: u8, right: u8}) => ( left + right );
    {}
    `;

    const exports = (await compileToInstance(code, "u8_values")).exports as any;
    assertEquals(exports.add_one(41), 42);
    assertEquals(exports.add_pair(12, 34), 46);
});
