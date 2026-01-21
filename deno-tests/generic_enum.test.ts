import { assertEquals } from "@std/asserts";
import { compileToInstance } from "./test_helpers.ts";

Deno.test("generic enum: Option<T>", async () => {
  const source = `
    Option := (T: type) => (
        enum { Some = T, None = {} }
    );

    (export wasm) unwrap_or_zero := (x: i32) => (
        val: Option(i32) := if x > 0 then (
            Option(i32)::Some(x)
        ) else (
            Option(i32)::None
        );

        if Option(i32)::Some(v) := val then (
            v
        ) else (
            0
        )
    );
    `;

  const exports = (await compileToInstance(source, "generic_enum_option"))
    .exports as { unwrap_or_zero: (value: number) => number };
  assertEquals(exports.unwrap_or_zero(10), 10);
  assertEquals(exports.unwrap_or_zero(-5), 0);
});

Deno.test("generic enum: nested generics", async () => {
  const source = `
    Option := (T: type) => (
        enum { Some = T, None = {} }
    );
    Container := (T: type) => (
        enum { Wrapped = Option(T), Empty = {} }
    );

    (export wasm) check := (x: i32) => (
        c := Container(i32)::Wrapped(Option(i32)::Some(x));
        
        if Container(i32)::Wrapped(opt) := c then (
            if Option(i32)::Some(val) := opt then (
                val
            ) else ( 0 )
        ) else ( 0 )
    );
    `;

  const exports = (await compileToInstance(source, "generic_enum_nested"))
    .exports as { check: (value: number) => number };
  assertEquals(exports.check(42), 42);
});
