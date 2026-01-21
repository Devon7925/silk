import { assertEquals } from "@std/asserts";
import { compileToInstance } from "./test_helpers.ts";

Deno.test("match literal number", async () => {
  const code = `
      (export wasm) main := {} => (
        x := 10;
        x |> match {
          10 => 1,
          20 => 2,
          else => 3
        }
      );
      {}
    `;
  const exports = (await compileToInstance(code, "match_literal"))
    .exports as { main: () => number };
  assertEquals(exports.main(), 1);
});

Deno.test("match literal number else", async () => {
  const code = `
      (export wasm) main := {} => (
        x := 30;
        x |> match {
          10 => 1,
          20 => 2,
          else => 3
        }
      );
      {}
    `;
  const exports = (await compileToInstance(code, "match_literal_else"))
    .exports as { main: () => number };
  assertEquals(exports.main(), 3);
});

Deno.test("match boolean", async () => {
  const code = `
      (export wasm) main := {} => (
        x := true;
        x |> match {
          true => 1,
          false => 0
        }
      );
      {}
    `;
  const exports = (await compileToInstance(code, "match_boolean"))
    .exports as { main: () => number };
  assertEquals(exports.main(), 1);
});

Deno.test("match enum variant without payload", async () => {
  const code = `
      Color := enum { Red = {}, Green = {}, Blue = {} };

      (export wasm) main := {} => (
        c := Color::Green;
        c |> match {
          Color::Red => 1,
          Color::Green => 2,
          Color::Blue => 3
        }
      );
      {}
    `;
  const exports = (await compileToInstance(code, "match_enum_simple"))
    .exports as { main: () => number };
  assertEquals(exports.main(), 2);
});

Deno.test("match enum variant with payload", async () => {
  const code = `
      Result := enum { Ok = i32, Err = i32 };

      (export wasm) main := {} => (
        r := Result::Ok(42);
        r |> match {
          Result::Ok(v) => v,
          Result::Err(e) => e
        }
      );
      {}
    `;
  const exports = (await compileToInstance(code, "match_enum_payload"))
    .exports as { main: () => number };
  assertEquals(exports.main(), 42);
});

Deno.test("match enum variant with payload else", async () => {
  const code = `
      Result := enum { Ok = i32, Err = i32 };

      (export wasm) main := {} => (
        r := Result::Err(10);
        r |> match {
          Result::Ok(v) => v,
          else => 0
        }
      );
      {}
    `;
  const exports = (await compileToInstance(code, "match_enum_else"))
    .exports as { main: () => number };
  assertEquals(exports.main(), 0);
});

Deno.test("match with block bodies", async () => {
  const code = `
      (export wasm) main := {} => (
        x := 1;
        x |> match {
          1 => (
            y := 2;
            y + 1
          ),
          else => 0
        }
      );
      {}
    `;
  const exports = (await compileToInstance(code, "match_block"))
    .exports as { main: () => number };
  assertEquals(exports.main(), 3);
});
