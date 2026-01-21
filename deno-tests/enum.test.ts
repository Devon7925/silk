import {
  assertEquals,
  assertNotEquals,
  assertStringIncludes,
} from "https://deno.land/std/testing/asserts.ts";
import {
  cleanup,
  compileSilk,
  compileToInstance,
  tempBase,
} from "./test_helpers.ts";

async function compileExpectError(silkCode: string, prefix: string) {
  const basePath = tempBase(prefix);
  const outputPath = basePath + ".wasm";
  const { code, stderr, silkPath } = await compileSilk(silkCode, outputPath);
  try {
    return { code, stderr };
  } finally {
    await cleanup([silkPath, outputPath]);
  }
}

Deno.test("enum construction and matching", async () => {
  const silkCode = `
    IntOption := enum { Some = i32, None = {} };
    (export wasm) unwrap_or_zero := (x: i32) => (
        value := if x > 0 then (
            IntOption::Some(x)
        ) else (
            IntOption::None
        );

        if IntOption::Some(v) := value then (
            v
        ) else (
            0
        )
    );
    {} 
    `;

  const exports = (await compileToInstance(silkCode, "enum_match"))
    .exports as any;
  assertEquals(exports.unwrap_or_zero(3), 3);
  assertEquals(exports.unwrap_or_zero(-2), 0);
});

Deno.test("enum rejects value payloads", async () => {
  const silkCode = `
    Bad := enum { Value = 1 };
    {};
    `;

  const { code, stderr } = await compileExpectError(
    silkCode,
    "enum_bad_payload",
  );
  assertNotEquals(code, 0);
  assertStringIncludes(stderr, "Enum variant payload must be a type");
});

Deno.test("enum intrinsic can be aliased", async () => {
  const silkCode = `
    EnumFactory := enum;
    Flag := EnumFactory { On = {}, Off = {} };
    (export wasm) as_bool := {flag: i32} => (
        value := if flag > 0 then (
            Flag::On
        ) else (
            Flag::Off
        );

        if Flag::On := value then (
            1
        ) else (
            0
        )
    );
    {};
    `;

  const exports = (await compileToInstance(silkCode, "enum_alias"))
    .exports as any;
  assertEquals(exports.as_bool(1), 1);
  assertEquals(exports.as_bool(-1), 0);
});

Deno.test("enum patterns require defined enum types", async () => {
  const silkCode = `
    Opt := enum { Some = i32, None = {} };
    (export wasm) demo := {} => (
        value := Opt::Some(1);
        if Missing::Some(v) := value then ( v ) else ( 0 )
    );
    {};
    `;

  const { code, stderr } = await compileExpectError(silkCode, "enum_missing");
  assertNotEquals(code, 0);
  assertStringIncludes(stderr, "Unbound identifier: Missing");
});

Deno.test("enum patterns respect variant enum types", async () => {
  const silkCode = `
    First := enum { Some = i32, None = {} };
    Second := enum { Some = {}, None = {} };
    (export wasm) check := {} => (
        value := First::Some(3);
        if Second::Some := value then ( 1 ) else ( 0 )
    );
    {};
    `;

  const exports = (await compileToInstance(silkCode, "enum_variant"))
    .exports as any;
  assertEquals(exports.check(), 0);
});
