import { assertEquals, assertStringIncludes } from "@std/asserts";
import {
  cleanup,
  compileSilk,
  compileToBase,
  importJsModule,
  tempBase,
} from "./test_helpers.ts";

async function compileAndLoad(silkCode: string, prefix: string) {
  const { basePath, code, stderr, silkPath } = await compileToBase(
    silkCode,
    prefix,
  );
  if (code !== 0) {
    await cleanup([silkPath, basePath + ".js"]);
    throw new Error(`Compilation failed:\n${stderr}`);
  }
  const jsPath = basePath + ".js";
  try {
    const module = await importJsModule(jsPath);
    return {
      module,
      cleanup: async () => {
        await cleanup([silkPath, jsPath]);
      },
    };
  } catch (err) {
    await cleanup([silkPath, jsPath]);
    throw err;
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

Deno.test("compiles and runs js export", async () => {
  const { module, cleanup } = await compileAndLoad(SILK_CODE, "js_export");
  try {
    assertEquals(module.answer, 42);
    assertEquals(module.add(10, 20), 30);
    assertEquals(module.calculate(10), 21);
  } finally {
    await cleanup();
  }
});

Deno.test("supports if expressions", async () => {
  const SILK_CODE_IF = `
    (export js) check := (x: i32) => (
        if (x > 10) then 1 else 0
    );
    `;
  const { module, cleanup } = await compileAndLoad(SILK_CODE_IF, "js_if");
  try {
    assertEquals(module.check(11), 1);
    assertEquals(module.check(10), 0);
  } finally {
    await cleanup();
  }
});

Deno.test("supports struct objects", async () => {
  const SILK_CODE_STRUCT = `
    (export js) make_point := {x: i32, y: i32} => (
        { x = x, y = y }
    );
    `;
  const { module, cleanup } = await compileAndLoad(
    SILK_CODE_STRUCT,
    "js_struct",
  );
  try {
    const p = module.make_point(10, 20);
    assertEquals(p.x, 10);
    assertEquals(p.y, 20);
  } finally {
    await cleanup();
  }
});

Deno.test("exported function can call internal function", async () => {
  const SILK_CODE_INTERNAL = `
    double := (x: i32) => (x * 2);
    (export js) quadruple := (x: i32) => (double(double(x)));
    `;
  const { module, cleanup } = await compileAndLoad(
    SILK_CODE_INTERNAL,
    "js_internal",
  );
  try {
    assertEquals(module.quadruple(5), 20);
  } finally {
    await cleanup();
  }
});

Deno.test("supports struct property access", async () => {
  const SILK_CODE_PROP = `
    (export js) get_x := (val: i32) => (
        p := { x = val, y = 100 };
        p.x
    );
    `;
  const { module, cleanup } = await compileAndLoad(SILK_CODE_PROP, "js_prop");
  try {
    assertEquals(module.get_x(42), 42);
  } finally {
    await cleanup();
  }
});

Deno.test("supports tagged union struct (enum pattern)", async () => {
  const SILK_CODE_ENUM = `
    Option := (T: type) => (
        enum { Some = T, None = {} }
    );
    (export js) is_some := (val: i32) => (
        opt := if val > 0 then Option(i32)::Some(val) else Option(i32)::None;
        if Option(i32)::Some(v) := opt then 1 else 0
    );
    `;
  const { module, cleanup } = await compileAndLoad(SILK_CODE_ENUM, "js_enum");
  try {
    assertEquals(module.is_some(42), 1);
    assertEquals(module.is_some(-1), 0);
  } finally {
    await cleanup();
  }
});

Deno.test("supports mutable assignment", async () => {
  const SILK_CODE_ASSIGN = `
    (export js) test_assign := (val: i32) => (
        mut p := { x = 1, y = 2 };
        p.x = val;
        p.x
    );
    `;
  const { module, cleanup } = await compileAndLoad(
    SILK_CODE_ASSIGN,
    "js_assign",
  );
  try {
    assertEquals(module.test_assign(42), 42);
  } finally {
    await cleanup();
  }
});

Deno.test("supports boxed values", async () => {
  const SILK_CODE_BOX = `
    Point := { x = Box(i32), y = i32 };
    (export js) sum_boxed := {} => (
        boxed: Box(i32) := 10;
        p: Point := { x = 7, y = 5 };
        boxed + p.x + p.y
    );
    `;
  const { module, cleanup } = await compileAndLoad(SILK_CODE_BOX, "js_box");
  try {
    assertEquals(module.sum_boxed(), 22);
  } finally {
    await cleanup();
  }
});

Deno.test("rejects runtime box allocations in js exports", async () => {
  const SILK_CODE_BOX = `
    (export js) bad_box := (x: i32) => (
        boxed: Box(i32) := x;
        boxed + 1
    );
    `;

  const basePath = tempBase("js_bad_box");
  const outputBase = basePath;
  const { code, stderr, silkPath } = await compileSilk(
    SILK_CODE_BOX,
    outputBase,
  );
  try {
    assertEquals(code, 1);
    assertStringIncludes(stderr, "Box values must be compile-time constants");
  } finally {
    await cleanup([silkPath, outputBase + ".js"]);
  }
});
