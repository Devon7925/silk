import {
  assertEquals,
  assertStringIncludes,
} from "https://deno.land/std/testing/asserts.ts";
import {
  cleanup,
  compileSilk,
  compileToInstance,
  tempBase,
} from "./test_helpers.ts";

Deno.test("allows matching target bindings", async () => {
  const silkCode = `
    (target wasm) base := 40;
    (export wasm) (target wasm) add_two := {} => (
        base + 2
    );
    {}
  `;

  const { add_two } = (await compileToInstance(silkCode, "target_match"))
    .exports as any;
  assertEquals(add_two(), 42);
});

Deno.test("allows bindings with fewer target annotations to use shared bindings", async () => {
  const silkCode = `
    (target wasm) (target js) shared := 21;
    (export wasm) (target wasm) double_shared := {} => (
        shared + shared
    );
    {}
  `;

  const { double_shared } = (await compileToInstance(silkCode, "target_shared"))
    .exports as any;
  assertEquals(double_shared(), 42);
});

Deno.test("rejects unannotated bindings using target bindings", async () => {
  const silkCode = `
    (target wasm) secret := 7;
    (export wasm) reveal := {} => (
        secret
    );
    {}
  `;

  const basePath = tempBase("target_unannotated");
  const outputPath = basePath + ".wasm";
  const { code, stderr, silkPath } = await compileSilk(silkCode, outputPath);
  try {
    assertEquals(code, 1);
    assertStringIncludes(
      stderr,
      "Cannot use target binding secret without a target annotation",
    );
  } finally {
    await cleanup([silkPath, outputPath]);
  }
});

Deno.test("rejects bindings with extra target annotations using narrower bindings", async () => {
  const silkCode = `
    (target wasm) only_wasm := 5;
    (export wasm) (target wasm) (target js) bad := {} => (
        only_wasm
    );
    {}
  `;

  const basePath = tempBase("target_narrow");
  const outputPath = basePath + ".wasm";
  const { code, stderr, silkPath } = await compileSilk(silkCode, outputPath);
  try {
    assertEquals(code, 1);
    assertStringIncludes(
      stderr,
      "Binding only_wasm requires target annotation(s): wasm",
    );
  } finally {
    await cleanup([silkPath, outputPath]);
  }
});
