import { assert, assertEquals, assertInstanceOf } from "@std/asserts";
import { join } from "@std/path";
import { ROOT_DIR, runCommand } from "./test_helpers.ts";

async function runSilk(args: string[]) {
  const result = await runCommand("cargo", ["run", "--quiet", "--", ...args], {
    stdout: "piped",
    stderr: "piped",
  });

  return {
    exitCode: result.code,
    stdout: result.stdout,
    stderr: result.stderr,
  };
}

async function compileFixtureToBytes(fixture: string) {
  const tmpDir = await Deno.makeTempDir({ prefix: "silk-cli-" });
  const programPath = join(ROOT_DIR, "fixtures", fixture);
  const outputPath = join(tmpDir, "module.wasm");

  try {
    const result = await runSilk([programPath, "--output", outputPath]);
    if (result.exitCode !== 0) {
      console.error("Compilation failed:", result.stderr);
      console.error("With stdout:", result.stdout);
    }
    assertEquals(result.exitCode, 0);
    const bytes = await Deno.readFile(outputPath);
    assert(bytes.length > 0);
    return bytes;
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
}

Deno.test("compiles a wasm export to a file", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "silk-cli-" });
  const programPath = join(ROOT_DIR, "fixtures", "wasm_export.silk");
  const outputPath = join(tmpDir, "answer.wasm");

  try {
    const result = await runSilk([programPath, "--output", outputPath]);
    assertEquals(result.exitCode, 0);

    const moduleBytes = await Deno.readFile(outputPath);
    assert(moduleBytes.length > 0);
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

Deno.test("compiles a global wasm export", async () => {
  const moduleBytes = await compileFixtureToBytes("invalid_export.silk");
  const { instance } = await WebAssembly.instantiate(moduleBytes);
  const answer = instance.exports.answer as WebAssembly.Global;

  assertInstanceOf(answer, WebAssembly.Global);
  assertEquals(answer.value, 42);
});

Deno.test("runs wasm_export module and returns const value", async () => {
  const moduleBytes = await compileFixtureToBytes("wasm_export.silk");
  const { instance } = await WebAssembly.instantiate(moduleBytes);
  const answer = instance.exports.answer as () => number;

  assertEquals(typeof answer, "function");
  assertEquals(answer(), 42);
});

Deno.test("runs add_one module and returns incremented value", async () => {
  const moduleBytes = await compileFixtureToBytes("add_one.silk");
  const { instance } = await WebAssembly.instantiate(moduleBytes);
  const addOne = instance.exports.add_one as (value: number) => number;

  assertEquals(typeof addOne, "function");
  assertEquals(addOne(41), 42);
  assertEquals(addOne(-1), 0);
});

Deno.test("runs pass_pair module and returns correct value", async () => {
  const moduleBytes = await compileFixtureToBytes("pass_pair.silk");
  const { instance } = await WebAssembly.instantiate(moduleBytes);
  const constructRange = instance.exports.construct_range as (
    max: number,
  ) => unknown;
  const rangeSum = instance.exports.sum_range as (range: unknown) => number;

  assertEquals(typeof rangeSum, "function");
  assertEquals(rangeSum(constructRange(1)), 1);
  assertEquals(rangeSum(constructRange(3)), 6);
  assertEquals(rangeSum(constructRange(4)), 10);
});

Deno.test("runs range_sum module and returns correct value", async () => {
  const moduleBytes = await compileFixtureToBytes("range_sum.silk");
  const { instance } = await WebAssembly.instantiate(moduleBytes);
  const constructRange = instance.exports.construct_range as (
    min: number,
    max: number,
  ) => unknown;
  const rangeSum = instance.exports.sum_range as (range: unknown) => number;

  assertEquals(typeof constructRange, "function");
  assertEquals(typeof rangeSum, "function");
  assertEquals(rangeSum(constructRange(0, 1)), 1);
  assertEquals(rangeSum(constructRange(1, 3)), 5);
});

Deno.test("runs binding_in_function module and returns correct value", async () => {
  const moduleBytes = await compileFixtureToBytes("binding_in_function.silk");
  const { instance } = await WebAssembly.instantiate(moduleBytes);
  const addOneSquared = instance.exports.add_one_squared as (
    value: number,
  ) => number;

  assertEquals(typeof addOneSquared, "function");
  assertEquals(addOneSquared(-1), 0);
  assertEquals(addOneSquared(10), 121);
});
