import { expect, test } from "bun:test";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { TextDecoder } from "node:util";

const testDir = dirname(fileURLToPath(import.meta.url));
const projectRoot = dirname(testDir);
const decoder = new TextDecoder();
const TEST_TIMEOUT_MS = 20000;

function runSilk(args: string[]) {
  const result = Bun.spawnSync({
    cmd: ["cargo", "run", "--quiet", "--", ...args],
    cwd: projectRoot,
    stdout: "pipe",
    stderr: "pipe",
  });

  return {
    exitCode: result.exitCode,
    stdout: decoder.decode(result.stdout),
    stderr: decoder.decode(result.stderr),
  };
}

function compileFixtureToBytes(fixture: string) {
  const tmpDir = mkdtempSync(join(tmpdir(), "silk-cli-"));
  const programPath = join(projectRoot, "fixtures", fixture);
  const outputPath = join(tmpDir, "module.wasm");

  try {
    const result = runSilk([programPath, "--output", outputPath]);
    if (result.exitCode !== 0) {
      console.error("Compilation failed:", result.stderr);
    }
    expect(result.exitCode).toBe(0);
    const bytes = readFileSync(outputPath);
    expect(bytes.length).toBeGreaterThan(0);
    return bytes;
  } finally {
    rmSync(tmpDir, { recursive: true, force: true });
  }
}

test("compiles a wasm export to a file", () => {
  const tmpDir = mkdtempSync(join(tmpdir(), "silk-cli-"));
  const programPath = join(projectRoot, "fixtures", "wasm_export.silk");
  const outputPath = join(tmpDir, "answer.wasm");

  try {
    const result = runSilk([programPath, "--output", outputPath]);
    expect(result.exitCode).toBe(0);

    const moduleBytes = readFileSync(outputPath);
    expect(moduleBytes.length).toBeGreaterThan(0);
  } finally {
    rmSync(tmpDir, { recursive: true, force: true });
  }
}, TEST_TIMEOUT_MS);

test("prints diagnostics for invalid programs", () => {
  const invalidPath = join(projectRoot, "fixtures", "invalid_export.silk");
  const result = runSilk([invalidPath]);

  expect(result.exitCode).not.toBe(0);
  expect(result.stderr).toContain("Compilation failed for");
  expect(result.stderr).toContain("Only functions can be exported to wasm");
}, TEST_TIMEOUT_MS);

test("runs wasm_export module and returns const value", async () => {
  const moduleBytes = compileFixtureToBytes("wasm_export.silk");
  const { instance } = await WebAssembly.instantiate(moduleBytes);
  const answer = instance.exports.answer as () => number;

  expect(typeof answer).toBe("function");
  expect(answer()).toBe(42);
}, TEST_TIMEOUT_MS);

test("runs add_one module and returns incremented value", async () => {
  const moduleBytes = compileFixtureToBytes("add_one.silk");
  const { instance } = await WebAssembly.instantiate(moduleBytes);
  const addOne = instance.exports.add_one as (value: number) => number;

  expect(typeof addOne).toBe("function");
  expect(addOne(41)).toBe(42);
  expect(addOne(-1)).toBe(0);
}, TEST_TIMEOUT_MS);

test("runs binding_in_function module and returns correct value", async () => {
  const moduleBytes = compileFixtureToBytes("binding_in_function.silk");
  const { instance } = await WebAssembly.instantiate(moduleBytes);
  const addOneSquared = instance.exports.add_one_squared as (value: number) => number;

  expect(typeof addOneSquared).toBe("function");
  expect(addOneSquared(-1)).toBe(0);
  expect(addOneSquared(10)).toBe(121);
}, TEST_TIMEOUT_MS);
