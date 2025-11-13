import { expect, test } from "bun:test";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { TextDecoder } from "node:util";

const testDir = dirname(fileURLToPath(import.meta.url));
const projectRoot = dirname(testDir);
const decoder = new TextDecoder();

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
});

test("prints diagnostics for invalid programs", () => {
  const invalidPath = join(projectRoot, "fixtures", "invalid_export.silk");
  const result = runSilk([invalidPath]);

  expect(result.exitCode).not.toBe(0);
  expect(result.stderr).toContain("Compilation failed for");
  expect(result.stderr).toContain("Only functions can be exported to wasm");
});
