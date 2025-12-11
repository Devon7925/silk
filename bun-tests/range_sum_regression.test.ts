import { expect, test } from "bun:test";
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

test("generic enum destructuring still fails wasm lowering", () => {
  const programPath = join(projectRoot, "fixtures", "generic_option_while.silk");
  const result = runSilk([programPath]);

  expect(result.exitCode).not.toBe(0);
  expect(result.stderr).toContain("Enum pattern requires struct-backed enum type");
});
