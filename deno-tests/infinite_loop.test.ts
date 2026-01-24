import { assert } from "@std/asserts";
import { cleanupBase, FIXTURES_DIR, ROOT_DIR, tempBase } from "./test_helpers.ts";
import { join } from "@std/path";

Deno.test("compiler does not hang on infinite loop evaluation in function bodies", async () => {
  const silkPath = join(FIXTURES_DIR, "infinite_loop.silk");
  const basePath = tempBase("infinite_loop");
  const outputPath = basePath + ".wasm";

  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), 8000);

  let completed = false;
  try {
    const _result = await new Deno.Command("cargo", {
      args: ["run", "--", silkPath, "-o", outputPath],
      cwd: ROOT_DIR,
      stdout: "null",
      stderr: "piped",
      signal: controller.signal,
    }).output();

    completed = true;
  } catch (err) {
    if (err instanceof DOMException && err.name === "AbortError") {
      throw new Error(
        "Compilation timed out; infinite loop evaluation is likely executing at compile time.",
      );
    }
    throw err;
  } finally {
    clearTimeout(timeout);
    await cleanupBase(basePath);
  }

  assert(completed);
});
