import { assert, assertEquals } from "@std/asserts";
import { join } from "@std/path";
import { cleanupBase, FIXTURES_DIR, runSilk, tempBase } from "./test_helpers.ts";

Deno.test("boxed enum array initialization compiles", async () => {
  const silkPath = join(FIXTURES_DIR, "boxed_enum_array.silk");
  const basePath = tempBase("boxed_enum_array");
  const outputPath = basePath + ".wasm";

  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), 8000);

  try {
    const result = await runSilk([silkPath, "-o", outputPath], {
      stdout: "null",
      stderr: "piped",
      signal: controller.signal,
    });

    assertEquals(result.code, 0);
    const bytes = await Deno.readFile(outputPath);
    assert(bytes.length > 0);
  } catch (err) {
    if (err instanceof DOMException && err.name === "AbortError") {
      throw new Error(
        "Compilation timed out; boxed enum arrays should compile quickly.",
      );
    }
    throw err;
  } finally {
    clearTimeout(timeout);
    await cleanupBase(basePath);
  }
});
