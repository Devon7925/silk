import { assertEquals } from "@std/asserts";
import { basename, dirname, join } from "@std/path";
import { cleanup, runCommand, tempBase } from "./test_helpers.ts";

async function compileAndLoad<T extends WebAssembly.Exports>(
  mainPath: string,
  wasmPath: string,
) {
  const { code, stderr } = await runCommand("cargo", [
    "run",
    "--",
    mainPath,
    "-o",
    wasmPath,
  ]);
  if (code !== 0) {
    throw new Error(`Compilation failed:\n${stderr}`);
  }
  const wasmBuffer = await Deno.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBuffer);
  return instance.exports as T;
}

async function setupUseFiles(
  mainContents: (moduleName: string) => string,
  moduleContents: string,
) {
  const basePath = tempBase("use_demo");
  const dir = dirname(basePath);
  const moduleName = `${basename(basePath)}_module.silk`;
  const mainName = `${basename(basePath)}_main.silk`;
  const mainPath = join(dir, mainName);
  const modulePath = join(dir, moduleName);

  await Deno.writeTextFile(mainPath, mainContents(moduleName));
  await Deno.writeTextFile(modulePath, moduleContents);

  return { mainPath, modulePath, wasmPath: basePath + ".wasm" };
}

Deno.test("use imports block expressions from other files", async () => {
  const { mainPath, modulePath, wasmPath } = await setupUseFiles(
    (moduleName) => `
        lib := use "${moduleName}";
        (export wasm) answer := {} => ( lib.answer );
        {}
        `,
    `
        {
            answer = 40 + 2
        }
        `,
  );

  try {
    const exports = await compileAndLoad<{ answer: () => number }>(
      mainPath,
      wasmPath,
    );
    assertEquals(exports.answer(), 42);
  } finally {
    await cleanup([mainPath, modulePath, wasmPath]);
  }
});

Deno.test("use imports functions from other files", async () => {
  const { mainPath, modulePath, wasmPath } = await setupUseFiles(
    (moduleName) => `
        lib := use "${moduleName}";
        (export wasm) double_answer := {} => ( lib.double 21 );
        {}
        `,
    `
        {
            double = (value: i32) => ( value * 2 )
        }
        `,
  );

  try {
    const exports = await compileAndLoad<{ double_answer: () => number }>(
      mainPath,
      wasmPath,
    );
    assertEquals(exports.double_answer(), 42);
  } finally {
    await cleanup([mainPath, modulePath, wasmPath]);
  }
});
