import { describe, expect, test, afterAll } from "bun:test";

import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(ROOT_DIR, "fixtures");
const TEMP_FILES = new Set<string>();
const TEST_TIMEOUT_MS = 20000;

async function compileAndLoad(silkCode: string, basename: string) {
  const wasmPath = join(FIXTURES_DIR, `${basename}.wasm`);
  const silkPath = join(FIXTURES_DIR, `${basename}.silk`);
  TEMP_FILES.add(wasmPath);
  TEMP_FILES.add(silkPath);

  writeFileSync(silkPath, silkCode);

  const proc = Bun.spawn(["cargo", "run", "--", silkPath, "-o", wasmPath], {
    cwd: ROOT_DIR,
    stderr: "pipe",
  });

  const exitCode = await proc.exited;
  if (exitCode !== 0) {
    const stderr = await new Response(proc.stderr).text();
    throw new Error(`Compilation failed:\n${stderr}`);
  }

  const wasmBuffer = await Bun.file(wasmPath).arrayBuffer();
  const { instance } = await WebAssembly.instantiate(wasmBuffer);
  return instance.exports as any;
}

describe("match expressions", () => {
  test("match literal number", async () => {
    const code = `
      (export wasm) main := {} => (
        x := 10;
        x |> match {
          10 => 1,
          20 => 2,
          else => 3
        }
      );
      {}
    `;
    const exports = await compileAndLoad(code, "match_literal");
    expect(exports.main()).toBe(1);
  });

  test("match literal number else", async () => {
    const code = `
      (export wasm) main := {} => (
        x := 30;
        x |> match {
          10 => 1,
          20 => 2,
          else => 3
        }
      );
      {}
    `;
    const exports = await compileAndLoad(code, "match_literal_else");
    expect(exports.main()).toBe(3);
  });

  test("match boolean", async () => {
    const code = `
      (export wasm) main := {} => (
        x := true;
        x |> match {
          true => 1,
          false => 0
        }
      );
      {}
    `;
    const exports = await compileAndLoad(code, "match_boolean");
    expect(exports.main()).toBe(1);
  });

  test("match enum variant without payload", async () => {
    const code = `
      Color := enum { Red = {}, Green = {}, Blue = {} };

      (export wasm) main := {} => (
        c := Color::Green;
        c |> match {
          Color::Red => 1,
          Color::Green => 2,
          Color::Blue => 3
        }
      );
      {}
    `;
    const exports = await compileAndLoad(code, "match_enum_simple");
    expect(exports.main()).toBe(2);
  });

  test("match enum variant with payload", async () => {
    const code = `
      Result := enum { Ok = i32, Err = i32 };

      (export wasm) main := {} => (
        r := Result::Ok(42);
        r |> match {
          Result::Ok(v) => v,
          Result::Err(e) => e
        }
      );
      {}
    `;
    const exports = await compileAndLoad(code, "match_enum_payload");
    expect(exports.main()).toBe(42);
  });

  test("match enum variant with payload else", async () => {
    const code = `
      Result := enum { Ok = i32, Err = i32 };

      (export wasm) main := {} => (
        r := Result::Err(10);
        r |> match {
          Result::Ok(v) => v,
          else => 0
        }
      );
      {}
    `;
    const exports = await compileAndLoad(code, "match_enum_else");
    expect(exports.main()).toBe(0);
  });

  test("match with block bodies", async () => {
    const code = `
      (export wasm) main := {} => (
        x := 1;
        x |> match {
          1 => (
            y := 2;
            y + 1
          ),
          else => 0
        }
      );
      {}
    `;
    const exports = await compileAndLoad(code, "match_block");
    expect(exports.main()).toBe(3);
  });


  afterAll(() => {
    for (const file of TEMP_FILES) {
      try {
        unlinkSync(file);
      } catch (e) { }
    }
  });
});
