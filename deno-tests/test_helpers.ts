import { dirname, fromFileUrl, join, toFileUrl } from "@std/path";

export const ROOT_DIR = fromFileUrl(new URL("..", import.meta.url));
export const FIXTURES_DIR = join(ROOT_DIR, "fixtures");

const decoder = new TextDecoder();

type RunCommandOptions = {
  cwd?: string;
  stdout?: "null" | "piped";
  stderr?: "null" | "piped";
  stdin?: "null" | "piped";
  signal?: AbortSignal;
};

let silkBinaryPromise: Promise<string> | null = null;

export function tempBase(prefix: string) {
  const unique = `${prefix}_${Date.now()}_${
    Math.random().toString(16).slice(2)
  }`;
  const dir = Deno.makeTempDirSync({ prefix: "silk_test_" });
  return join(dir, unique);
}

export async function runCommand(
  command: string,
  args: string[],
  options: RunCommandOptions = {},
) {
  const result = await new Deno.Command(command, {
    args,
    cwd: options.cwd ?? ROOT_DIR,
    stdout: options.stdout ?? "null",
    stderr: options.stderr ?? "piped",
    stdin: options.stdin ?? "null",
    signal: options.signal,
  }).output();
  const stdout = options.stdout === "piped"
    ? decoder.decode(result.stdout)
    : "";
  const stderr = options.stderr === "piped"
    ? decoder.decode(result.stderr)
    : "";
  return {
    code: result.code,
    stdout,
    stderr,
  };
}

async function ensureSilkBinary() {
  if (silkBinaryPromise) {
    return await silkBinaryPromise;
  }

  silkBinaryPromise = (async () => {
    const exeName = Deno.build.os === "windows" ? "silk.exe" : "silk";
    const binaryPath = join(ROOT_DIR, "target", "debug", exeName);
    try {
      const stat = await Deno.stat(binaryPath);
      if (stat.isFile) {
        return binaryPath;
      }
    } catch {
      // Build below.
    }

    const build = await runCommand("cargo", ["build", "--quiet"], {
      stdout: "piped",
      stderr: "piped",
    });
    if (build.code !== 0) {
      const combined = [build.stderr, build.stdout].filter(Boolean).join("");
      throw new Error(`Failed to build silk binary:\n${combined}`);
    }

    return binaryPath;
  })();

  return await silkBinaryPromise;
}

export async function runSilk(
  args: string[],
  options: RunCommandOptions = {},
) {
  const binaryPath = await ensureSilkBinary();
  return await runCommand(binaryPath, args, options);
}

export async function compileSilk(silkCode: string, outputPath: string) {
  const silkPath = outputPath.endsWith(".silk")
    ? outputPath
    : outputPath + ".silk";
  const normalized = silkCode.replaceAll("\r\n", "\n").replaceAll("\r", "\n");
  await Deno.writeTextFile(silkPath, normalized);
  const { code, stdout, stderr } = await runSilk(
    [silkPath, "-o", outputPath],
    { stdout: "piped", stderr: "piped" },
  );
  const combined = [stderr, stdout].filter(Boolean).join("");
  return { code, stderr: combined, silkPath, outputPath };
}

export async function compileToWasm(silkCode: string, prefix: string) {
  const basePath = tempBase(prefix);
  const wasmPath = basePath + ".wasm";
  const { code, stderr, silkPath } = await compileSilk(silkCode, wasmPath);
  if (code !== 0) {
    throw new Error(`Compilation failed:\n${stderr}`);
  }
  return { wasmPath, silkPath };
}

export async function compileToInstance(silkCode: string, prefix: string) {
  const { wasmPath, silkPath } = await compileToWasm(silkCode, prefix);
  const bytes = await Deno.readFile(wasmPath);
  await cleanup([silkPath, wasmPath]);
  const { instance } = await WebAssembly.instantiate(bytes);
  return instance;
}

export async function compileToBase(silkCode: string, prefix: string) {
  const basePath = tempBase(prefix);
  const { code, stderr, silkPath } = await compileSilk(silkCode, basePath);
  return { basePath, code, stderr, silkPath };
}

export async function loadWasm(
  wasmPath: string,
  imports: WebAssembly.Imports = {},
) {
  const bytes = await Deno.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(bytes, imports);
  return instance;
}

export async function importJsModule(jsPath: string) {
  return await import(toFileUrl(jsPath).href);
}

export async function cleanup(paths: string[]) {
  for (const path of paths) {
    try {
      await Deno.remove(path, { recursive: true });
    } catch (err) {
      if (!(err instanceof Deno.errors.NotFound)) {
        throw err;
      }
    }
  }
}

export async function cleanupBase(basePath: string, exts?: string[]) {
  const extensions = exts ?? [".silk", ".wasm", ".js", ".wgsl"];
  await cleanup(extensions.map((ext) => basePath + ext));
  try {
    await Deno.remove(dirname(basePath), { recursive: true });
  } catch (err) {
    if (!(err instanceof Deno.errors.NotFound)) {
      throw err;
    }
  }
}
