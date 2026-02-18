import { dirname, fromFileUrl, join, toFileUrl } from "@std/path";

export const ROOT_DIR = fromFileUrl(new URL("..", import.meta.url));
export const FIXTURES_DIR = join(ROOT_DIR, "fixtures");

const decoder = new TextDecoder();
const encoder = new TextEncoder();

const WASM_CACHE_DIR = join(ROOT_DIR, "tmp", "wasm_cache");
const BUILD_INPUT_PATHS = ["Cargo.toml", "Cargo.lock", "src", "silk_src"] as const;

async function ensureWasmCacheDir() {
  await Deno.mkdir(WASM_CACHE_DIR, { recursive: true });
}

function toHex(buffer: ArrayBuffer) {
  return Array.from(new Uint8Array(buffer))
    .map((byte) => byte.toString(16).padStart(2, "0"))
    .join("");
}

async function hashWasmInputs(
  silkCode: string,
  extraFiles: Record<string, string>,
) {
  const entries = Object.entries(extraFiles).sort(([a], [b]) =>
    a.localeCompare(b)
  );
  let payload = `silk:${silkCode}\0`;
  for (const [name, contents] of entries) {
    payload += `file:${name}\0${contents}\0`;
  }
  const digest = await crypto.subtle.digest("SHA-256", encoder.encode(payload));
  return toHex(digest);
}

async function fileExists(path: string) {
  try {
    const stat = await Deno.stat(path);
    return stat.isFile;
  } catch {
    return false;
  }
}

async function fileStat(path: string) {
  try {
    const stat = await Deno.stat(path);
    return stat.isFile ? stat : null;
  } catch {
    return null;
  }
}

async function newestPathMtimeMs(path: string): Promise<number> {
  try {
    const stat = await Deno.stat(path);
    let latest = stat.mtime?.getTime() ?? 0;
    if (stat.isDirectory) {
      for await (const entry of Deno.readDir(path)) {
        if (
          entry.name === ".git" || entry.name === "target" ||
          entry.name === "node_modules"
        ) {
          continue;
        }
        const childPath = join(path, entry.name);
        const childLatest = await newestPathMtimeMs(childPath);
        if (childLatest > latest) {
          latest = childLatest;
        }
      }
    }
    return latest;
  } catch {
    return 0;
  }
}

async function newestBuildInputMtimeMs() {
  let latest = 0;
  for (const relativePath of BUILD_INPUT_PATHS) {
    const absolutePath = join(ROOT_DIR, relativePath);
    const inputLatest = await newestPathMtimeMs(absolutePath);
    if (inputLatest > latest) {
      latest = inputLatest;
    }
  }
  return latest;
}

type RunCommandOptions = {
  cwd?: string;
  stdout?: "null" | "piped";
  stderr?: "null" | "piped";
  stdin?: "null" | "piped";
  signal?: AbortSignal;
};

let silkBinaryPromise: Promise<string> | null = null;

function normalizeLineEndings(text: string) {
  return text.replaceAll("\r\n", "\n").replaceAll("\r", "\n");
}

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
    let override: string | undefined;
    try {
      override = Deno.env.get("SILK_BINARY") ?? undefined;
    } catch {
      override = undefined;
    }
    if (override) {
      return override;
    }
    const exeName = Deno.build.os === "windows" ? "silk.exe" : "silk";
    const releasePath = join(ROOT_DIR, "target", "release", exeName);
    const debugPath = join(ROOT_DIR, "target", "debug", exeName);
    const [releaseStat, debugStat] = await Promise.all([
      fileStat(releasePath),
      fileStat(debugPath),
    ]);
    const sourceMtime = await newestBuildInputMtimeMs();
    const releaseTime = releaseStat?.mtime?.getTime() ?? 0;
    const debugTime = debugStat?.mtime?.getTime() ?? 0;

    const releaseFresh = !!releaseStat && releaseTime >= sourceMtime;
    const debugFresh = !!debugStat && debugTime >= sourceMtime;

    if (releaseFresh && debugFresh) {
      return debugTime > releaseTime ? debugPath : releasePath;
    }
    if (debugFresh) {
      return debugPath;
    }
    if (releaseFresh) {
      return releasePath;
    }

    const buildRelease = await runCommand("cargo", ["build", "--release", "--quiet"], {
      stdout: "piped",
      stderr: "piped",
    });
    if (buildRelease.code === 0) {
      return releasePath;
    }

    const buildDebug = await runCommand("cargo", ["build", "--quiet"], {
      stdout: "piped",
      stderr: "piped",
    });
    if (buildDebug.code !== 0) {
      if (releaseStat && debugStat) {
        return debugTime > releaseTime ? debugPath : releasePath;
      }
      if (debugStat) {
        return debugPath;
      }
      if (releaseStat) {
        return releasePath;
      }
      const combined = [
        buildRelease.stderr,
        buildRelease.stdout,
        buildDebug.stderr,
        buildDebug.stdout,
      ].filter(Boolean).join("");
      throw new Error(`Failed to build silk binary:\n${combined}`);
    }

    return debugPath;
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
  const normalized = normalizeLineEndings(silkCode);
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

export async function compileToWasmWithExtraFiles(
  silkCode: string,
  prefix: string,
  extraFiles: Record<string, string>,
) {
  const normalized = normalizeLineEndings(silkCode);
  const normalizedExtras: Record<string, string> = {};
  for (const [name, contents] of Object.entries(extraFiles)) {
    normalizedExtras[name] = normalizeLineEndings(contents);
  }
  await ensureWasmCacheDir();
  const cacheKey = await hashWasmInputs(normalized, normalizedExtras);
  const cacheBase = join(WASM_CACHE_DIR, `${prefix}_${cacheKey}`);
  const cacheWasmPath = cacheBase + ".wasm";

  const basePath = tempBase(prefix);
  const wasmPath = basePath + ".wasm";
  const silkPath = basePath + ".silk";
  const dir = dirname(silkPath);
  const extraPaths: string[] = [];
  await Deno.writeTextFile(silkPath, normalized);
  for (const [name, contents] of Object.entries(normalizedExtras)) {
    const extraPath = join(dir, name);
    await Deno.writeTextFile(extraPath, contents);
    extraPaths.push(extraPath);
  }

  if (await fileExists(cacheWasmPath)) {
    await Deno.copyFile(cacheWasmPath, wasmPath);
    return { wasmPath, silkPath, extraPaths };
  }

  const { code, stdout, stderr } = await runSilk(
    [silkPath, "-o", wasmPath],
    { stdout: "piped", stderr: "piped" },
  );
  const combined = [stderr, stdout].filter(Boolean).join("");
  if (code !== 0) {
    await cleanup([silkPath, wasmPath, ...extraPaths]);
    throw new Error(`Compilation failed:\n${combined}`);
  }
  await Deno.copyFile(wasmPath, cacheWasmPath);
  return { wasmPath, silkPath, extraPaths };
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
