import { test, expect, afterAll } from "bun:test";
import { join } from "path";
import { writeFileSync, unlinkSync } from "fs";

const ROOT_DIR = join(import.meta.dir, "..");
const FIXTURES_DIR = join(import.meta.dir, "..", "fixtures");
const TEMP_WGSL = join(FIXTURES_DIR, "temp_bindings.wgsl");
const TEMP_SILK = join(FIXTURES_DIR, "temp_bindings.silk");
const TEST_TIMEOUT_MS = 20000;

async function hasWebGpu() {
    const proc = Bun.spawn(
        [
            "deno",
            "eval",
            "const adapter = await navigator.gpu?.requestAdapter(); Deno.exit(adapter ? 0 : 2);",
        ],
        {
            cwd: ROOT_DIR,
            stderr: "pipe",
        },
    );
    const exitCode = await proc.exited;
    if (exitCode === 0) {
        return true;
    }
    if (exitCode === 2) {
        return false;
    }
    const stderr = await new Response(proc.stderr).text();
    if (stderr) {
        console.error(stderr);
    }
    return false;
}

const HAS_WEBGPU = await hasWebGpu();
const wgslTest = HAS_WEBGPU ? test : test.skip;

async function compileToWgsl(silkCode: string) {
    writeFileSync(TEMP_SILK, silkCode);

    const proc = Bun.spawn(["cargo", "run", "--", TEMP_SILK, "-o", TEMP_WGSL], {
        cwd: ROOT_DIR,
        stderr: "pipe",
    });

    const exitCode = await proc.exited;
    if (exitCode !== 0) {
        const stderr = await new Response(proc.stderr).text();
        throw new Error(`Compilation failed:\n${stderr}`);
    }

    return await Bun.file(TEMP_WGSL).text();
}

function extractBinding(wgsl: string, name: string) {
    const pattern = new RegExp(`@binding\\((\\d+)\\)\\s+var<storage,\\s*read_write>\\s+${name}\\b`);
    const match = wgsl.match(pattern);
    if (!match) {
        throw new Error(`Missing storage binding for ${name}`);
    }
    return Number(match[1]);
}

test("emits wgsl compute entrypoints with workgroup size 1", async () => {
    const silkCode = `
    (export wgsl) add_one := (x: i32) => (
        x + 1
    );
    {}
  `;

    const wgsl = await compileToWgsl(silkCode);
    expect(wgsl).toContain("@compute");
    expect(wgsl).toContain("@workgroup_size(1)");
    expect(wgsl).toContain("fn add_one()");
}, TEST_TIMEOUT_MS);

wgslTest("executes wgsl with boxed storage bindings", async () => {
    const silkCode = `
    (export wgsl) box_a: Box(i32) := 11;
    (export wgsl) box_b: Box(i32) := 42;
    (export wgsl) sum_boxes := {} => (
        box_a + box_b
    );
    {}
  `;

    const wgsl = await compileToWgsl(silkCode);
    const boxABinding = extractBinding(wgsl, "box_a");
    const boxBBinding = extractBinding(wgsl, "box_b");
    const outBinding = extractBinding(wgsl, "silk_out_sum_boxes");

    const denoScript = `
        const wgslPath = Deno.args[0];
        const bindings = JSON.parse(Deno.args[1]);
        const wgsl = Deno.readTextFileSync(wgslPath);
        const adapter = await navigator.gpu?.requestAdapter();
        if (!adapter) {
            console.error("No GPU adapter available");
            Deno.exit(1);
        }
        const device = await adapter.requestDevice();
        const module = device.createShaderModule({ code: wgsl });
        const pipeline = device.createComputePipeline({
            layout: "auto",
            compute: { module, entryPoint: "sum_boxes" },
        });
        const bindGroupLayout = pipeline.getBindGroupLayout(0);

        const makeStorage = () => device.createBuffer({
            size: 16,
            usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_SRC | GPUBufferUsage.COPY_DST,
        });

        const boxA = makeStorage();
        const boxB = makeStorage();
        const out = makeStorage();
        device.queue.writeBuffer(boxA, 0, new Int32Array([0]));
        device.queue.writeBuffer(boxB, 0, new Int32Array([0]));
        device.queue.writeBuffer(out, 0, new Int32Array([0]));

        const bindGroup = device.createBindGroup({
            layout: bindGroupLayout,
            entries: [
                { binding: bindings.box_a, resource: { buffer: boxA } },
                { binding: bindings.box_b, resource: { buffer: boxB } },
                { binding: bindings.out, resource: { buffer: out } },
            ],
        });

        const encoder = device.createCommandEncoder();
        const pass = encoder.beginComputePass();
        pass.setPipeline(pipeline);
        pass.setBindGroup(0, bindGroup);
        pass.dispatchWorkgroups(1);
        pass.end();
        device.queue.submit([encoder.finish()]);
        await device.queue.onSubmittedWorkDone();

        async function readI32(buffer) {
            const read = device.createBuffer({
                size: 4,
                usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.MAP_READ,
            });
            const copyEncoder = device.createCommandEncoder();
            copyEncoder.copyBufferToBuffer(buffer, 0, read, 0, 4);
            device.queue.submit([copyEncoder.finish()]);
            await device.queue.onSubmittedWorkDone();
            await read.mapAsync(GPUMapMode.READ);
            const view = new DataView(read.getMappedRange());
            const value = view.getInt32(0, true);
            read.unmap();
            return value;
        }

        const [valueA, valueB, sum] = await Promise.all([
            readI32(boxA),
            readI32(boxB),
            readI32(out),
        ]);

        if (valueA !== 11) {
            console.error("box_a value mismatch:", valueA);
            throw new Error(\"box_a value mismatch: \" + valueA);
        }
        if (valueB !== 42) {
            console.error("box_b value mismatch:", valueB);
            throw new Error(\"box_b value mismatch: \" + valueB);
        }
        if (sum !== 53) {
            console.error("sum value mismatch:", sum);
            throw new Error(\"sum value mismatch: \" + sum);
        }
    `;

    const bindings = JSON.stringify({
        box_a: boxABinding,
        box_b: boxBBinding,
        out: outBinding,
    });
    const proc = Bun.spawn(["deno", "eval", denoScript, "--", TEMP_WGSL, bindings], {
        cwd: ROOT_DIR,
        stderr: "pipe",
    });
    const exitCode = await proc.exited;
    if (exitCode !== 0) {
        const stderr = await new Response(proc.stderr).text();
        console.error(stderr);
    }
    expect(exitCode).toBe(0);
}, TEST_TIMEOUT_MS);

wgslTest("runs wgsl export with input and output buffers", async () => {
    const silkCode = `
    (export wgsl) add_three := (x: i32) => (
        x + 3
    );
    {}
  `;

    const wgsl = await compileToWgsl(silkCode);
    const inBinding = extractBinding(wgsl, "silk_in_add_three");
    const outBinding = extractBinding(wgsl, "silk_out_add_three");

    const denoScript = `
        const wgslPath = Deno.args[0];
        const bindings = JSON.parse(Deno.args[1]);
        const inputValue = Number(Deno.args[2]);
        const wgsl = Deno.readTextFileSync(wgslPath);
        const adapter = await navigator.gpu?.requestAdapter();
        if (!adapter) {
            console.error("No GPU adapter available");
            Deno.exit(1);
        }
        const device = await adapter.requestDevice();
        const module = device.createShaderModule({ code: wgsl });
        const pipeline = device.createComputePipeline({
            layout: "auto",
            compute: { module, entryPoint: "add_three" },
        });
        const bindGroupLayout = pipeline.getBindGroupLayout(0);

        const input = device.createBuffer({
            size: 16,
            usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_SRC | GPUBufferUsage.COPY_DST,
        });
        const output = device.createBuffer({
            size: 16,
            usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_SRC | GPUBufferUsage.COPY_DST,
        });
        device.queue.writeBuffer(input, 0, new Int32Array([inputValue]));
        device.queue.writeBuffer(output, 0, new Int32Array([0]));

        const bindGroup = device.createBindGroup({
            layout: bindGroupLayout,
            entries: [
                { binding: bindings.input, resource: { buffer: input } },
                { binding: bindings.output, resource: { buffer: output } },
            ],
        });

        const encoder = device.createCommandEncoder();
        const pass = encoder.beginComputePass();
        pass.setPipeline(pipeline);
        pass.setBindGroup(0, bindGroup);
        pass.dispatchWorkgroups(1);
        pass.end();
        device.queue.submit([encoder.finish()]);
        await device.queue.onSubmittedWorkDone();

        const read = device.createBuffer({
            size: 4,
            usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.MAP_READ,
        });
        const copyEncoder = device.createCommandEncoder();
        copyEncoder.copyBufferToBuffer(output, 0, read, 0, 4);
        device.queue.submit([copyEncoder.finish()]);
        await device.queue.onSubmittedWorkDone();
        await read.mapAsync(GPUMapMode.READ);
        const view = new DataView(read.getMappedRange());
        const value = view.getInt32(0, true);
        read.unmap();
        if (value !== inputValue + 3) {
            throw new Error("add_three mismatch: " + value);
        }
    `;

    const bindings = JSON.stringify({
        input: inBinding,
        output: outBinding,
    });
    const proc = Bun.spawn(["deno", "eval", denoScript, "--", TEMP_WGSL, bindings, "7"], {
        cwd: ROOT_DIR,
        stderr: "pipe",
    });
    const exitCode = await proc.exited;
    if (exitCode !== 0) {
        const stderr = await new Response(proc.stderr).text();
        console.error(stderr);
    }
    expect(exitCode).toBe(0);
}, TEST_TIMEOUT_MS);

test("omits output binding for unit return exports", async () => {
    const silkCode = `
    (export wgsl) do_nothing := {} => (
        {}
    );
    {}
  `;

    const wgsl = await compileToWgsl(silkCode);
    expect(wgsl).not.toContain("silk_out_do_nothing");
}, TEST_TIMEOUT_MS);

afterAll(() => {
    try {
        unlinkSync(TEMP_SILK);
    } catch (e) { }
    try {
        unlinkSync(TEMP_WGSL);
    } catch (e) { }
});
