import {
    assertEquals,
    assertStringIncludes,
} from "https://deno.land/std/testing/asserts.ts";
import { cleanup, compileSilk, tempBase } from "./test_helpers.ts";

const HAS_WEBGPU = Boolean(await navigator.gpu?.requestAdapter());

async function compileToWgsl(silkCode: string, prefix: string) {
    const basePath = tempBase(prefix);
    const wgslPath = basePath + ".wgsl";
    const { code, stderr, silkPath } = await compileSilk(silkCode, wgslPath);
    if (code !== 0) {
        await cleanup([silkPath, wgslPath]);
        throw new Error(`Compilation failed:\n${stderr}`);
    }
    return { wgslPath, silkPath };
}

function extractBinding(wgsl: string, name: string) {
    const pattern = new RegExp(
        `@binding\\((\\d+)\\)\\s+var<storage,\\s*read_write>\\s+${name}\\b`,
    );
    const match = wgsl.match(pattern);
    if (!match) {
        throw new Error(`Missing storage binding for ${name}`);
    }
    return Number(match[1]);
}

Deno.test("emits wgsl compute entrypoints with workgroup size 1", async () => {
    const silkCode = `
    (export wgsl) add_one := (x: i32) => (
        x + 1
    );
    {}
  `;

    const { wgslPath, silkPath } = await compileToWgsl(silkCode, "wgsl_entrypoints");
    try {
        const wgsl = await Deno.readTextFile(wgslPath);
        assertStringIncludes(wgsl, "@compute");
        assertStringIncludes(wgsl, "@workgroup_size(1)");
        assertStringIncludes(wgsl, "fn add_one()");
    } finally {
        await cleanup([silkPath, wgslPath]);
    }
});

Deno.test({
    name: "executes wgsl with boxed storage bindings",
    ignore: !HAS_WEBGPU,
    fn: async () => {
        const silkCode = `
    (export wgsl) box_a: Box(i32) := 11;
    (export wgsl) box_b: Box(i32) := 42;
    (export wgsl) sum_boxes := {} => (
        box_a + box_b
    );
    {}
  `;

        const { wgslPath, silkPath } = await compileToWgsl(silkCode, "wgsl_boxed");
        try {
            const wgsl = await Deno.readTextFile(wgslPath);
            const boxABinding = extractBinding(wgsl, "box_a");
            const boxBBinding = extractBinding(wgsl, "box_b");
            const outBinding = extractBinding(wgsl, "silk_out_sum_boxes");

            const adapter = await navigator.gpu?.requestAdapter();
            if (!adapter) {
                throw new Error("No GPU adapter available");
            }
            const device = await adapter.requestDevice();
            const module = device.createShaderModule({ code: wgsl });
            const pipeline = device.createComputePipeline({
                layout: "auto",
                compute: { module, entryPoint: "sum_boxes" },
            });
            const bindGroupLayout = pipeline.getBindGroupLayout(0);

            const makeStorage = () =>
                device.createBuffer({
                    size: 16,
                    usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_SRC |
                        GPUBufferUsage.COPY_DST,
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
                    { binding: boxABinding, resource: { buffer: boxA } },
                    { binding: boxBBinding, resource: { buffer: boxB } },
                    { binding: outBinding, resource: { buffer: out } },
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

            async function readI32(buffer: GPUBuffer) {
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

            assertEquals(valueA, 11);
            assertEquals(valueB, 42);
            assertEquals(sum, 53);
        } finally {
            await cleanup([silkPath, wgslPath]);
        }
    },
});

Deno.test({
    name: "runs wgsl export with input and output buffers",
    ignore: !HAS_WEBGPU,
    fn: async () => {
        const silkCode = `
    (export wgsl) add_three := (x: i32) => (
        x + 3
    );
    {}
  `;

        const { wgslPath, silkPath } = await compileToWgsl(silkCode, "wgsl_io");
        try {
            const wgsl = await Deno.readTextFile(wgslPath);
            const inBinding = extractBinding(wgsl, "silk_in_add_three");
            const outBinding = extractBinding(wgsl, "silk_out_add_three");

            const adapter = await navigator.gpu?.requestAdapter();
            if (!adapter) {
                throw new Error("No GPU adapter available");
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
                usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_SRC |
                    GPUBufferUsage.COPY_DST,
            });
            const output = device.createBuffer({
                size: 16,
                usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_SRC |
                    GPUBufferUsage.COPY_DST,
            });
            device.queue.writeBuffer(input, 0, new Int32Array([7]));
            device.queue.writeBuffer(output, 0, new Int32Array([0]));

            const bindGroup = device.createBindGroup({
                layout: bindGroupLayout,
                entries: [
                    { binding: inBinding, resource: { buffer: input } },
                    { binding: outBinding, resource: { buffer: output } },
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
            assertEquals(value, 10);
        } finally {
            await cleanup([silkPath, wgslPath]);
        }
    },
});

Deno.test("omits output binding for unit return exports", async () => {
    const silkCode = `
    (export wgsl) do_nothing := {} => (
        {}
    );
    {}
  `;

    const { wgslPath, silkPath } = await compileToWgsl(silkCode, "wgsl_unit");
    try {
        const wgsl = await Deno.readTextFile(wgslPath);
        if (wgsl.includes("silk_out_do_nothing")) {
            throw new Error("Expected no output binding for unit return");
        }
    } finally {
        await cleanup([silkPath, wgslPath]);
    }
});
