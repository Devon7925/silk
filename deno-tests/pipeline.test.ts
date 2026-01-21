import { assertEquals } from "https://deno.land/std/testing/asserts.ts";
import { compileToInstance } from "./test_helpers.ts";

Deno.test("pipeline forwards values into functions", async () => {
  const silkCode = `
    (export wasm) pipe_simple := (value: i32) => (
        increment := (x: i32) => (
            x + 1
        );

        value |> increment
    );
    `;

  const exports = (await compileToInstance(silkCode, "pipeline_simple"))
    .exports as any;
  assertEquals(exports.pipe_simple(41), 42);
});

Deno.test("pipeline composes multiple functions", async () => {
  const silkCode = `
    (export wasm) pipe_chain := (value: i32) => (
        increment := (x: i32) => (
            x + 1
        );
        double := (x: i32) => (
            x * 2
        );

        value |> increment |> double
    );
    `;

  const exports = (await compileToInstance(silkCode, "pipeline_chain"))
    .exports as any;
  assertEquals(exports.pipe_chain(5), 12);
});

Deno.test("pipeline accepts inline function literals", async () => {
  const silkCode = `
    (export wasm) pipe_literal := (value: i32) => (
        value |> ((y: i32) => (
            y * y
        ))
    );
    `;

  const exports = (await compileToInstance(silkCode, "pipeline_literal"))
    .exports as any;
  assertEquals(exports.pipe_literal(6), 36);
});
