import { assertEquals } from "@std/asserts";
import { join } from "@std/path";
import { cleanup, compileToWasm, FIXTURES_DIR } from "./test_helpers.ts";

const parserSource = await Deno.readTextFile(
  join(FIXTURES_DIR, "simple_parser.silk"),
);

const KIND_LITERAL = 13;
const KIND_IDENTIFIER = 14;
const KIND_OPERATION = 15;
const KIND_BINDING = 20;
const KIND_BLOCK = 21;

function writeInput(memory: WebAssembly.Memory, text: string) {
  const bytes = new TextEncoder().encode(text);
  const view = new Uint8Array(memory.buffer);
  view.fill(0);
  view.set(bytes);
  view[bytes.length] = 0;
}

function readSpan(memory: WebAssembly.Memory, start: number, length: number) {
  const bytes = new Uint8Array(memory.buffer.slice(start, start + length));
  return new TextDecoder().decode(bytes);
}

type ParserExports = {
  parse: () => number;
  input: WebAssembly.Memory;
  nodes: WebAssembly.Memory;
  state: WebAssembly.Memory;
  get_state_error: () => number;
  get_kind_tag: (idx: number) => number;
  get_span_start: (idx: number) => number;
  get_span_length: (idx: number) => number;
  get_literal_number: (idx: number) => number;
  get_identifier_start: (idx: number) => number;
  get_identifier_length: (idx: number) => number;
  get_operation_left: (idx: number) => number;
  get_operation_right: (idx: number) => number;
  get_operation_operator_start: (idx: number) => number;
  get_operation_operator_length: (idx: number) => number;
  get_binding_expr: (idx: number) => number;
  get_binding_name_start: (idx: number) => number;
  get_binding_name_length: (idx: number) => number;
  get_block_count: (idx: number) => number;
  get_block_item: (idx: number, pos: number) => number;
};

const parserExports = await (async () => {
  const { wasmPath, silkPath } = await compileToWasm(
    parserSource,
    "wasm_parser",
  );
  const bytes = await Deno.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(bytes);
  addEventListener("unload", () => {
    void cleanup([silkPath, wasmPath]);
  });
  return instance.exports as ParserExports;
})();

async function withParserInstance(
  fn: (exports: ParserExports) => void | Promise<void>,
) {
  await fn(parserExports);
}

Deno.test("wasm parser: parses binary precedence", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "1 + 2 * 3");
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);

    assertEquals(exports.get_kind_tag(root), KIND_BLOCK);
    assertEquals(exports.get_block_count(root), 1);

    const exprNode = exports.get_block_item(root, 0);
    assertEquals(exports.get_kind_tag(exprNode), KIND_OPERATION);
    assertEquals(
      readSpan(
        exports.input,
        exports.get_operation_operator_start(exprNode),
        exports.get_operation_operator_length(exprNode),
      ),
      "+",
    );

    const leftNode = exports.get_operation_left(exprNode);
    const rightNode = exports.get_operation_right(exprNode);
    assertEquals(exports.get_kind_tag(leftNode), KIND_LITERAL);
    assertEquals(exports.get_literal_number(leftNode), 1);
    assertEquals(exports.get_kind_tag(rightNode), KIND_OPERATION);
    assertEquals(
      readSpan(
        exports.input,
        exports.get_operation_operator_start(rightNode),
        exports.get_operation_operator_length(rightNode),
      ),
      "*",
    );

    const rightLeft = exports.get_operation_left(rightNode);
    const rightRight = exports.get_operation_right(rightNode);
    assertEquals(exports.get_literal_number(rightLeft), 2);
    assertEquals(exports.get_literal_number(rightRight), 3);
  });
});

Deno.test("wasm parser: parses bindings and identifiers", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "foo := 10 + 20; bar");
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);

    assertEquals(exports.get_kind_tag(root), KIND_BLOCK);
    assertEquals(exports.get_block_count(root), 2);

    const bindingNode = exports.get_block_item(root, 0);
    const secondExpr = exports.get_block_item(root, 1);
    assertEquals(exports.get_kind_tag(bindingNode), KIND_BINDING);

    assertEquals(
      readSpan(
        exports.input,
        exports.get_binding_name_start(bindingNode),
        exports.get_binding_name_length(bindingNode),
      ),
      "foo",
    );

    const exprNode = exports.get_binding_expr(bindingNode);
    assertEquals(exports.get_kind_tag(exprNode), KIND_OPERATION);
    assertEquals(
      readSpan(
        exports.input,
        exports.get_operation_operator_start(exprNode),
        exports.get_operation_operator_length(exprNode),
      ),
      "+",
    );

    assertEquals(exports.get_kind_tag(secondExpr), KIND_IDENTIFIER);
    assertEquals(
      readSpan(
        exports.input,
        exports.get_identifier_start(secondExpr),
        exports.get_identifier_length(secondExpr),
      ),
      "bar",
    );
  });
});

Deno.test("wasm parser: respects parentheses", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "(1 + 2) * 3");
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);

    const exprNode = exports.get_block_item(root, 0);
    assertEquals(exports.get_kind_tag(exprNode), KIND_OPERATION);
    assertEquals(
      readSpan(
        exports.input,
        exports.get_operation_operator_start(exprNode),
        exports.get_operation_operator_length(exprNode),
      ),
      "*",
    );

    const leftNode = exports.get_operation_left(exprNode);
    assertEquals(exports.get_kind_tag(leftNode), KIND_OPERATION);
    assertEquals(
      readSpan(
        exports.input,
        exports.get_operation_operator_start(leftNode),
        exports.get_operation_operator_length(leftNode),
      ),
      "+",
    );
  });
});

Deno.test("wasm parser: parses subtraction and division", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "8 - 4 / 2");
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);

    const exprNode = exports.get_block_item(root, 0);
    assertEquals(exports.get_kind_tag(exprNode), KIND_OPERATION);
    assertEquals(
      readSpan(
        exports.input,
        exports.get_operation_operator_start(exprNode),
        exports.get_operation_operator_length(exprNode),
      ),
      "-",
    );

    const rightNode = exports.get_operation_right(exprNode);
    assertEquals(exports.get_kind_tag(rightNode), KIND_OPERATION);
    assertEquals(
      readSpan(
        exports.input,
        exports.get_operation_operator_start(rightNode),
        exports.get_operation_operator_length(rightNode),
      ),
      "/",
    );
  });
});
