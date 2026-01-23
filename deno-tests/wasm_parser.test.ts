import { assertEquals } from "@std/asserts";
import { join } from "@std/path";
import { cleanup, compileToWasm, FIXTURES_DIR } from "./test_helpers.ts";

const parserSource = await Deno.readTextFile(
  join(FIXTURES_DIR, "simple_parser.silk"),
);

const NODE_SIZE = 7;
const NODE_FIELDS = {
  a: 0,
  b: 1,
  c: 2,
  kind: 3,
  span_end: 4,
  span_start: 5,
  value: 6,
} as const;

const KIND_NUMBER = 1;
const KIND_IDENTIFIER = 2;
const KIND_BINARY = 3;
const KIND_BINDING = 4;
const KIND_BLOCK = 5;
const KIND_LIST = 6;

const OP_ADD = 1;
const OP_SUB = 2;
const OP_MUL = 3;
const OP_DIV = 4;

function writeInput(memory: WebAssembly.Memory, text: string) {
  const bytes = new TextEncoder().encode(text);
  const view = new Uint8Array(memory.buffer);
  view.fill(0);
  view.set(bytes);
  view[bytes.length] = 0;
}

function readNode(view: Int32Array, idx: number) {
  const base = idx * NODE_SIZE;
  return {
    a: view[base + NODE_FIELDS.a],
    b: view[base + NODE_FIELDS.b],
    c: view[base + NODE_FIELDS.c],
    kind: view[base + NODE_FIELDS.kind],
    span_end: view[base + NODE_FIELDS.span_end],
    span_start: view[base + NODE_FIELDS.span_start],
    value: view[base + NODE_FIELDS.value],
  };
}

function readSpan(memory: WebAssembly.Memory, start: number, end: number) {
  const bytes = new Uint8Array(memory.buffer.slice(start, end));
  return new TextDecoder().decode(bytes);
}

type ParserExports = {
  parse: () => number;
  input: WebAssembly.Memory;
  nodes: WebAssembly.Memory;
  state: WebAssembly.Memory;
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

const STATE_ERROR = 1;

function readStateError(state: WebAssembly.Memory) {
  const view = new Int32Array(state.buffer);
  return view[STATE_ERROR];
}

Deno.test("wasm parser: parses binary precedence", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "1 + 2 * 3");
    const root = exports.parse();
    assertEquals(readStateError(exports.state), -1);

    const nodeView = new Int32Array(exports.nodes.buffer);
    const rootNode = readNode(nodeView, root);
    assertEquals(rootNode.kind, KIND_BLOCK);
    assertEquals(rootNode.b, 1);

    const listNode = readNode(nodeView, rootNode.a);
    assertEquals(listNode.kind, KIND_LIST);

    const exprNode = readNode(nodeView, listNode.a);
    assertEquals(exprNode.kind, KIND_BINARY);
    assertEquals(exprNode.c, OP_ADD);

    const leftNode = readNode(nodeView, exprNode.a);
    const rightNode = readNode(nodeView, exprNode.b);
    assertEquals(leftNode.kind, KIND_NUMBER);
    assertEquals(leftNode.value, 1);
    assertEquals(rightNode.kind, KIND_BINARY);
    assertEquals(rightNode.c, OP_MUL);

    const rightLeft = readNode(nodeView, rightNode.a);
    const rightRight = readNode(nodeView, rightNode.b);
    assertEquals(rightLeft.value, 2);
    assertEquals(rightRight.value, 3);
  });
});

Deno.test("wasm parser: parses bindings and identifiers", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "foo := 10 + 20; bar");
    const root = exports.parse();
    assertEquals(readStateError(exports.state), -1);

    const nodeView = new Int32Array(exports.nodes.buffer);
    const rootNode = readNode(nodeView, root);
    assertEquals(rootNode.kind, KIND_BLOCK);
    assertEquals(rootNode.b, 2);

    const firstList = readNode(nodeView, rootNode.a);
    const secondList = readNode(nodeView, firstList.b);
    assertEquals(firstList.kind, KIND_LIST);
    assertEquals(secondList.kind, KIND_LIST);

    const bindingNode = readNode(nodeView, firstList.a);
    assertEquals(bindingNode.kind, KIND_BINDING);

    const nameNode = readNode(nodeView, bindingNode.a);
    assertEquals(nameNode.kind, KIND_IDENTIFIER);
    assertEquals(readSpan(exports.input, nameNode.span_start, nameNode.span_end), "foo");

    const exprNode = readNode(nodeView, bindingNode.b);
    assertEquals(exprNode.kind, KIND_BINARY);
    assertEquals(exprNode.c, OP_ADD);

    const secondExpr = readNode(nodeView, secondList.a);
    assertEquals(secondExpr.kind, KIND_IDENTIFIER);
    assertEquals(
      readSpan(exports.input, secondExpr.span_start, secondExpr.span_end),
      "bar",
    );
  });
});

Deno.test("wasm parser: respects parentheses", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "(1 + 2) * 3");
    const root = exports.parse();
    assertEquals(readStateError(exports.state), -1);

    const nodeView = new Int32Array(exports.nodes.buffer);
    const rootNode = readNode(nodeView, root);
    const listNode = readNode(nodeView, rootNode.a);
    const exprNode = readNode(nodeView, listNode.a);
    assertEquals(exprNode.kind, KIND_BINARY);
    assertEquals(exprNode.c, OP_MUL);

    const leftNode = readNode(nodeView, exprNode.a);
    assertEquals(leftNode.kind, KIND_BINARY);
    assertEquals(leftNode.c, OP_ADD);
  });
});

Deno.test("wasm parser: parses subtraction and division", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "8 - 4 / 2");
    const root = exports.parse();
    assertEquals(readStateError(exports.state), -1);

    const nodeView = new Int32Array(exports.nodes.buffer);
    const rootNode = readNode(nodeView, root);
    const listNode = readNode(nodeView, rootNode.a);
    const exprNode = readNode(nodeView, listNode.a);
    assertEquals(exprNode.kind, KIND_BINARY);
    assertEquals(exprNode.c, OP_SUB);

    const rightNode = readNode(nodeView, exprNode.b);
    assertEquals(rightNode.kind, KIND_BINARY);
    assertEquals(rightNode.c, OP_DIV);
  });
});
