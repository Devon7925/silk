import { assert, assertEquals } from "@std/asserts";
import { join } from "@std/path";
import {
  cleanup,
  compileToWasmWithExtraFiles,
  ROOT_DIR,
} from "./test_helpers.ts";

const parserSource = await Deno.readTextFile(
  join(ROOT_DIR, "silk_src/parser.silk"),
);
const interpreterSource = await Deno.readTextFile(
  join(ROOT_DIR, "silk_src/interpreter.silk"),
);
const typesSource = await Deno.readTextFile(
  join(ROOT_DIR, "silk_src/types.silk"),
);

const VALUE_NUMBER = 0;
const VALUE_BOOLEAN = 1;
const VALUE_CHAR = 2;
const VALUE_STRING = 3;
const VALUE_UNIT = 4;
const TARGET_MASK_JS = 1;
const TARGET_MASK_WASM = 2;

type ParserExports = {
  parse: () => number;
  input: WebAssembly.Memory;
  nodes: WebAssembly.Memory;
  list_nodes: WebAssembly.Memory;
  state: WebAssembly.Memory;
  get_state_error: () => number;
};

type InterpreterExports = {
  interpret: (root: number) => number;
  input: WebAssembly.Memory;
  nodes: WebAssembly.Memory;
  list_nodes: WebAssembly.Memory;
  state: WebAssembly.Memory;
  values: WebAssembly.Memory;
  value_state: WebAssembly.Memory;
  get_interp_error: () => number;
  get_value_tag: (idx: number) => number;
  get_value_number: (idx: number) => number;
  get_value_boolean: (idx: number) => number;
  get_value_char: (idx: number) => number;
  get_value_string_start: (idx: number) => number;
  get_value_string_length: (idx: number) => number;
  get_binding_count: () => number;
  get_binding_name_start: (idx: number) => number;
  get_binding_name_length: (idx: number) => number;
  get_binding_target_mask: (idx: number) => number;
  get_binding_export_mask: (idx: number) => number;
  get_binding_wrap_mask: (idx: number) => number;
};

const { parserExports, interpreterExports } = await (async () => {
  const [{ wasmPath: parserPath, silkPath: parserSilk, extraPaths: parserExtra },
    { wasmPath: interpPath, silkPath: interpSilk, extraPaths: interpExtra }] =
    await Promise.all([
      compileToWasmWithExtraFiles(parserSource, "wasm_parser", {
        "types.silk": typesSource,
      }),
      compileToWasmWithExtraFiles(interpreterSource, "wasm_interpreter", {
        "types.silk": typesSource,
      }),
    ]);
  const [parserBytes, interpreterBytes] = await Promise.all([
    Deno.readFile(parserPath),
    Deno.readFile(interpPath),
  ]);
  const [parserInstance, interpreterInstance] = await Promise.all([
    WebAssembly.instantiate(parserBytes),
    WebAssembly.instantiate(interpreterBytes),
  ]);
  addEventListener("unload", () => {
    void cleanup([
      parserSilk,
      parserPath,
      ...parserExtra,
      interpSilk,
      interpPath,
      ...interpExtra,
    ]);
  });
  return {
    parserExports: parserInstance.instance.exports as ParserExports,
    interpreterExports: interpreterInstance.instance.exports as InterpreterExports,
  };
})();

function writeInput(memory: WebAssembly.Memory, text: string) {
  const bytes = new TextEncoder().encode(text);
  const view = new Uint8Array(memory.buffer);
  view.fill(0);
  view.set(bytes);
  view[bytes.length] = 0;
}

function copyMemory(src: WebAssembly.Memory, dst: WebAssembly.Memory) {
  const srcView = new Uint8Array(src.buffer);
  const dstView = new Uint8Array(dst.buffer);
  assert(
    dstView.byteLength >= srcView.byteLength,
    "destination memory is smaller than source",
  );
  dstView.fill(0);
  dstView.set(srcView);
}

function readSpan(memory: WebAssembly.Memory, start: number, length: number) {
  const bytes = new Uint8Array(memory.buffer.slice(start, start + length));
  return new TextDecoder().decode(bytes);
}

function parseAndInterpret(source: string) {
  writeInput(parserExports.input, source);
  const root = parserExports.parse();
  assertEquals(parserExports.get_state_error(), -1);
  copyMemory(parserExports.input, interpreterExports.input);
  copyMemory(parserExports.nodes, interpreterExports.nodes);
  copyMemory(parserExports.list_nodes, interpreterExports.list_nodes);
  copyMemory(parserExports.state, interpreterExports.state);
  const resultIdx = interpreterExports.interpret(root);
  assertEquals(interpreterExports.get_interp_error(), -1);
  return resultIdx;
}

function parseAndExpectError(source: string) {
  writeInput(parserExports.input, source);
  const root = parserExports.parse();
  assertEquals(parserExports.get_state_error(), -1);
  copyMemory(parserExports.input, interpreterExports.input);
  copyMemory(parserExports.nodes, interpreterExports.nodes);
  copyMemory(parserExports.list_nodes, interpreterExports.list_nodes);
  copyMemory(parserExports.state, interpreterExports.state);
  interpreterExports.interpret(root);
  const errorPos = interpreterExports.get_interp_error();
  assert(errorPos !== -1);
  return errorPos;
}

function findBindingIndex(name: string) {
  const count = interpreterExports.get_binding_count();
  for (let i = 0; i < count; i++) {
    const start = interpreterExports.get_binding_name_start(i);
    const length = interpreterExports.get_binding_name_length(i);
    if (start < 0 || length <= 0) {
      continue;
    }
    if (readSpan(interpreterExports.input, start, length) === name) {
      return i;
    }
  }
  return -1;
}

Deno.test("wasm interpreter: evaluates arithmetic", () => {
  const resultIdx = parseAndInterpret("1 + 2 * 3");
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 7);
});

Deno.test("wasm interpreter: evaluates blocks sequentially", () => {
  const resultIdx = parseAndInterpret("1; 2; 3");
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 3);
});

Deno.test("wasm interpreter: evaluates if expressions", () => {
  const resultIdx = parseAndInterpret("if 1 == 1 then (2) else (3)");
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 2);
});

Deno.test("wasm interpreter: returns string literals", () => {
  const resultIdx = parseAndInterpret("\"hi\"");
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_STRING);
  const start = interpreterExports.get_value_string_start(resultIdx);
  const length = interpreterExports.get_value_string_length(resultIdx);
  assertEquals(readSpan(interpreterExports.input, start, length), "hi");
});

Deno.test("wasm interpreter: returns unit for empty struct", () => {
  const resultIdx = parseAndInterpret("{}");
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_UNIT);
});

Deno.test("wasm interpreter: resolves bindings and identifiers", () => {
  const resultIdx = parseAndInterpret("x := 5; x + 2");
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 7);
});

Deno.test("wasm interpreter: supports mutable assignment", () => {
  const resultIdx = parseAndInterpret("mut x := 1; x = x + 2; x");
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 3);
});

Deno.test("wasm interpreter: supports nested struct mutation", () => {
  const resultIdx = parseAndInterpret(
    "mut foo := { first = { 5, 3 }, second = 4 }; foo.first.0 = foo.first.0 + foo.second; foo.first.0",
  );
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 9);
});

Deno.test("wasm interpreter: evaluates while loops", () => {
  const resultIdx = parseAndInterpret(
    "mut x := 0; mut acc := 0; while x < 4 do (acc = acc + x; x = x + 1;); acc",
  );
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 6);
});

Deno.test("wasm interpreter: evaluates function calls", () => {
  const resultIdx = parseAndInterpret(
    "add_one := (x: i32) => (x + 1); add_one 4",
  );
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 5);
});

Deno.test("wasm interpreter: accesses struct properties", () => {
  const resultIdx = parseAndInterpret("point := { x = 5, y = 10 }; point.x");
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 5);
});

Deno.test("wasm interpreter: calls functions stored on structs", () => {
  const resultIdx = parseAndInterpret(
    "container := { inc = (value: i32) => (value + 1) }; container.inc(41)",
  );
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 42);
});

Deno.test("wasm interpreter: evaluates array repeats", () => {
  const resultIdx = parseAndInterpret("arr := {5; 3}; arr.1");
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 5);
});

Deno.test("wasm interpreter: matches enum patterns in while", () => {
  const resultIdx = parseAndInterpret(`
    Option := (T: type) => (enum { Some = T, None = {} });
    mut opt := Option(i32)::Some(7);
    mut result := 0;
    while Option(i32)::Some(value) := opt do (
      opt = Option(i32)::None;
      result = value;
    );
    result
  `);
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 7);
});

Deno.test("wasm interpreter: evaluates match expressions via match builtin", () => {
  const resultIdx = parseAndInterpret(`
    Option := (T: type) => (enum { Some = T, None = {} });
    choose := (option: Option(i32)) => (
      option |> match {
        Option(i32)::Some(value) => value,
        else => 0
      }
    );
    choose(Option(i32)::Some(5))
  `);
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 5);
});

Deno.test("wasm interpreter: destructures struct patterns", () => {
  const resultIdx = parseAndInterpret(
    "mut { first = a, second = b } := { first = 3, second = 4 }; a + b",
  );
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 7);
});

Deno.test("wasm interpreter: destructures positional struct patterns", () => {
  const resultIdx = parseAndInterpret("{ a, b } := { 3, 4 }; a + b");
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 7);
});

Deno.test("wasm interpreter: matches enum variants by name", () => {
  const resultIdx = parseAndInterpret(`
    Either := (T: type) => (enum { Left = T, Right = T });
    value := Either(i32)::Right(7);
    value |> match {
      Either(i32)::Left(x) => x,
      else => 0
    }
  `);
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 0);
});

Deno.test("wasm interpreter: supports pattern parameters", () => {
  const resultIdx = parseAndInterpret(
    "sum_pair := { first = a, second = b } => (a + b); sum_pair { first = 2, second = 5 }",
  );
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 7);
});

Deno.test("wasm interpreter: supports implementations on intrinsic types", () => {
  const resultIdx = parseAndInterpret(`
    Meters := i32 @ { square = (self: i32) => self * self };
    value: Meters := 5;
    value.square
  `);
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 25);
});

Deno.test("wasm interpreter: supports implementations on struct types", () => {
  const resultIdx = parseAndInterpret(`
    Pair := { first = i32, second = i32 } @ {
      sum = (self: { first = i32, second = i32 }) => self.first + self.second,
    };
    pair: Pair := { first = 3, second = 4 };
    pair.sum
  `);
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 7);
});

Deno.test("wasm interpreter: evaluates for loops via iterator impls", () => {
  const resultIdx = parseAndInterpret(`
    RangeIter := { value = i32, limit = i32 } @ {
      iter_ty = i32,
      next = (mut self: { value = i32, limit = i32 }) => (
        if self.value < self.limit then (
          current := self.value;
          self.value = self.value + 1;
          Option(i32)::Some(current)
        ) else Option(i32)::None
      ),
    };
    make_range := (limit: i32) => (
      range: RangeIter := { value = 0, limit = limit };
      range
    );
    mut acc := 0;
    for value in make_range(4) do (
      acc = acc + value;
    );
    acc
  `);
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 6);
});

Deno.test("wasm interpreter: iterates range operators", () => {
  const resultIdx = parseAndInterpret(`
    mut acc := 0;
    for value in 0..4 do (
      acc = acc + value;
    );
    acc
  `);
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 6);
});

Deno.test("wasm interpreter: resolves Box type annotations", () => {
  const resultIdx = parseAndInterpret(`
    Boxed := Box(i32) @ { get = (self: Box(i32)) => self };
    value: Box(i32) := 5;
    value.get
  `);
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 5);
});

Deno.test("wasm interpreter: rejects Box of non-type expressions", () => {
  parseAndExpectError("Box(1 + 2)");
});

Deno.test("wasm interpreter: rejects nested Box types", () => {
  parseAndExpectError("Box(Box(i32))");
});

Deno.test("wasm interpreter: rejects asm without target context", () => {
  parseAndExpectError(`foo := asm(wasm)("i32.const 1"); 0`);
});

Deno.test("wasm interpreter: rejects asm with mismatched target context", () => {
  parseAndExpectError(`(target js) foo := asm(wasm)("i32.const 1"); 0`);
});

Deno.test("wasm interpreter: allows asm with matching target context", () => {
  const resultIdx = parseAndInterpret(`
    (target wasm) foo := asm(wasm)("i32.const 1");
    0
  `);
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 0);
});

Deno.test("wasm interpreter: rejects target bindings outside target context", () => {
  parseAndExpectError(`(target wasm) foo := 1; foo`);
});

Deno.test("wasm interpreter: allows target bindings inside matching context", () => {
  const resultIdx = parseAndInterpret(`
    (target wasm) foo := 1;
    (target wasm) bar := foo;
    0
  `);
  assertEquals(interpreterExports.get_value_tag(resultIdx), VALUE_NUMBER);
  assertEquals(interpreterExports.get_value_number(resultIdx), 0);
});

Deno.test("wasm interpreter: exposes binding annotations", () => {
  parseAndInterpret(`
    (export wasm) (wrap js) add_one := (x: i32) => (x + 1);
    (target wasm) only_wasm := 5;
    (export js) (wrap wasm) bridge := (x: i32) => x;
    0
  `);

  const addIdx = findBindingIndex("add_one");
  assert(addIdx !== -1);
  assertEquals(interpreterExports.get_binding_export_mask(addIdx), TARGET_MASK_WASM);
  assertEquals(interpreterExports.get_binding_wrap_mask(addIdx), TARGET_MASK_JS);
  assertEquals(interpreterExports.get_binding_target_mask(addIdx), 0);

  const onlyIdx = findBindingIndex("only_wasm");
  assert(onlyIdx !== -1);
  assertEquals(interpreterExports.get_binding_target_mask(onlyIdx), TARGET_MASK_WASM);
  assertEquals(interpreterExports.get_binding_export_mask(onlyIdx), 0);
  assertEquals(interpreterExports.get_binding_wrap_mask(onlyIdx), 0);

  const bridgeIdx = findBindingIndex("bridge");
  assert(bridgeIdx !== -1);
  assertEquals(interpreterExports.get_binding_export_mask(bridgeIdx), TARGET_MASK_JS);
  assertEquals(interpreterExports.get_binding_wrap_mask(bridgeIdx), TARGET_MASK_WASM);
  assertEquals(interpreterExports.get_binding_target_mask(bridgeIdx), 0);
});
