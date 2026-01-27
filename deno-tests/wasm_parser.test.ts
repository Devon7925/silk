import { assertEquals } from "@std/asserts";
import { join } from "@std/path";
import { cleanup, compileToWasm, FIXTURES_DIR } from "./test_helpers.ts";

const parserSource = await Deno.readTextFile(
  join(FIXTURES_DIR, "simple_parser.silk"),
);

const KIND_LITERAL = 13;
const KIND_IDENTIFIER = 14;
const KIND_OPERATION = 15;
const KIND_ASSIGNMENT = 16;
const KIND_FUNCTION_CALL = 17;
const KIND_TYPE_PROPERTY = 19;
const KIND_BINDING = 20;
const KIND_BLOCK = 21;
const KIND_IF = 7;
const KIND_FUNCTION = 9;
const KIND_FUNCTION_TYPE = 10;
const KIND_STRUCT = 11;
const KIND_ARRAY_REPEAT = 12;
const KIND_DIVERGE = 22;
const KIND_LOOP = 23;

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
  get_literal_boolean: (idx: number) => number;
  get_literal_char: (idx: number) => number;
  get_literal_string_start: (idx: number) => number;
  get_literal_string_length: (idx: number) => number;
  get_identifier_start: (idx: number) => number;
  get_identifier_length: (idx: number) => number;
  get_operation_left: (idx: number) => number;
  get_operation_right: (idx: number) => number;
  get_operation_operator_start: (idx: number) => number;
  get_operation_operator_length: (idx: number) => number;
  get_binding_expr: (idx: number) => number;
  get_binding_name_start: (idx: number) => number;
  get_binding_name_length: (idx: number) => number;
  get_binding_pattern_tag: (idx: number) => number;
  get_binding_pattern_struct_fields: (idx: number) => number;
  get_binding_pattern_typehint_pattern: (idx: number) => number;
  get_binding_pattern_typehint_type: (idx: number) => number;
  get_binding_pattern_annotations: (idx: number) => number;
  get_binding_pattern_annotated_pattern: (idx: number) => number;
  get_binding_pattern_enum_type: (idx: number) => number;
  get_binding_pattern_enum_variant_start: (idx: number) => number;
  get_binding_pattern_enum_variant_length: (idx: number) => number;
  get_binding_pattern_enum_payload: (idx: number) => number;
  get_block_count: (idx: number) => number;
  get_block_item: (idx: number, pos: number) => number;
  get_list_next: (idx: number) => number;
  get_list_value: (idx: number) => number;
  get_struct_fields: (idx: number) => number;
  get_array_repeat_value: (idx: number) => number;
  get_array_repeat_count: (idx: number) => number;
  get_assignment_target_kind: (idx: number) => number;
  get_assignment_target_identifier_start: (idx: number) => number;
  get_assignment_target_identifier_length: (idx: number) => number;
  get_assignment_target_property_start: (idx: number) => number;
  get_assignment_target_property_length: (idx: number) => number;
  get_assignment_target_index_expr: (idx: number) => number;
  get_function_param_pattern_tag: (idx: number) => number;
  get_function_body: (idx: number) => number;
  get_function_return_type: (idx: number) => number;
  get_function_type_param: (idx: number) => number;
  get_function_type_return: (idx: number) => number;
  get_if_condition: (idx: number) => number;
  get_if_then: (idx: number) => number;
  get_if_else: (idx: number) => number;
  get_loop_body: (idx: number) => number;
  get_diverge_type: (idx: number) => number;
  get_diverge_value: (idx: number) => number;
  get_function_call_function: (idx: number) => number;
  get_function_call_argument: (idx: number) => number;
  get_type_property_access_object: (idx: number) => number;
  get_type_property_access_property_start: (idx: number) => number;
  get_type_property_access_property_length: (idx: number) => number;
  get_attach_type_expr: (idx: number) => number;
  get_attach_implementation: (idx: number) => number;
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

Deno.test("wasm parser: skips whitespace and comments", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, " 1 + 2 // comment\n +\t3");
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);
    assertEquals(exports.get_kind_tag(root), KIND_BLOCK);
    assertEquals(exports.get_block_count(root), 1);
  });
});

Deno.test("wasm parser: parses negative numbers", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "-5 + 2");
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);
    const exprNode = exports.get_block_item(root, 0);
    const leftNode = exports.get_operation_left(exprNode);
    assertEquals(exports.get_literal_number(leftNode), -5);
  });
});

Deno.test("wasm parser: parses char and string literals", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "'a'; \"hi\"");
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);
    const charNode = exports.get_block_item(root, 0);
    const stringNode = exports.get_block_item(root, 1);
    assertEquals(exports.get_literal_char(charNode), "a".charCodeAt(0));
    assertEquals(
      readSpan(
        exports.input,
        exports.get_literal_string_start(stringNode),
        exports.get_literal_string_length(stringNode),
      ),
      "hi",
    );
  });
});

Deno.test("wasm parser: supports multi-char operators", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "1 <= 2 && 3 != 4");
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
      "&&",
    );
  });
});

Deno.test("wasm parser: parses assignments and lvalues", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "foo = 1; foo::bar = 2; foo(3) = 4");
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);
    const first = exports.get_block_item(root, 0);
    const second = exports.get_block_item(root, 1);
    const third = exports.get_block_item(root, 2);
    assertEquals(exports.get_kind_tag(first), KIND_ASSIGNMENT);
    assertEquals(exports.get_assignment_target_kind(first), 0);
    assertEquals(
      readSpan(
        exports.input,
        exports.get_assignment_target_identifier_start(first),
        exports.get_assignment_target_identifier_length(first),
      ),
      "foo",
    );
    assertEquals(exports.get_assignment_target_kind(second), 1);
    assertEquals(
      readSpan(
        exports.input,
        exports.get_assignment_target_property_start(second),
        exports.get_assignment_target_property_length(second),
      ),
      "bar",
    );
    assertEquals(exports.get_assignment_target_kind(third), 2);
    const indexExpr = exports.get_assignment_target_index_expr(third);
    assertEquals(exports.get_literal_number(indexExpr), 3);
  });
});

Deno.test("wasm parser: parses functions and calls", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "inc := (x: i32) => x + 1; inc 2");
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);
    const binding = exports.get_block_item(root, 0);
    const call = exports.get_block_item(root, 1);
    const funcNode = exports.get_binding_expr(binding);
    assertEquals(exports.get_kind_tag(funcNode), KIND_FUNCTION);
    assertEquals(exports.get_function_param_pattern_tag(funcNode), 4);
    assertEquals(exports.get_kind_tag(call), KIND_FUNCTION_CALL);
  });
});

Deno.test("wasm parser: parses function type expressions", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "a -> b");
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);
    const exprNode = exports.get_block_item(root, 0);
    assertEquals(exports.get_kind_tag(exprNode), KIND_FUNCTION_TYPE);
  });
});

Deno.test("wasm parser: desugars dot calls", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "foo.bar baz");
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);
    const exprNode = exports.get_block_item(root, 0);
    assertEquals(exports.get_kind_tag(exprNode), KIND_FUNCTION_CALL);
    const inner = exports.get_function_call_function(exprNode);
    assertEquals(exports.get_kind_tag(inner), KIND_FUNCTION_CALL);
    const access = exports.get_function_call_function(inner);
    assertEquals(exports.get_kind_tag(access), KIND_TYPE_PROPERTY);
    assertEquals(
      readSpan(
        exports.input,
        exports.get_type_property_access_property_start(access),
        exports.get_type_property_access_property_length(access),
      ),
      "bar",
    );
  });
});

Deno.test("wasm parser: parses type property access", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "foo::bar");
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);
    const exprNode = exports.get_block_item(root, 0);
    assertEquals(exports.get_kind_tag(exprNode), KIND_TYPE_PROPERTY);
    assertEquals(
      readSpan(
        exports.input,
        exports.get_type_property_access_property_start(exprNode),
        exports.get_type_property_access_property_length(exprNode),
      ),
      "bar",
    );
  });
});

Deno.test("wasm parser: parses attach implementation", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "Type @ Impl");
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);
    const exprNode = exports.get_block_item(root, 0);
    assertEquals(exports.get_kind_tag(exprNode), 8);
    assertEquals(exports.get_attach_type_expr(exprNode) !== -1, true);
    assertEquals(exports.get_attach_implementation(exprNode) !== -1, true);
  });
});

Deno.test("wasm parser: parses if expressions", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "if true then 1 else 2");
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);
    const exprNode = exports.get_block_item(root, 0);
    assertEquals(exports.get_kind_tag(exprNode), KIND_IF);
    const condition = exports.get_if_condition(exprNode);
    assertEquals(exports.get_literal_boolean(condition), 1);
  });
});

Deno.test("wasm parser: parses loop and while expressions", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "loop (1); while true do (1)");
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);
    const loopNode = exports.get_block_item(root, 0);
    const whileNode = exports.get_block_item(root, 1);
    assertEquals(exports.get_kind_tag(loopNode), KIND_LOOP);
    assertEquals(exports.get_kind_tag(whileNode), KIND_LOOP);
    const whileBody = exports.get_loop_body(whileNode);
    assertEquals(exports.get_kind_tag(whileBody), KIND_IF);
  });
});

Deno.test("wasm parser: parses for expressions into blocks", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "for x in y do (x)");
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);
    const exprNode = exports.get_block_item(root, 0);
    assertEquals(exports.get_kind_tag(exprNode), KIND_BLOCK);
    assertEquals(exports.get_block_count(exprNode), 2);
  });
});

Deno.test("wasm parser: parses return and break expressions", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "return 1; break");
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);
    const retNode = exports.get_block_item(root, 0);
    const breakNode = exports.get_block_item(root, 1);
    assertEquals(exports.get_kind_tag(retNode), KIND_DIVERGE);
    assertEquals(exports.get_diverge_type(retNode), 0);
    assertEquals(exports.get_kind_tag(breakNode), KIND_DIVERGE);
    assertEquals(exports.get_diverge_type(breakNode), 1);
  });
});

Deno.test("wasm parser: parses struct literals and array repeat", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, "{foo = 1, 2}; {3; 4}");
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);
    const structNode = exports.get_block_item(root, 0);
    const repeatNode = exports.get_block_item(root, 1);
    assertEquals(exports.get_kind_tag(structNode), KIND_STRUCT);
    assertEquals(exports.get_struct_fields(structNode) !== -1, true);
    assertEquals(exports.get_kind_tag(repeatNode), KIND_ARRAY_REPEAT);
    const repeatValue = exports.get_array_repeat_value(repeatNode);
    const repeatCount = exports.get_array_repeat_count(repeatNode);
    assertEquals(exports.get_literal_number(repeatValue), 3);
    assertEquals(exports.get_literal_number(repeatCount), 4);
  });
});

Deno.test("wasm parser: parses rich binding patterns", async () => {
  await withParserInstance((exports) => {
    writeInput(
      exports.input,
      "1 := 2; {foo = bar} := baz; Option::Some value := qux; foo: i32 := 1; mut foo := 2",
    );
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);
    const litPat = exports.get_block_item(root, 0);
    const structPat = exports.get_block_item(root, 1);
    const enumPat = exports.get_block_item(root, 2);
    const typePat = exports.get_block_item(root, 3);
    const annPat = exports.get_block_item(root, 4);
    assertEquals(exports.get_binding_pattern_tag(litPat), 1);
    assertEquals(exports.get_binding_pattern_tag(structPat), 2);
    assertEquals(exports.get_binding_pattern_tag(enumPat), 3);
    assertEquals(exports.get_binding_pattern_tag(typePat), 4);
    assertEquals(exports.get_binding_pattern_tag(annPat), 5);
    const annList = exports.get_binding_pattern_annotations(annPat);
    const annValue = exports.get_list_value(annList);
    assertEquals(
      readSpan(
        exports.input,
        exports.get_identifier_start(annValue),
        exports.get_identifier_length(annValue),
      ),
      "mut",
    );
  });
});

Deno.test("wasm parser: reports diagnostics on invalid input", async () => {
  await withParserInstance((exports) => {
    writeInput(exports.input, ":= 1");
    exports.parse();
    assertEquals(exports.get_state_error() !== -1, true);
  });
});

Deno.test("wasm parser: handles larger node counts", async () => {
  await withParserInstance((exports) => {
    const items = Array.from({ length: 40 }, (_, i) => `${i}`).join(";");
    writeInput(exports.input, items);
    const root = exports.parse();
    assertEquals(exports.get_state_error(), -1);
    assertEquals(exports.get_block_count(root), 40);
  });
});
