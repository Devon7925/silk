use std::fs;
use std::rc::Rc;
use std::sync::OnceLock;

use crate::parsing::{
    Binding, BindingPattern, DivergeExpressionType, Expression, ExpressionKind, Identifier, LValue,
};
use crate::{ArtifactKind, CompilationArtifact, SourceSpan};
use wasmtime::{Config, Engine, Instance, Memory, Module, Store};

const KIND_INTRINSIC_TYPE: i32 = 0;
const KIND_BOX_TYPE: i32 = 1;
const KIND_INTRINSIC_OPERATION: i32 = 2;
const KIND_ENUM_TYPE: i32 = 3;
const KIND_MATCH: i32 = 4;
const KIND_ENUM_VALUE: i32 = 5;
const KIND_ENUM_CONSTRUCTOR: i32 = 6;
const KIND_IF: i32 = 7;
const KIND_ATTACH_IMPLEMENTATION: i32 = 8;
const KIND_FUNCTION: i32 = 9;
const KIND_FUNCTION_TYPE: i32 = 10;
const KIND_STRUCT: i32 = 11;
const KIND_ARRAY_REPEAT: i32 = 12;
const KIND_LITERAL: i32 = 13;
const KIND_IDENTIFIER: i32 = 14;
const KIND_OPERATION: i32 = 15;
const KIND_ASSIGNMENT: i32 = 16;
const KIND_FUNCTION_CALL: i32 = 17;
const KIND_ARRAY_INDEX: i32 = 18;
const KIND_TYPE_PROPERTY: i32 = 19;
const KIND_BINDING: i32 = 20;
const KIND_BLOCK: i32 = 21;
const KIND_DIVERGE: i32 = 22;
const KIND_LOOP: i32 = 23;

static WASM_BYTES: OnceLock<Vec<u8>> = OnceLock::new();
static WASM_ENGINE: OnceLock<Engine> = OnceLock::new();
static WASM_MODULE: OnceLock<Module> = OnceLock::new();

fn load_simple_parser_wasm() -> &'static [u8] {
    WASM_BYTES.get_or_init(|| {
        let path = "fixtures/simple_parser.silk";
        let source = fs::read_to_string(path).expect("read simple_parser.silk");
        let artifacts = crate::compile(vec![(path, source.as_str())], path)
            .unwrap_or_else(|err| panic!("{}", err.render_with_source(&source)));
        let CompilationArtifact { content, .. } = artifacts
            .into_iter()
            .find(|artifact| matches!(artifact.kind, ArtifactKind::Wasm))
            .expect("expected wasm artifact for simple_parser.silk");
        content
    })
}

fn wasm_engine() -> &'static Engine {
    WASM_ENGINE.get_or_init(|| {
        let mut config = Config::new();
        config.wasm_reference_types(true);
        config.wasm_gc(true);
        Engine::new(&config).expect("create wasmtime engine")
    })
}

fn wasm_module() -> &'static Module {
    WASM_MODULE.get_or_init(|| {
        Module::new(wasm_engine(), load_simple_parser_wasm())
            .expect("compile simple_parser.wasm")
    })
}

struct WasmParser {
    store: Store<()>,
    instance: Instance,
    input: Memory,
    source: String,
}
impl WasmParser {
    fn new() -> Self {
        let engine = wasm_engine();
        let module = wasm_module();
        let mut store = Store::new(engine, ());
        let instance = Instance::new(&mut store, module, &[]).expect("instantiate module");
        let input = instance
            .get_memory(&mut store, "input")
            .expect("input memory export");
        Self {
            store,
            instance,
            input,
            source: String::new(),
        }
    }

    fn call0(&mut self, name: &str) -> i32 {
        let func = self
            .instance
            .get_typed_func::<(), i32>(&mut self.store, name)
            .unwrap_or_else(|err| panic!("missing export {name}: {err}"));
        func.call(&mut self.store, ())
            .unwrap_or_else(|err| panic!("call {name}: {err}"))
    }

    fn call1(&mut self, name: &str, arg: i32) -> i32 {
        let func = self
            .instance
            .get_typed_func::<i32, i32>(&mut self.store, name)
            .unwrap_or_else(|err| panic!("missing export {name}: {err}"));
        func.call(&mut self.store, arg)
            .unwrap_or_else(|err| panic!("call {name}: {err}"))
    }

    fn call2(&mut self, name: &str, arg0: i32, arg1: i32) -> i32 {
        let func = self
            .instance
            .get_typed_func::<(i32, i32), i32>(&mut self.store, name)
            .unwrap_or_else(|err| panic!("missing export {name}: {err}"));
        func.call(&mut self.store, (arg0, arg1))
            .unwrap_or_else(|err| panic!("call {name}: {err}"))
    }

    fn write_input(&mut self, text: &str) {
        self.source.clear();
        self.source.push_str(text);
        let bytes = self.source.as_bytes();
        let data = self.input.data_mut(&mut self.store);
        assert!(
            bytes.len() + 1 <= data.len(),
            "input too long for parser buffer"
        );
        data.fill(0);
        data[..bytes.len()].copy_from_slice(bytes);
        data[bytes.len()] = 0;
    }

    fn span_string(&self, start: i32, length: i32) -> String {
        let start = start.max(0) as usize;
        let length = length.max(0) as usize;
        let bytes = self.source.as_bytes();
        assert!(start + length <= bytes.len(), "span out of bounds");
        String::from_utf8_lossy(&bytes[start..start + length]).to_string()
    }

    fn parse_root(&mut self) -> Result<i32, i32> {
        let root = self.call0("parse");
        let error = self.call0("get_state_error");
        if error == -1 { Ok(root) } else { Err(error) }
    }

    fn parse_block(&mut self, source: &str) -> Result<Expression, i32> {
        self.write_input(source);
        let root = self.parse_root()?;
        Ok(self.expression_from_node(root))
    }

    fn parse_single_expression(&mut self, source: &str) -> Result<Expression, i32> {
        let block = self.parse_block(source)?;
        let ExpressionKind::Block(items) = block.kind else {
            panic!("expected block from wasm parser");
        };
        assert_eq!(items.len(), 1, "expected single expression");
        Ok(items.into_iter().next().expect("single expression"))
    }

    fn expression_from_node(&mut self, idx: i32) -> Expression {
        let span = self.node_span(idx);
        match self.call1("get_kind_tag", idx) {
            KIND_LITERAL => self.literal_expression(idx, span),
            KIND_IDENTIFIER => {
                let start = self.call1("get_identifier_start", idx);
                let length = self.call1("get_identifier_length", idx);
                let name = self.span_string(start, length);
                Expression::new(ExpressionKind::Identifier(Identifier::new(name)), span)
            }
            KIND_OPERATION => {
                let op_start = self.call1("get_operation_operator_start", idx);
                let op_len = self.call1("get_operation_operator_length", idx);
                let operator = self.span_string(op_start, op_len);
                let left_idx = self.call1("get_operation_left", idx);
                let right_idx = self.call1("get_operation_right", idx);
                let left = Rc::new(self.expression_from_node(left_idx));
                let right = Rc::new(self.expression_from_node(right_idx));
                Expression::new(
                    ExpressionKind::Operation {
                        operator,
                        left,
                        right,
                    },
                    span,
                )
            }
            KIND_ASSIGNMENT => {
                let target_kind = self.call1("get_assignment_target_kind", idx);
                let target = match target_kind {
                    0 => {
                        let start = self.call1("get_assignment_target_identifier_start", idx);
                        let length = self.call1("get_assignment_target_identifier_length", idx);
                        let name = self.span_string(start, length);
                        let target_span = SourceSpan::new(start as usize, length as usize);
                        LValue::Identifier(Identifier::new(name), target_span)
                    }
                    _ => panic!("unsupported assignment target kind {target_kind}"),
                };
                let expr_idx = self.call1("get_assignment_expr", idx);
                let expr = Rc::new(self.expression_from_node(expr_idx));
                Expression::new(ExpressionKind::Assignment { target, expr }, span)
            }
            KIND_FUNCTION_CALL => {
                let function_idx = self.call1("get_function_call_function", idx);
                let argument_idx = self.call1("get_function_call_argument", idx);
                let function = Rc::new(self.expression_from_node(function_idx));
                let argument = Rc::new(self.expression_from_node(argument_idx));
                Expression::new(
                    ExpressionKind::FunctionCall { function, argument },
                    span,
                )
            }
            KIND_TYPE_PROPERTY => {
                let object_idx = self.call1("get_type_property_access_object", idx);
                let prop_start = self.call1("get_type_property_access_property_start", idx);
                let prop_len = self.call1("get_type_property_access_property_length", idx);
                let object = Rc::new(self.expression_from_node(object_idx));
                let property = self.span_string(prop_start, prop_len);
                Expression::new(
                    ExpressionKind::TypePropertyAccess { object, property },
                    span,
                )
            }
            KIND_BINDING => {
                let pattern = self.binding_pattern_from_binding_node(idx);
                let expr_idx = self.call1("get_binding_expr", idx);
                let expr = self.expression_from_node(expr_idx);
                Expression::new(ExpressionKind::Binding(Rc::new(Binding { pattern, expr })), span)
            }
            KIND_BLOCK => {
                let count = self.call1("get_block_count", idx);
                let mut items = Vec::with_capacity(count.max(0) as usize);
                for pos in 0..count.max(0) {
                    let item = self.call2("get_block_item", idx, pos);
                    items.push(self.expression_from_node(item));
                }
                let block_span = if let (Some(first), Some(last)) = (items.first(), items.last()) {
                    let start = first.span.start();
                    let end = last.span.end();
                    SourceSpan::new(start, end.saturating_sub(start))
                } else {
                    span
                };
                Expression::new(ExpressionKind::Block(items), block_span)
            }
            KIND_FUNCTION => {
                let parameter = self.function_param_pattern_from_node(idx);
                let return_type_idx = self.call1("get_function_return_type", idx);
                let return_type = if return_type_idx == -1 {
                    None
                } else {
                    Some(Rc::new(self.expression_from_node(return_type_idx)))
                };
                let body_idx = self.call1("get_function_body", idx);
                let body = Rc::new(self.expression_from_node(body_idx));
                Expression::new(
                    ExpressionKind::Function {
                        parameter,
                        return_type,
                        body,
                    },
                    span,
                )
            }
            KIND_FUNCTION_TYPE => {
                let param_idx = self.call1("get_function_type_param", idx);
                let ret_idx = self.call1("get_function_type_return", idx);
                let parameter = Rc::new(self.expression_from_node(param_idx));
                let return_type = Rc::new(self.expression_from_node(ret_idx));
                Expression::new(
                    ExpressionKind::FunctionType {
                        parameter,
                        return_type,
                    },
                    span,
                )
            }
            KIND_IF => {
                let cond_idx = self.call1("get_if_condition", idx);
                let then_idx = self.call1("get_if_then", idx);
                let else_idx = self.call1("get_if_else", idx);
                let condition = Rc::new(self.expression_from_node(cond_idx));
                let then_branch = Rc::new(self.expression_from_node(then_idx));
                let else_branch = Rc::new(self.expression_from_node(else_idx));
                Expression::new(
                    ExpressionKind::If {
                        condition,
                        then_branch,
                        else_branch,
                    },
                    span,
                )
            }
            KIND_LOOP => {
                let body_idx = self.call1("get_loop_body", idx);
                let body = Rc::new(self.expression_from_node(body_idx));
                Expression::new(ExpressionKind::Loop { body }, span)
            }
            KIND_DIVERGE => {
                let divergance_type = if self.call1("get_diverge_type", idx) == 0 {
                    DivergeExpressionType::Return
                } else {
                    DivergeExpressionType::Break
                };
                let value_idx = self.call1("get_diverge_value", idx);
                let value = Rc::new(self.expression_from_node(value_idx));
                Expression::new(
                    ExpressionKind::Diverge {
                        value,
                        divergance_type,
                    },
                    span,
                )
            }
            KIND_STRUCT => {
                let mut fields = Vec::new();
                let mut current = self.call1("get_struct_fields", idx);
                let mut positional_index = 0;
                while current != -1 {
                    let field_node = self.call1("get_list_value", current);
                    let name = self.binding_name_or_positional(field_node, &mut positional_index);
                    let expr_idx = self.call1("get_binding_expr", field_node);
                    let expr = self.expression_from_node(expr_idx);
                    fields.push((Identifier::new(name), expr));
                    current = self.call1("get_list_next", current);
                }
                Expression::new(ExpressionKind::Struct(fields), span)
            }
            KIND_ARRAY_REPEAT => {
                let value_idx = self.call1("get_array_repeat_value", idx);
                let count_idx = self.call1("get_array_repeat_count", idx);
                let value = Rc::new(self.expression_from_node(value_idx));
                let count = Rc::new(self.expression_from_node(count_idx));
                Expression::new(ExpressionKind::ArrayRepeat { value, count }, span)
            }
            KIND_ATTACH_IMPLEMENTATION => {
                let type_idx = self.call1("get_attach_type_expr", idx);
                let impl_idx = self.call1("get_attach_implementation", idx);
                let type_expr = Rc::new(self.expression_from_node(type_idx));
                let implementation = Rc::new(self.expression_from_node(impl_idx));
                Expression::new(
                    ExpressionKind::AttachImplementation {
                        type_expr,
                        implementation,
                    },
                    span,
                )
            }
            KIND_ARRAY_INDEX
            | KIND_INTRINSIC_TYPE
            | KIND_BOX_TYPE
            | KIND_INTRINSIC_OPERATION
            | KIND_ENUM_TYPE
            | KIND_MATCH
            | KIND_ENUM_VALUE
            | KIND_ENUM_CONSTRUCTOR => {
                panic!("unsupported node kind {idx}");
            }
            other => panic!("unknown node kind {other} for node {idx}"),
        }
    }

    fn literal_expression(&mut self, idx: i32, span: SourceSpan) -> Expression {
        let string_start = self.call1("get_literal_string_start", idx);
        if string_start != -1 {
            let length = self.call1("get_literal_string_length", idx);
            let bytes = self.span_string(string_start, length).into_bytes();
            return Expression::new(
                ExpressionKind::Literal(crate::parsing::ExpressionLiteral::String(bytes)),
                span,
            );
        }
        let char_value = self.call1("get_literal_char", idx);
        if char_value != -1 {
            return Expression::new(
                ExpressionKind::Literal(crate::parsing::ExpressionLiteral::Char(
                    char_value as u8,
                )),
                span,
            );
        }
        let bool_value = self.call1("get_literal_boolean", idx);
        if bool_value != -1 {
            return Expression::new(
                ExpressionKind::Literal(crate::parsing::ExpressionLiteral::Boolean(
                    bool_value != 0,
                )),
                span,
            );
        }
        let num_value = self.call1("get_literal_number", idx);
        if num_value != -1 {
            return Expression::new(
                ExpressionKind::Literal(crate::parsing::ExpressionLiteral::Number(num_value)),
                span,
            );
        }
        panic!("unsupported literal at {idx}");
    }

    fn binding_pattern_from_binding_node(&mut self, idx: i32) -> BindingPattern {
        match self.call1("get_binding_pattern_tag", idx) {
            0 => {
                let name = self.binding_name(idx);
                let span = self.binding_pattern_span(idx);
                BindingPattern::Identifier(Identifier::new(name), span)
            }
            1 => {
                let span = self.binding_pattern_span(idx);
                let literal = crate::parsing::ExpressionLiteral::Number(
                    self.call1("get_literal_number", idx),
                );
                BindingPattern::Literal(literal, span)
            }
            2 => {
                let span = self.binding_pattern_span(idx);
                let fields_head = self.call1("get_binding_pattern_struct_fields", idx);
                let fields = self.struct_pattern_fields(fields_head);
                BindingPattern::Struct(fields, span)
            }
            3 => {
                let span = self.binding_pattern_span(idx);
                let enum_type_idx = self.call1("get_binding_pattern_enum_type", idx);
                let enum_type = self.expression_from_node(enum_type_idx);
                let start = self.call1("get_binding_pattern_enum_variant_start", idx);
                let length = self.call1("get_binding_pattern_enum_variant_length", idx);
                let variant = Identifier::new(self.span_string(start, length));
                let payload_idx = self.call1("get_binding_pattern_enum_payload", idx);
                let payload = if payload_idx == -1 {
                    None
                } else {
                    Some(Box::new(self.binding_pattern_from_expression(payload_idx)))
                };
                BindingPattern::EnumVariant {
                    enum_type: Box::new(enum_type),
                    variant,
                    payload,
                    span,
                }
            }
            4 => {
                let span = self.binding_pattern_span(idx);
                let pattern_idx = self.call1("get_binding_pattern_typehint_pattern", idx);
                let type_idx = self.call1("get_binding_pattern_typehint_type", idx);
                let pattern = self.binding_pattern_from_expression(pattern_idx);
                let type_expr = self.expression_from_node(type_idx);
                BindingPattern::TypeHint(Box::new(pattern), Box::new(type_expr), span)
            }
            5 => {
                let span = self.binding_pattern_span(idx);
                let annotations_head = self.call1("get_binding_pattern_annotations", idx);
                let annotations = self.list_to_expressions(annotations_head);
                let pattern_idx = self.call1("get_binding_pattern_annotated_pattern", idx);
                let pattern = self.binding_pattern_from_expression(pattern_idx);
                BindingPattern::Annotated {
                    annotations,
                    pattern: Box::new(pattern),
                    span,
                }
            }
            other => panic!("unknown binding pattern tag {other}"),
        }
    }

    fn function_param_pattern_from_node(&mut self, idx: i32) -> BindingPattern {
        match self.call1("get_function_param_pattern_tag", idx) {
            0 => {
                let start = self.call1("get_function_param_pattern_identifier_start", idx);
                let length = self.call1("get_function_param_pattern_identifier_length", idx);
                let name = self.span_string(start, length);
                BindingPattern::Identifier(Identifier::new(name), self.function_param_pattern_span(idx))
            }
            2 => {
                let fields_head = self.call1("get_function_param_pattern_struct_fields", idx);
                let fields = self.struct_pattern_fields(fields_head);
                BindingPattern::Struct(fields, self.function_param_pattern_span(idx))
            }
            3 => {
                let enum_type_idx = self.call1("get_function_param_pattern_enum_type", idx);
                let enum_type = self.expression_from_node(enum_type_idx);
                let start = self.call1("get_function_param_pattern_enum_variant_start", idx);
                let length = self.call1("get_function_param_pattern_enum_variant_length", idx);
                let variant = Identifier::new(self.span_string(start, length));
                let payload_idx = self.call1("get_function_param_pattern_enum_payload", idx);
                let payload = if payload_idx == -1 {
                    None
                } else {
                    Some(Box::new(self.binding_pattern_from_expression(payload_idx)))
                };
                BindingPattern::EnumVariant {
                    enum_type: Box::new(enum_type),
                    variant,
                    payload,
                    span: self.function_param_pattern_span(idx),
                }
            }
            4 => {
                let pattern_idx = self.call1("get_function_param_pattern_typehint_pattern", idx);
                let type_idx = self.call1("get_function_param_pattern_typehint_type", idx);
                let pattern = self.binding_pattern_from_expression(pattern_idx);
                let type_expr = self.expression_from_node(type_idx);
                BindingPattern::TypeHint(
                    Box::new(pattern),
                    Box::new(type_expr),
                    self.function_param_pattern_span(idx),
                )
            }
            5 => {
                let annotations_head = self.call1("get_function_param_pattern_annotations", idx);
                let annotations = self.list_to_expressions(annotations_head);
                let pattern_idx = self.call1("get_function_param_pattern_annotated_pattern", idx);
                let pattern = self.binding_pattern_from_expression(pattern_idx);
                BindingPattern::Annotated {
                    annotations,
                    pattern: Box::new(pattern),
                    span: self.function_param_pattern_span(idx),
                }
            }
            other => panic!("unsupported function param pattern tag {other}"),
        }
    }

    fn binding_pattern_from_expression(&mut self, idx: i32) -> BindingPattern {
        match self.call1("get_kind_tag", idx) {
            KIND_LITERAL => {
                let span = self.node_span(idx);
                let value = self.call1("get_literal_number", idx);
                BindingPattern::Literal(crate::parsing::ExpressionLiteral::Number(value), span)
            }
            KIND_IDENTIFIER => {
                let start = self.call1("get_identifier_start", idx);
                let length = self.call1("get_identifier_length", idx);
                BindingPattern::Identifier(Identifier::new(self.span_string(start, length)), self.node_span(idx))
            }
            KIND_STRUCT => {
                let span = self.node_span(idx);
                let fields_head = self.call1("get_struct_fields", idx);
                let fields = self.struct_pattern_fields(fields_head);
                BindingPattern::Struct(fields, span)
            }
            KIND_OPERATION => {
                let op_start = self.call1("get_operation_operator_start", idx);
                let op_len = self.call1("get_operation_operator_length", idx);
                let operator = self.span_string(op_start, op_len);
                if operator == ":" {
                    let pattern_idx = self.call1("get_operation_left", idx);
                    let type_idx = self.call1("get_operation_right", idx);
                    let pattern = self.binding_pattern_from_expression(pattern_idx);
                    let type_expr = self.expression_from_node(type_idx);
                    BindingPattern::TypeHint(
                        Box::new(pattern),
                        Box::new(type_expr),
                        self.node_span(idx),
                    )
                } else {
                    BindingPattern::Identifier(Identifier::new(String::new()), self.node_span(idx))
                }
            }
            KIND_TYPE_PROPERTY => {
                let object_idx = self.call1("get_type_property_access_object", idx);
                let start = self.call1("get_type_property_access_property_start", idx);
                let length = self.call1("get_type_property_access_property_length", idx);
                let enum_type = self.expression_from_node(object_idx);
                let variant = Identifier::new(self.span_string(start, length));
                BindingPattern::EnumVariant {
                    enum_type: Box::new(enum_type),
                    variant,
                    payload: None,
                    span: self.node_span(idx),
                }
            }
            KIND_FUNCTION_CALL => {
                let function_idx = self.call1("get_function_call_function", idx);
                if self.call1("get_kind_tag", function_idx) == KIND_TYPE_PROPERTY {
                    let object_idx = self.call1("get_type_property_access_object", function_idx);
                    let start = self.call1("get_type_property_access_property_start", function_idx);
                    let length = self.call1("get_type_property_access_property_length", function_idx);
                    let enum_type = self.expression_from_node(object_idx);
                    let variant = Identifier::new(self.span_string(start, length));
                    let payload_idx = self.call1("get_function_call_argument", idx);
                    let payload = self.binding_pattern_from_expression(payload_idx);
                    BindingPattern::EnumVariant {
                        enum_type: Box::new(enum_type),
                        variant,
                        payload: Some(Box::new(payload)),
                        span: self.node_span(idx),
                    }
                } else {
                    let annotations = self.extract_binding_annotations(function_idx);
                    let pattern_idx = self.call1("get_function_call_argument", idx);
                    let pattern = self.binding_pattern_from_expression(pattern_idx);
                    BindingPattern::Annotated {
                        annotations,
                        pattern: Box::new(pattern),
                        span: self.node_span(idx),
                    }
                }
            }
            other => panic!("unsupported binding pattern expression kind {other}"),
        }
    }

    fn extract_binding_annotations(&mut self, mut current: i32) -> Vec<Expression> {
        let mut args = Vec::new();
        loop {
            if self.call1("get_kind_tag", current) != KIND_FUNCTION_CALL {
                break;
            }
            let function_idx = self.call1("get_function_call_function", current);
            if self.call1("get_kind_tag", function_idx) == KIND_FUNCTION_CALL {
                let argument_idx = self.call1("get_function_call_argument", current);
                args.push(self.expression_from_node(argument_idx));
                current = function_idx;
            } else {
                break;
            }
        }
        args.push(self.expression_from_node(current));
        args.reverse();
        args
    }

    fn list_to_expressions(&mut self, head: i32) -> Vec<Expression> {
        let mut out = Vec::new();
        let mut current = head;
        while current != -1 {
            let value = self.call1("get_list_value", current);
            out.push(self.expression_from_node(value));
            current = self.call1("get_list_next", current);
        }
        out
    }

    fn struct_pattern_fields(&mut self, head: i32) -> Vec<(Identifier, BindingPattern)> {
        let mut fields = Vec::new();
        let mut current = head;
        let mut positional_index = 0;
        while current != -1 {
            let field_node = self.call1("get_list_value", current);
            let name = self.binding_name_or_positional(field_node, &mut positional_index);
            let pattern_expr = self.call1("get_binding_expr", field_node);
            let pattern = self.binding_pattern_from_expression(pattern_expr);
            fields.push((Identifier::new(name), pattern));
            current = self.call1("get_list_next", current);
        }
        fields
    }

    fn binding_name_or_positional(&mut self, idx: i32, positional_index: &mut i32) -> String {
        let start = self.call1("get_binding_name_start", idx);
        let length = self.call1("get_binding_name_length", idx);
        if length == 0 || start == -1 {
            let name = positional_index.to_string();
            *positional_index += 1;
            name
        } else {
            self.span_string(start, length)
        }
    }

    fn binding_name(&mut self, idx: i32) -> String {
        let start = self.call1("get_binding_name_start", idx);
        let length = self.call1("get_binding_name_length", idx);
        if length == 0 || start == -1 {
            String::new()
        } else {
            self.span_string(start, length)
        }
    }

    fn binding_pattern_span(&mut self, idx: i32) -> SourceSpan {
        let start = self.call1("get_binding_pattern_span_start", idx);
        let length = self.call1("get_binding_pattern_span_length", idx);
        SourceSpan::new(start as usize, length as usize)
    }

    fn function_param_pattern_span(&mut self, idx: i32) -> SourceSpan {
        let start = self.call1("get_function_param_pattern_span_start", idx);
        let length = self.call1("get_function_param_pattern_span_length", idx);
        SourceSpan::new(start as usize, length as usize)
    }

    fn node_span(&mut self, idx: i32) -> SourceSpan {
        let start = self.call1("get_span_start", idx);
        let length = self.call1("get_span_length", idx);
        SourceSpan::new(start as usize, length as usize)
    }
}
fn assert_wasm_block_matches_rust(source: &str) {
    let (expected, remaining) = crate::parsing::parse_block(source)
        .unwrap_or_else(|err| panic!("{}", err.render_with_source(source)));
    assert!(remaining.trim().is_empty());
    let mut parser = WasmParser::new();
    let actual = parser.parse_block(source).expect("wasm parse");
    assert_eq!(actual, expected);
}

fn assert_wasm_single_matches_rust(source: &str) {
    let (expected, remaining) = crate::parsing::parse_operation_expression(source)
        .unwrap_or_else(|err| panic!("{}", err.render_with_source(source)));
    assert!(remaining.trim().is_empty());
    let mut parser = WasmParser::new();
    let actual = parser.parse_single_expression(source).expect("wasm parse");
    assert_eq!(actual, expected);
}

#[test]
fn wasm_parse_basic_let() {
    assert_wasm_block_matches_rust(
        "
x := 42;
y: i32 := x
    ",
    );
}

#[test]
fn wasm_parse_binding_with_export_annotation() {
    assert_wasm_block_matches_rust(
        "
(export js) foo := 42;
(export wasm) (export js) bar := foo;
bar
    ",
    );
}

#[test]
fn wasm_parse_struct_pattern_with_inner_multiple_annotations() {
    assert_wasm_block_matches_rust(
        "
{ foo = (export js) mut foo_binding, bar } := value;
foo_binding
    ",
    );
}

#[test]
fn wasm_parse_struct_pattern_with_inner_export_annotation() {
    assert_wasm_block_matches_rust(
        "
{ foo = (export js) foo_binding, bar } := value;
foo_binding
    ",
    );
}

#[test]
fn wasm_parse_mutable_binding_and_assignment() {
    assert_wasm_block_matches_rust(
        "
mut foo := 1;
foo = 2
    ",
    );
}

#[test]
fn wasm_parse_for_loop_desugars_to_iterator_binding() {
    assert_wasm_block_matches_rust("for foo in bar do (foo)");
}

#[test]
fn wasm_parse_operation_expression_precedence() {
    assert_wasm_single_matches_rust("1 + 2 * 3 / 4 - 5");
}

#[test]
fn wasm_parse_function_binding_and_call() {
    assert_wasm_block_matches_rust(
        "
foo := (bar: i32) => bar + 1;
foo(123)
    ",
    );
}

#[test]
fn wasm_parse_function_struct_parameter_pattern() {
    assert_wasm_single_matches_rust(
        "{bar1: i32, bar2: i32} => (
    bar1 + bar2
)",
    );
}

#[test]
fn wasm_parse_struct_literal_named_and_tuple_fields() {
    let source = "{foo = 10, 20, bar = 30}";
    let (expected, remaining) = crate::parsing::parse_isolated_expression(source)
        .unwrap_or_else(|err| panic!("{}", err.render_with_source(source)));
    assert!(remaining.trim().is_empty());
    let mut parser = WasmParser::new();
    let actual = parser
        .parse_single_expression(source)
        .expect("wasm parse");
    assert_eq!(actual, expected);
}

#[test]
fn wasm_parse_struct_binding_pattern_named_fields() {
    let source = "{foo = first: i32, second: i32}";
    let (expected, remaining) =
        crate::parsing::parse_operation_expression(source).expect("pattern parse");
    assert!(remaining.trim().is_empty());
    let mut parser = WasmParser::new();
    let actual = parser
        .parse_single_expression(source)
        .expect("wasm parse");
    assert_eq!(actual, expected);
}

#[test]
fn wasm_parse_struct_property_access_chain() {
    assert_wasm_single_matches_rust("foo::bar::baz");
}

#[test]
fn wasm_parse_struct_property_access_then_call() {
    assert_wasm_single_matches_rust("foo.bar baz");
}

#[test]
fn wasm_diagnostics_include_binding_pattern_source_reference() {
    let source = ":= 5;";
    let err = crate::parsing::parse_block(source).expect_err("binding should fail");
    let span = err.span.expect("rust diagnostic span");
    let mut parser = WasmParser::new();
    parser.write_input(source);
    let err_pos = parser.parse_root().expect_err("wasm parse should fail");
    assert_eq!(err_pos as usize, span.start());
}

#[test]
fn wasm_diagnostics_include_grouping_closure_reference() {
    let source = "(1 + 2";
    let err = crate::parsing::parse_isolated_expression(source)
        .expect_err("missing ) should fail");
    let span = err.span.expect("rust diagnostic span");
    let mut parser = WasmParser::new();
    parser.write_input(source);
    let err_pos = parser.parse_root().expect_err("wasm parse should fail");
    assert_eq!(err_pos as usize, span.start());
}
