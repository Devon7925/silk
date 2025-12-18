use crate::{interpret::Context, parsing::{BindingPattern, DivergeExpressionType, Expression, ExpressionLiteral, Identifier, IntrinsicOperation, IntrinsicType, LValue, TargetLiteral}};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntermediateKind {
    IntrinsicOperation(IntrinsicOperation),
    Match {
        value: Box<IntermediateKind>,
        branches: Vec<(BindingPattern, IntermediateKind)>,
    },
    If {
        condition: Box<IntermediateKind>,
        then_branch: Box<IntermediateKind>,
        else_branch: Box<IntermediateKind>,
    },
    Struct(Vec<(Identifier, IntermediateKind)>),
    Literal(ExpressionLiteral),
    Identifier(Identifier),
    Assignment {
        target: LValue,
        expr: Box<IntermediateKind>,
    },
    FunctionCall {
        function: usize,
        argument: Box<IntermediateKind>,
    },
    PropertyAccess {
        object: Box<IntermediateKind>,
        property: String,
    },
    Binding(Box<IntermediateBinding>),
    Block(Vec<IntermediateKind>),
    Diverge {
        value: Box<IntermediateKind>,
        divergance_type: DivergeExpressionType,
    },
    Loop {
        body: Box<IntermediateKind>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntermediateBindingPattern {
    Identifier(Identifier),
    Literal(ExpressionLiteral),
    Struct(Vec<(usize, IntermediateBindingPattern)>),
    EnumVariant {
        variant: Identifier,
        payload: Option<Box<IntermediateBindingPattern>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntermediateBinding {
    pub pattern: BindingPattern,
    pub binding_type: IntermediateType,
    pub expr: IntermediateKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntermediateType {
    I32,
    Struct(Vec<IntermediateType>)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntermediateFunction {
    input_type: Box<IntermediateType>,
    return_type: Box<IntermediateType>,
    parameter: BindingPattern,
    body: Box<IntermediateKind>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntermediateExportType {
    Function,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntermediateExport {
    target: TargetLiteral,
    name: String,
    export_type: IntermediateExportType,
    index: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntermediateResult {
    pub functions: Vec<IntermediateFunction>,
    pub exports: Vec<IntermediateExport>,
}

pub fn context_to_intermediate(context: Context) -> IntermediateResult {
    todo!()
}

pub fn expression_to_intermediate(expr: Expression) -> IntermediateKind {
    todo!()
}