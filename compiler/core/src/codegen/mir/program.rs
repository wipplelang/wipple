use crate::{db::Node, span::Span};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Program {
    pub source_files: Vec<Span>,
    pub source_map: BTreeMap<Node, Span>,
    pub functions: Vec<Function>,
    pub named_tys: Vec<NamedTy>,
    pub main: Option<usize>,
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Function {
    pub type_parameters: Vec<TyParameter>,
    pub captures: Vec<usize>,
    pub inputs: Vec<Local>,
    pub locals: Vec<Local>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Local {
    pub ty: Ty,
    pub mutable: bool,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Statement {
    If {
        branches: Vec<(Condition, Vec<Statement>)>,
        else_branch: Option<Vec<Statement>>,
    },
    Return {
        value: SourceMapped<Expression>,
    },
    Loop {
        body: Vec<Statement>,
    },
    Break,
    Assign {
        local: usize,
        value: SourceMapped<Expression>,
    },
    Trace {
        span: Span,
    },
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Condition {
    True,
    False,
    And {
        left: Box<Condition>,
        right: Box<Condition>,
    },
    Or {
        left: Box<Condition>,
        right: Box<Condition>,
    },
    Variant {
        value: SourceMapped<Expression>,
        variant: usize,
    },
    Initialize {
        local: usize,
        value: SourceMapped<Expression>,
    },
    Mutate {
        local: usize,
        value: usize,
    },
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Expression {
    Function {
        index: usize,
        bounds: Vec<usize>,
    },
    Call {
        function: usize,
        inputs: Vec<usize>,
    },
    Closure(Function),
    Element {
        value: usize,
        index: usize,
    },
    Tuple {
        elements: Vec<usize>,
    },
    Marker,
    Local {
        local: usize,
    },
    MutableLocal {
        local: usize,
    },
    Number {
        value: String,
    },
    Intrinsic {
        intrinsic: Intrinsic<SourceMapped<Box<Expression>>>,
    },
    String {
        value: String,
    },
    Structure {
        fields: Vec<(usize, usize)>,
    },
    Variant {
        variant: usize,
        elements: Vec<usize>,
    },
    VariantElement {
        value: usize,
        variant: usize,
        index: usize,
    },
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Intrinsic<T> {
    Debug {
        value: T,
    },
    StringCount {
        value: T,
    },
    StringConcat {
        left: T,
        right: T,
    },
    External {
        name: T,
        value: T,
    },
    NumberToString {
        value: T,
    },
    StringToNumber {
        value: T,
    },
    Add {
        left: T,
        right: T,
    },
    Sub {
        left: T,
        right: T,
    },
    Mul {
        left: T,
        right: T,
    },
    Div {
        left: T,
        right: T,
    },
    Rem {
        left: T,
        right: T,
    },
    Pow {
        left: T,
        right: T,
    },
    Floor {
        value: T,
    },
    Ceil {
        value: T,
    },
    Sqrt {
        value: T,
    },
    Neg {
        value: T,
    },
    Sin {
        value: T,
    },
    Cos {
        value: T,
    },
    Tan {
        value: T,
    },
    NumberEqual {
        left: T,
        right: T,
        true_variant: usize,
        false_variant: usize,
    },
    StringEqual {
        left: T,
        right: T,
        true_variant: usize,
        false_variant: usize,
    },
    Order {
        left: T,
        right: T,
        is_less_than_variant: usize,
        is_equal_variant: usize,
        is_greater_than_variant: usize,
    },
    EmptyList,
    ListCount {
        value: T,
    },
    ListFirst {
        value: T,
    },
    ListLast {
        value: T,
    },
    ListInitial {
        value: T,
    },
    ListTail {
        value: T,
    },
    ListNth {
        value: T,
        index: T,
    },
    ListAppend {
        value: T,
        element: T,
    },
    ListPrepend {
        value: T,
        element: T,
    },
    ListInsertAt {
        value: T,
        index: T,
        element: T,
    },
    ListRemoveAt {
        value: T,
        index: T,
    },
    StringCharacters {
        value: T,
    },
    RandomNumber {
        min: T,
        max: T,
    },
    Nan,
    IsNan {
        value: T,
        true_variant: usize,
        false_variant: usize,
    },
    HashString {
        value: T,
    },
    Unreachable,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct NamedTy {
    pub parameters: Vec<TyParameter>,
    pub representation: TyRepresentation,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct TyParameter {}

#[derive(Debug, Serialize, Deserialize)]
pub enum Ty {
    Named { index: usize, parameters: Vec<Ty> },
    Tuple { elements: Vec<Ty> },
    Function { inputs: Vec<Ty>, output: Box<Ty> },
    Parameter { index: usize },
}

#[derive(Debug, Serialize, Deserialize)]
pub enum TyRepresentation {
    Opaque,
    Number,
    String,
    Marker,
    Structure { fields: Vec<Ty> },
    Enumeration { variants: Vec<Vec<Ty>> },
}

#[derive(Debug, Serialize, Deserialize)]
pub struct SourceMapped<T> {
    pub node: Option<Node>,
    pub inner: T,
}

impl<T> SourceMapped<T> {
    pub fn new(node: Option<Node>, inner: T) -> Self {
        SourceMapped { node, inner }
    }

    pub fn boxed(self) -> SourceMapped<Box<T>> {
        SourceMapped {
            node: self.node,
            inner: Box::new(self.inner),
        }
    }

    pub fn as_ref(&self) -> SourceMapped<&T> {
        SourceMapped {
            node: self.node,
            inner: &self.inner,
        }
    }

    pub fn as_deref(&self) -> SourceMapped<&T::Target>
    where
        T: std::ops::Deref,
    {
        SourceMapped {
            node: self.node,
            inner: &*self.inner,
        }
    }
}
