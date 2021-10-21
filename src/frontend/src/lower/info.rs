use crate::lower::*;
use serde::Serialize;
use std::collections::HashSet;
use wipple_diagnostics::*;
use wipple_parser::decimal::Decimal;

pub struct Info<'a> {
    pub diagnostics: &'a mut Diagnostics,
    pub declared_variables: Vec<Variable>,
    pub used_variables: HashSet<VariableId>,
    pub constants: HashMap<ConstantValue, ConstantId>,
    pub functions: HashMap<FunctionId, Function>,
}

impl<'a> Info<'a> {
    pub fn new(diagnostics: &'a mut Diagnostics) -> Self {
        Info {
            diagnostics,
            declared_variables: Default::default(),
            used_variables: Default::default(),
            constants: Default::default(),
            functions: Default::default(),
        }
    }
}

#[derive(Clone, Serialize)]
pub struct DebugInfo {
    pub span: Span,
    pub declared_name: Option<LocalIntern<String>>,
}

impl DebugInfo {
    pub fn new(span: Span) -> Self {
        DebugInfo {
            span,
            declared_name: None,
        }
    }
}

id! {
    pub struct ConstantId;
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct Constant {
    pub id: ConstantId,
    pub value: ConstantValue,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum ConstantValue {
    Number(LocalIntern<Decimal>),
    Text(LocalIntern<String>),
}

impl Constant {
    pub fn with_id(id: ConstantId, value: ConstantValue) -> Self {
        Constant { id, value }
    }
}

impl Info<'_> {
    pub fn add_constant(&mut self, value: ConstantValue) -> ConstantId {
        *self.constants.entry(value).or_insert_with(ConstantId::new)
    }
}

id! {
    pub struct FunctionId;
}

#[derive(Serialize)]
pub struct Function {
    pub id: FunctionId,
    pub body: SpannedItem,
    pub captures: HashSet<VariableId>,
}

impl Function {
    pub fn new(body: SpannedItem, captures: HashSet<VariableId>) -> Self {
        Function {
            id: FunctionId::new(),
            body,
            captures,
        }
    }
}
