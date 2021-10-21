use std::collections::HashSet;

use crate::lower::*;
use serde::Serialize;
use wipple_diagnostics::*;

pub struct Info<'a> {
    pub diagnostics: &'a mut Diagnostics,
    pub declared_variables: Vec<Variable>,
    pub used_variables: HashSet<VariableId>,
    pub functions: HashMap<FunctionId, Function>,
}

impl<'a> Info<'a> {
    pub fn new(diagnostics: &'a mut Diagnostics) -> Self {
        Info {
            diagnostics,
            declared_variables: Default::default(),
            used_variables: Default::default(),
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
