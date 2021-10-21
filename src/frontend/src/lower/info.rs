use crate::lower::*;
use serde::Serialize;
use wipple_diagnostics::*;

pub struct Info<'a> {
    pub diagnostics: &'a mut Diagnostics,
    pub declared_variables: Vec<Variable>,
    pub used_variables: Vec<VariableId>,
}

impl<'a> Info<'a> {
    pub fn new(diagnostics: &'a mut Diagnostics) -> Self {
        Info {
            diagnostics,
            declared_variables: Default::default(),
            used_variables: Default::default(),
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
