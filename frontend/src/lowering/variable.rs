use serde::Serialize;
use wipple_diagnostics::*;
use wipple_parser::Intern;

id! {
    pub struct VariableId;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct Variable {
    pub id: VariableId,
    pub declaration_span: Span,
    pub name: Intern<String>,
}

impl Variable {
    pub fn new(declaration_span: Span, name: Intern<String>) -> Self {
        Variable {
            id: VariableId::new(),
            declaration_span,
            name,
        }
    }
}
