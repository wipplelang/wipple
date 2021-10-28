use crate::lower::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct VariableItem {
    pub variable: VariableId,
}

impl Item {
    pub fn variable(span: Span, variable: VariableId) -> Self {
        Item::new(span, ItemKind::Variable(VariableItem { variable }))
    }
}
