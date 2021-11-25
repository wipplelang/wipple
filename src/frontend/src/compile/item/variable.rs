use crate::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct VariableItem {
    pub variable: VariableId,
}

impl VariableItem {
    pub fn new(variable: VariableId) -> Self {
        VariableItem { variable }
    }
}

impl Item {
    pub fn variable(span: Span, variable: VariableId) -> Self {
        Item::new(span, ItemKind::Variable(VariableItem::new(variable)))
    }
}
