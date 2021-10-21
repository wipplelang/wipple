use crate::lower::*;
use serde::Serialize;

#[derive(Clone, Serialize)]
pub struct VariableItem {
    pub variable: VariableId,
}

impl SpannedItem {
    pub fn variable(span: Span, variable: VariableId) -> Self {
        SpannedItem::new(span, Item::Variable(VariableItem { variable }))
    }
}
