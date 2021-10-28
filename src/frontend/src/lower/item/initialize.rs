use crate::lower::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct InitializeItem {
    pub variable: VariableId,
    pub value: Box<Item>,
}

impl Item {
    pub fn initialize(span: Span, variable: VariableId, value: Item) -> Self {
        Item::new(
            span,
            ItemKind::Initialize(InitializeItem {
                variable,
                value: Box::new(value),
            }),
        )
    }
}
