use crate::{compile::*, *};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InitializeItem {
    pub binding_info: ItemInfo,
    pub variable: VariableId,
    pub value: Box<Item>,
}

impl InitializeItem {
    pub fn new(binding_info: ItemInfo, variable: VariableId, value: Item) -> Self {
        InitializeItem {
            binding_info,
            variable,
            value: Box::new(value),
        }
    }
}

impl Item {
    pub fn initialize(
        span: Span,
        binding_info: ItemInfo,
        variable: VariableId,
        value: Item,
    ) -> Self {
        Item::new(
            span,
            ItemKind::Initialize(InitializeItem::new(binding_info, variable, value)),
        )
    }
}
