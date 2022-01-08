use crate::{typecheck::*, *};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InitializeItem {
    pub binding_info: compile::ItemInfo,
    pub variable: VariableId,
    pub value: Box<Item>,
}

impl InitializeItem {
    pub fn new(binding_info: compile::ItemInfo, variable: VariableId, value: Item) -> Self {
        InitializeItem {
            binding_info,
            variable,
            value: Box::new(value),
        }
    }
}

impl Item {
    pub fn initialize(
        compile_info: compile::ItemInfo,
        binding_info: compile::ItemInfo,
        variable: VariableId,
        value: Item,
    ) -> Self {
        Item::new(
            compile_info,
            Scheme::Type(BUILTIN_TYPES.unit.clone()),
            ItemKind::Initialize(InitializeItem::new(binding_info, variable, value)),
        )
    }
}
