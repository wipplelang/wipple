use crate::{typecheck::*, *};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct UnitItem;

impl UnitItem {
    pub fn new() -> Self {
        UnitItem
    }
}

impl Item {
    pub fn unit(compile_info: compile::ItemInfo) -> Self {
        Item::new(
            compile_info,
            Scheme::Type(BUILTIN_TYPES.unit.clone()),
            ItemKind::Unit(UnitItem::new()),
        )
    }
}
