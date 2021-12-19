use crate::{typecheck::*, *};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldItem {
    pub value: Box<Item>,
    pub index: usize,
}

impl FieldItem {
    pub fn new(value: Item, index: usize) -> Self {
        FieldItem {
            value: Box::new(value),
            index,
        }
    }
}

impl typecheck::Item {
    pub fn field(
        compile_info: compile::ItemInfo,
        ty: TypeSchema,
        value: Item,
        index: usize,
    ) -> Self {
        Item::new(compile_info, ty, ItemKind::Field(FieldItem::new(value, index)))
    }
}
