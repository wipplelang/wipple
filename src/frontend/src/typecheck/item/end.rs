use crate::{typecheck::*, *};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EndItem {
    pub value: Box<Item>,
}

impl EndItem {
    pub fn new(value: Item) -> Self {
        EndItem {
            value: Box::new(value),
        }
    }
}

impl Item {
    pub fn end(compile_info: compile::ItemInfo, ty: TypeSchema, value: Item) -> Self {
        Item::new(compile_info, ty, ItemKind::End(EndItem::new(value)))
    }
}
