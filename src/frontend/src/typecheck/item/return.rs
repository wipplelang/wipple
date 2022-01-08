use crate::{typecheck::*, *};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReturnItem {
    pub value: Box<Item>,
}

impl ReturnItem {
    pub fn new(value: Item) -> Self {
        ReturnItem {
            value: Box::new(value),
        }
    }
}

impl Item {
    pub fn r#return(compile_info: compile::ItemInfo, ty: Scheme, value: Item) -> Self {
        Item::new(compile_info, ty, ItemKind::Return(ReturnItem::new(value)))
    }
}
