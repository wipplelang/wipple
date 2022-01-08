use crate::{typecheck::*, *};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LoopItem {
    pub body: Box<Item>,
}

impl LoopItem {
    pub fn new(body: Item) -> Self {
        LoopItem {
            body: Box::new(body),
        }
    }
}

impl Item {
    pub fn r#loop(compile_info: compile::ItemInfo, ty: Scheme, body: Item) -> Self {
        Item::new(compile_info, ty, ItemKind::Loop(LoopItem::new(body)))
    }
}
