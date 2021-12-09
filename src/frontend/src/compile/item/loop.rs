use crate::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
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
    pub fn r#loop(span: Span, body: Item) -> Self {
        Item::new(span, ItemKind::Loop(LoopItem::new(body)))
    }
}
