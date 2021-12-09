use crate::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct MutableItem {
    pub value: Box<Item>,
}

impl MutableItem {
    pub fn new(value: Item) -> Self {
        MutableItem {
            value: Box::new(value),
        }
    }
}

impl Item {
    pub fn mutable(span: Span, value: Item) -> Self {
        Item::new(span, ItemKind::Mutable(MutableItem::new(value)))
    }
}
