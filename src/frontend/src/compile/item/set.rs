use crate::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct SetItem {
    pub mutable: Box<Item>,
    pub value: Box<Item>,
}

impl SetItem {
    pub fn new(mutable: Item, value: Item) -> Self {
        SetItem {
            mutable: Box::new(mutable),
            value: Box::new(value),
        }
    }
}

impl Item {
    pub fn set(span: Span, mutable: Item, value: Item) -> Self {
        Item::new(span, ItemKind::Set(SetItem::new(mutable, value)))
    }
}
