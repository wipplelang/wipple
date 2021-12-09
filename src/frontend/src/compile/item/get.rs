use crate::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct GetItem {
    pub mutable: Box<Item>,
}

impl GetItem {
    pub fn new(mutable: Item) -> Self {
        GetItem {
            mutable: Box::new(mutable),
        }
    }
}

impl Item {
    pub fn get(span: Span, mutable: Item) -> Self {
        Item::new(span, ItemKind::Get(GetItem::new(mutable)))
    }
}
