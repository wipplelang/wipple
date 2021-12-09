use crate::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
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
    pub fn end(span: Span, value: Item) -> Self {
        Item::new(span, ItemKind::End(EndItem::new(value)))
    }
}
