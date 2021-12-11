use crate::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
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
    pub fn r#return(span: Span, value: Item) -> Self {
        Item::new(span, ItemKind::Return(ReturnItem::new(value)))
    }
}
