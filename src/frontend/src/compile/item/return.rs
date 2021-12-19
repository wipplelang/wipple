use crate::{compile::*, *};
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
    pub fn r#return(span: Span, value: Item) -> Self {
        Item::new(span, ItemKind::Return(ReturnItem::new(value)))
    }
}
