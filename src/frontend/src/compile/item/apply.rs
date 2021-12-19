use crate::{compile::*, *};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApplyItem {
    pub function: Box<Item>,
    pub input: Box<Item>,
}

impl ApplyItem {
    pub fn new(function: Item, input: Item) -> Self {
        ApplyItem {
            function: Box::new(function),
            input: Box::new(input),
        }
    }
}

impl Item {
    pub fn apply(span: Span, function: Item, input: Item) -> Self {
        Item::new(span, ItemKind::Apply(ApplyItem::new(function, input)))
    }
}
