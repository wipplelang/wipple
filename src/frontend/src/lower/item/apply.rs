use crate::lower::*;
use serde::Serialize;

#[derive(Clone, Serialize)]
pub struct ApplyItem {
    pub function: Box<Item>,
    pub value: Box<Item>,
}

impl Item {
    pub fn apply(span: Span, function: Item, value: Item) -> Self {
        Item::new(
            span,
            ItemKind::Apply(ApplyItem {
                function: Box::new(function),
                value: Box::new(value),
            }),
        )
    }
}
