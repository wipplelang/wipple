use crate::lower::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct ApplyItem {
    pub function: Box<Item>,
    pub input: Box<Item>,
}

impl Item {
    pub fn apply(span: Span, function: Item, input: Item) -> Self {
        Item::new(
            span,
            ItemKind::Apply(ApplyItem {
                function: Box::new(function),
                input: Box::new(input),
            }),
        )
    }
}
