use crate::lower::*;
use serde::Serialize;

#[derive(Clone, Serialize)]
pub struct ApplyItem {
    pub function: Box<SpannedItem>,
    pub value: Box<SpannedItem>,
}

impl SpannedItem {
    pub fn apply(span: Span, function: SpannedItem, value: SpannedItem) -> Self {
        SpannedItem::new(
            span,
            Item::Apply(ApplyItem {
                function: Box::new(function),
                value: Box::new(value),
            }),
        )
    }
}
