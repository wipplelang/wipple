use crate::{compile::*, *};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldItem {
    pub value: Box<Item>,
    pub name_span: Span,
    pub name: InternedString,
}

impl FieldItem {
    pub fn new(value: Item, name_span: Span, name: InternedString) -> Self {
        FieldItem {
            value: Box::new(value),
            name_span,
            name,
        }
    }
}

impl Item {
    pub fn field(span: Span, value: Item, name_span: Span, name: InternedString) -> Self {
        Item::new(
            span,
            ItemKind::Field(FieldItem::new(value, name_span, name)),
        )
    }
}
