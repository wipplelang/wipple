use crate::{compile::*, *};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TextItem {
    pub value: InternedString,
}

impl TextItem {
    pub fn new(value: InternedString) -> Self {
        TextItem { value }
    }
}

impl Item {
    pub fn text(span: Span, value: InternedString) -> Self {
        Item::new(span, ItemKind::Text(TextItem::new(value)))
    }
}
