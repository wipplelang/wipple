use crate::{compile::*, *};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ErrorItem;

impl ErrorItem {
    pub fn new() -> Self {
        ErrorItem
    }
}

impl Item {
    pub fn error(span: Span) -> Self {
        Item::new(span, ItemKind::Error(ErrorItem::new()))
    }
}
