use crate::lower::*;
use serde::Serialize;

#[derive(Clone, Serialize)]
pub struct BlockItem {
    pub statements: Vec<SpannedItem>,
}

impl SpannedItem {
    pub fn block(span: Span, statements: Vec<SpannedItem>) -> Self {
        SpannedItem::new(span, Item::Block(BlockItem { statements }))
    }
}
