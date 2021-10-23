use crate::lower::*;
use serde::Serialize;

#[derive(Clone, Serialize)]
pub struct BlockItem {
    pub statements: Vec<Item>,
}

impl Item {
    pub fn block(span: Span, statements: Vec<Item>) -> Self {
        Item::new(span, ItemKind::Block(BlockItem { statements }))
    }
}
