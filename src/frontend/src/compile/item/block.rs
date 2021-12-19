use crate::{compile::*, *};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlockItem {
    pub statements: Vec<Item>,
}

impl BlockItem {
    pub fn new(statements: Vec<Item>) -> Self {
        BlockItem { statements }
    }
}

impl Item {
    pub fn block(span: Span, statements: Vec<Item>) -> Self {
        Item::new(span, ItemKind::Block(BlockItem::new(statements)))
    }
}
