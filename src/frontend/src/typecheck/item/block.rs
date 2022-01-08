use crate::{typecheck::*, *};
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
    pub fn block(
        compile_info: compile::ItemInfo,
        ty: Scheme,
        statements: Vec<Item>,
    ) -> Self {
        Item::new(
            compile_info,
            ty,
            ItemKind::Block(BlockItem::new(statements)),
        )
    }
}
