use crate::{typecheck::*, *};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ErrorItem;

impl ErrorItem {
    pub fn new() -> Self {
        ErrorItem
    }
}

impl Item {
    pub fn error(compile_info: compile::ItemInfo) -> Self {
        Item::new(
            compile_info,
            TypeSchema::Monotype(BUILTIN_TYPES.never.clone()),
            ItemKind::Error(ErrorItem::new()),
        )
    }
}
