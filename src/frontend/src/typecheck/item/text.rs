use crate::{typecheck::*, *};
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
    pub fn text(compile_info: compile::ItemInfo, value: InternedString) -> Self {
        Item::new(
            compile_info,
            Scheme::Type(BUILTIN_TYPES.text.clone()),
            ItemKind::Text(TextItem::new(value)),
        )
    }
}
