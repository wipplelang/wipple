use crate::{typecheck::*, *};
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NumberItem {
    pub value: Decimal,
}

impl NumberItem {
    pub fn new(value: Decimal) -> Self {
        NumberItem { value }
    }
}

impl Item {
    pub fn number(compile_info: compile::ItemInfo, value: Decimal) -> Self {
        Item::new(
            compile_info,
            TypeSchema::Monotype(BUILTIN_TYPES.number.clone()),
            ItemKind::Number(NumberItem::new(value)),
        )
    }
}
