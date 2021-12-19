use crate::{compile::*, *};
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
    pub fn number(span: Span, value: Decimal) -> Self {
        Item::new(span, ItemKind::Number(NumberItem::new(value)))
    }
}
