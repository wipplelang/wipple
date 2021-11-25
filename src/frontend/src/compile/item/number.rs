use crate::*;
use rust_decimal::Decimal;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
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
