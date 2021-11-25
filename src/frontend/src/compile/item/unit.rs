use crate::*;
use serde::Serialize;

#[derive(Debug, Clone, Default, Serialize)]
pub struct UnitItem;

impl UnitItem {
    pub fn new() -> Self {
        UnitItem
    }
}

impl Item {
    pub fn unit(span: Span) -> Self {
        Item::new(span, ItemKind::Unit(UnitItem::new()))
    }
}
