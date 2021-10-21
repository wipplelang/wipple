use crate::lower::*;
use serde::Serialize;

#[derive(Clone, Serialize)]
pub struct UnitItem;

impl SpannedItem {
    pub fn unit(span: Span) -> Self {
        SpannedItem::new(span, Item::Unit(UnitItem))
    }
}
