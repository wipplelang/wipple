use crate::lower::*;
use serde::Serialize;

#[derive(Clone, Copy, Serialize)]
pub struct UnitItem;

impl Item {
    pub fn unit(span: Span) -> Self {
        Item::new(span, ItemKind::Unit(UnitItem))
    }
}
