use crate::lower::*;
use serde::Serialize;

#[derive(Clone, Serialize)]
pub struct ConstantItem {
    pub constant: ConstantId,
}

impl SpannedItem {
    pub fn constant(span: Span, constant: ConstantId) -> Self {
        SpannedItem::new(span, Item::Constant(ConstantItem { constant }))
    }
}
