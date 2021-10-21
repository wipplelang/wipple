use crate::lower::*;
use serde::Serialize;

#[derive(Clone, Serialize)]
pub struct FunctionItem {
    pub function: FunctionId,
}

impl SpannedItem {
    pub fn function(span: Span, function: FunctionId) -> Self {
        SpannedItem::new(span, Item::Function(FunctionItem { function }))
    }
}
