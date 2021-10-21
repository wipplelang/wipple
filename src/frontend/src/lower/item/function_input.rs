use crate::lower::*;
use serde::Serialize;

#[derive(Clone, Serialize)]
pub struct FunctionInputItem;

impl SpannedItem {
    pub fn function_input(span: Span) -> Self {
        SpannedItem::new(span, Item::FunctionInput(FunctionInputItem))
    }
}
