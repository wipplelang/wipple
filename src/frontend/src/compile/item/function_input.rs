use crate::*;
use serde::Serialize;

#[derive(Debug, Clone, Default, Serialize)]
pub struct FunctionInputItem;

impl FunctionInputItem {
    pub fn new() -> Self {
        FunctionInputItem
    }
}

impl Item {
    pub fn function_input(span: Span) -> Self {
        Item::new(span, ItemKind::FunctionInput(FunctionInputItem::new()))
    }
}
