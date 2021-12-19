use crate::{compile::*, *};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
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
