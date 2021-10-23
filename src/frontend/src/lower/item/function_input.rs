use crate::lower::*;
use serde::Serialize;

#[derive(Clone, Serialize)]
pub struct FunctionInputItem;

impl Item {
    pub fn function_input(span: Span) -> Self {
        Item::new(span, ItemKind::FunctionInput(FunctionInputItem))
    }
}
