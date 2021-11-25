use crate::*;
use serde::Serialize;
use std::collections::HashSet;

#[derive(Debug, Clone, Serialize)]
pub struct FunctionItem {
    pub body: Box<Item>,
    pub captures: HashSet<VariableId>,
}

impl FunctionItem {
    pub fn new(body: Item, captures: HashSet<VariableId>) -> Self {
        FunctionItem {
            body: Box::new(body),
            captures,
        }
    }
}

impl Item {
    pub fn function(span: Span, body: Item, captures: HashSet<VariableId>) -> Self {
        Item::new(span, ItemKind::Function(FunctionItem::new(body, captures)))
    }
}
