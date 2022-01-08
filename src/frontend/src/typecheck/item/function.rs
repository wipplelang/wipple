use crate::{typecheck::*, *};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

#[derive(Debug, Clone, Serialize, Deserialize)]
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
    pub fn function(
        compile_info: compile::ItemInfo,
        ty: Scheme,
        body: Item,
        captures: HashSet<VariableId>,
    ) -> Self {
        Item::new(
            compile_info,
            ty,
            ItemKind::Function(FunctionItem::new(body, captures)),
        )
    }
}
