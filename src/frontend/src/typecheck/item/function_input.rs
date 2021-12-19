use crate::{typecheck::*, *};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct FunctionInputItem;

impl FunctionInputItem {
    pub fn new() -> Self {
        FunctionInputItem
    }
}

impl Item {
    pub fn function_input(compile_info: compile::ItemInfo, ty: TypeSchema) -> Self {
        Item::new(
            compile_info,
            ty,
            ItemKind::FunctionInput(FunctionInputItem::new()),
        )
    }
}
