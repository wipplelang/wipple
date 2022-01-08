use crate::{typecheck::*, *};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VariableItem {
    pub variable: VariableId,
}

impl VariableItem {
    pub fn new(variable: VariableId) -> Self {
        VariableItem { variable }
    }
}

impl Item {
    pub fn variable(compile_info: compile::ItemInfo, ty: Scheme, variable: VariableId) -> Self {
        Item::new(
            compile_info,
            ty,
            ItemKind::Variable(VariableItem::new(variable)),
        )
    }
}
