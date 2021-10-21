use crate::lower::*;
use serde::Serialize;

#[derive(Clone, Serialize)]
pub struct InitializeItem {
    pub variable: VariableId,
    pub value: Box<SpannedItem>,
}

impl InitializeItem {
    pub fn new(variable: VariableId, value: SpannedItem) -> Self {
        InitializeItem {
            variable,
            value: Box::new(value),
        }
    }
}
