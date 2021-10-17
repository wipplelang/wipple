use crate::lowering::*;
use internment::Intern;
use serde::Serialize;
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize)]
pub struct LoweredDataBlockExpr {
    pub variables: HashMap<Intern<String>, LoweredExpr>,
}

impl LoweredDataBlockExpr {
    pub fn new(variables: HashMap<Intern<String>, LoweredExpr>) -> Self {
        LoweredDataBlockExpr { variables }
    }
}
