use crate::lowering::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct LoweredDeclareExpr {
    pub variable: Variable,
}

impl LoweredDeclareExpr {
    pub fn new(variable: Variable) -> Self {
        LoweredDeclareExpr { variable }
    }
}
