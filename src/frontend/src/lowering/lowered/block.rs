use crate::lowering::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct LoweredBlockExpr {
    pub statements: Vec<LoweredExpr>,
}

impl LoweredBlockExpr {
    pub fn new(statements: Vec<LoweredExpr>) -> Self {
        LoweredBlockExpr { statements }
    }
}
