use crate::lowering::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct LoweredApplyExpr {
    pub function: Box<LoweredExpr>,
    pub value: Box<LoweredExpr>,
}

impl LoweredApplyExpr {
    pub fn new(function: LoweredExpr, value: LoweredExpr) -> Self {
        LoweredApplyExpr {
            function: Box::new(function),
            value: Box::new(value),
        }
    }
}
