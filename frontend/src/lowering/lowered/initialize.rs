use crate::lowering::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct LoweredInitializeExpr {
    pub variable: Variable,
    pub value: Box<LoweredExpr>,
}

impl LoweredInitializeExpr {
    pub fn new(variable: Variable, value: LoweredExpr) -> Self {
        LoweredInitializeExpr {
            variable,
            value: Box::new(value),
        }
    }
}
