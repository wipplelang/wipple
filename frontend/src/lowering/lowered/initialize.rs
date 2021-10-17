use crate::lowering::*;
use internment::Intern;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct LoweredInitializeExpr {
    pub name: Intern<String>,
    pub value: Box<LoweredExpr>,
}

impl LoweredInitializeExpr {
    pub fn new(name: Intern<String>, value: LoweredExpr) -> Self {
        LoweredInitializeExpr {
            name,
            value: Box::new(value),
        }
    }
}
