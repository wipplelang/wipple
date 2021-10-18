use crate::lowering::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct LoweredRuntimeVariableExpr {
    pub variable: Variable,
}

impl LoweredRuntimeVariableExpr {
    pub fn new(variable: Variable) -> Self {
        LoweredRuntimeVariableExpr { variable }
    }
}
