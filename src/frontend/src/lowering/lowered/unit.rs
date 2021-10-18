use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct LoweredUnitExpr(());

impl LoweredUnitExpr {
    pub fn new() -> Self {
        LoweredUnitExpr(())
    }
}
