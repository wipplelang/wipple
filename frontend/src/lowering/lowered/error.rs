use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct LoweredErrorExpr(());

impl LoweredErrorExpr {
    pub fn new() -> Self {
        LoweredErrorExpr(())
    }
}
