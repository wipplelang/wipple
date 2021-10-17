use serde::Serialize;
use wipple_parser::{decimal::Decimal, Intern};

#[derive(Debug, Clone, Serialize)]
pub struct LoweredConstantExpr {
    pub kind: LoweredConstantExprKind,
}

#[derive(Debug, Clone, Serialize)]
pub enum LoweredConstantExprKind {
    Number(Intern<Decimal>),
    Text(Intern<String>),
}

impl LoweredConstantExpr {
    pub fn new(kind: LoweredConstantExprKind) -> Self {
        LoweredConstantExpr { kind }
    }
}
