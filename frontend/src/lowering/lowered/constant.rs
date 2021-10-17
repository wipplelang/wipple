use internment::Intern;
use rust_decimal::Decimal;
use serde::Serialize;

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
