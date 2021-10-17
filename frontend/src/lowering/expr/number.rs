use crate::{diagnostics::Diagnostics, lowering::*, parser::Span};
use internment::Intern;
use rust_decimal::Decimal;

#[derive(Debug)]
pub struct NumberExpr {
    pub span: Span,
    pub value: Intern<Decimal>,
}

impl NumberExpr {
    pub fn new(span: Span, value: Intern<Decimal>) -> Self {
        NumberExpr { span, value }
    }
}

impl Expr for NumberExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn lower(self, _: &mut Scope, _: &mut Diagnostics) -> LoweredExpr {
        let constant_expr = LoweredConstantExpr::new(LoweredConstantExprKind::Number(self.value));
        LoweredExpr::new(self.span, LoweredExprKind::Constant(constant_expr))
    }
}
