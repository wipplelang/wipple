use crate::{diagnostics::Diagnostics, lowering::*, parser::Span};
use internment::Intern;

#[derive(Debug)]
pub struct TextExpr {
    pub span: Span,
    pub value: Intern<String>,
}

impl TextExpr {
    pub fn new(span: Span, value: Intern<String>) -> Self {
        TextExpr { span, value }
    }
}

impl Expr for TextExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn lower(self, _: &mut Scope, _: &mut Diagnostics) -> LoweredExpr {
        let constant_expr = LoweredConstantExpr::new(LoweredConstantExprKind::Text(self.value));
        LoweredExpr::new(self.span, LoweredExprKind::Constant(constant_expr))
    }
}
