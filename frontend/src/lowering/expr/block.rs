use crate::{diagnostics::Diagnostics, lowering::*, parser::Span};

#[derive(Debug)]
pub struct BlockExpr {
    pub span: Span,
    pub statements: Vec<AnyExpr>,
}

impl BlockExpr {
    pub fn new(span: Span, statements: Vec<AnyExpr>) -> Self {
        BlockExpr { span, statements }
    }
}

impl Expr for BlockExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn lower(self, scope: &mut Scope, diagnostics: &mut Diagnostics) -> LoweredExpr {
        let mut scope = scope.child();

        let statement_exprs = self
            .statements
            .into_iter()
            .map(|statement| statement.lower(&mut scope, diagnostics))
            .collect();

        let block_expr = LoweredBlockExpr::new(statement_exprs);

        LoweredExpr::new(self.span, LoweredExprKind::Block(block_expr))
    }
}
