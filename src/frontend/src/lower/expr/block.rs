use crate::lower::*;

#[derive(Debug)]
pub struct BlockExpr {
    pub span: Span,
    pub statements: Vec<Expr>,
}

impl BlockExpr {
    pub fn new(span: Span, statements: Vec<Expr>) -> Self {
        BlockExpr { span, statements }
    }
}

impl ExprKind for BlockExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn lower(self, _: LowerContext, stack: &Stack, info: &mut Info) -> Option<Form> {
        let stack = stack.child_block();

        let statements = self
            .statements
            .into_iter()
            .map(|statement| statement.lower_to_item(&stack, info))
            .collect::<Option<_>>()?;

        Some(Form::item(self.span, Item::block(self.span, statements)))
    }
}
