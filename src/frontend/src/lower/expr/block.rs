use crate::lower::*;

#[derive(Debug)]
pub struct BlockExpr {
    pub span: Span,
    pub statements: Vec<SpannedExpr>,
}

impl BlockExpr {
    pub fn new(span: Span, statements: Vec<SpannedExpr>) -> Self {
        BlockExpr { span, statements }
    }
}

impl Expr for BlockExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn lower_to_form(self, stack: Stack, info: &mut Info) -> SpannedForm {
        let variables = Default::default();
        let stack = stack.child(Scope::Block {
            variables: &variables,
        });

        let statements = self
            .statements
            .into_iter()
            .map(|statement| statement.lower_to_item(stack, info))
            .collect();

        SpannedItem::block(self.span, statements).into()
    }

    fn lower_to_binding(self, _: Stack, info: &mut Info) -> SpannedBinding {
        info.diagnostics.add(Diagnostic::new(
            DiagnosticLevel::Error,
            "Cannot assign to a block",
            vec![Note::primary(
                self.span,
                "Expected a variable name or other assignable expression",
            )],
        ));

        SpannedBinding::error(self.span)
    }
}
