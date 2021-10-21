use crate::lower::*;

use wipple_parser::{decimal::Decimal, Intern};

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

    fn lower_to_item(self, _: Stack, _: &mut Info) -> SpannedItem {
        SpannedItem::constant_number(self.span, self.value)
    }

    fn lower_to_binding(self, _: Stack, info: &mut Info) -> SpannedBinding {
        info.diagnostics.add(Diagnostic::new(
            DiagnosticLevel::Error,
            "Cannot assign to a number",
            vec![Note::primary(
                self.span,
                "Expected a variable name or other assignable expression",
            )],
        ));

        SpannedBinding::error(self.span)
    }
}
