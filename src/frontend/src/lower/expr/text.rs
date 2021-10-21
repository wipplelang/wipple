use crate::lower::*;

use wipple_parser::Intern;

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

    fn lower_to_item(self, _: Stack, _: &mut Info) -> SpannedItem {
        SpannedItem::constant_text(self.span, self.value)
    }

    fn lower_to_binding(self, _: Stack, info: &mut Info) -> SpannedBinding {
        info.diagnostics.add(Diagnostic::new(
            DiagnosticLevel::Error,
            "Cannot assign to text",
            vec![Note::primary(
                self.span,
                "Expected a variable name or other assignable expression",
            )],
        ));

        SpannedBinding::error(self.span)
    }
}
