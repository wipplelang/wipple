use crate::lower::*;
use wipple_parser::Intern;

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

    fn lower_to_form(self, _: Stack, _: &mut Info) -> SpannedForm {
        SpannedItem::constant_text(self.span, self.value).into()
    }
}
