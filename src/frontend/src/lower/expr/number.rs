use crate::lower::*;
use wipple_parser::{decimal::Decimal, Intern};

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

    fn lower_to_form(self, _: Stack, _: &mut Info) -> SpannedForm {
        SpannedItem::constant_number(self.span, self.value).into()
    }
}
