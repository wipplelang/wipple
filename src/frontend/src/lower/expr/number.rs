use crate::lower::*;
use wipple_parser::decimal::Decimal;

#[derive(Debug)]
pub struct NumberExpr {
    pub span: Span,
    pub value: LocalIntern<Decimal>,
}

impl NumberExpr {
    pub fn new(span: Span, value: LocalIntern<Decimal>) -> Self {
        NumberExpr { span, value }
    }
}

impl ExprKind for NumberExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn lower_to_form(self, _: &Stack, _: &mut Info) -> Form {
        Form::item(self.span, Item::number(self.span, self.value))
    }
}
