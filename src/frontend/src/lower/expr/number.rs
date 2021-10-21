use crate::lower::*;
use wipple_parser::decimal::Decimal;

pub struct NumberExpr {
    pub span: Span,
    pub value: LocalIntern<Decimal>,
}

impl NumberExpr {
    pub fn new(span: Span, value: LocalIntern<Decimal>) -> Self {
        NumberExpr { span, value }
    }
}

impl Expr for NumberExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn lower_to_form(self, _: &Stack, info: &mut Info) -> SpannedForm {
        let constant = info.add_constant(ConstantValue::Number(self.value));
        SpannedItem::constant(self.span, constant).into()
    }
}
