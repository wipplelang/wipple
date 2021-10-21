use crate::lower::*;

pub struct TextExpr {
    pub span: Span,
    pub value: LocalIntern<String>,
}

impl TextExpr {
    pub fn new(span: Span, value: LocalIntern<String>) -> Self {
        TextExpr { span, value }
    }
}

impl Expr for TextExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn lower_to_form(self, _: &Stack, info: &mut Info) -> SpannedForm {
        let constant = info.add_constant(ConstantValue::Text(self.value));
        SpannedItem::constant(self.span, constant).into()
    }
}
