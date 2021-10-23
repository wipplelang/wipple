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

impl ExprKind for TextExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn lower_to_form(self, _: &Stack, _: &mut Info) -> Form {
        Form::Item(Item::constant(
            self.span,
            ConstantItemKind::Text(self.value),
        ))
    }
}
