use crate::compile::*;

#[derive(Debug)]
pub struct TextExpr {
    pub span: Span,
    pub value: InternedString,
}

impl TextExpr {
    pub fn new(span: Span, value: InternedString) -> Self {
        TextExpr { span, value }
    }
}

impl ExprKind for TextExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn lower(self, _: LowerContext, _: &Stack, _: &mut Info) -> Option<Form> {
        Some(Form::item(self.span, Item::text(self.span, self.value)))
    }
}
