use crate::{parse::Span, helpers::InternedString};

#[derive(Debug, Clone)]
pub struct TextPattern {
    pub span: Span,
    pub value: InternedString,
}
