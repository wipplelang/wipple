use crate::lower::*;
use serde::Serialize;

#[derive(Clone, Serialize)]
pub struct TemplateForm {
    pub span: Span,
    // TODO
}

impl Form {
    pub fn template(span: Span) -> Self {
        Form::Template(TemplateForm {
            span,
            // TODO
        })
    }
}
