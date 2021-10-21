use crate::lower::*;
use serde::Serialize;

#[derive(Clone, Serialize)]
pub struct TemplateForm {
    // TODO
}

impl SpannedForm {
    pub fn template(span: Span) -> Self {
        SpannedForm::new(span, Form::Template(TemplateForm {}))
    }
}
