mod builtin;
mod operator;
mod template;

pub use builtin::*;
pub use operator::*;
pub use template::*;

use crate::lower::*;
use serde::Serialize;
use wipple_diagnostics::Span;

#[derive(Clone, Serialize)]
pub struct SpannedForm {
    pub span: Span,
    pub form: Form,
}

#[derive(Clone, Serialize)]
pub enum Form {
    Item(Item),
    Operator(OperatorForm),
    Template(TemplateForm),
    // eventually, types, etc.
}

impl SpannedForm {
    pub fn new(span: Span, form: Form) -> Self {
        SpannedForm { span, form }
    }
}

impl From<SpannedItem> for SpannedForm {
    fn from(item: SpannedItem) -> Self {
        SpannedForm::new(item.span, Form::Item(item.item))
    }
}
