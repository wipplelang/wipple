mod builtin;
mod operator;
mod template;

pub use builtin::*;
pub use operator::*;
pub use template::*;

use crate::lower::*;
use serde::Serialize;
use wipple_diagnostics::*;

#[derive(Clone, Serialize)]
pub struct SpannedForm {
    pub info: DebugInfo,
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
        SpannedForm::with_info(DebugInfo::new(span), form)
    }

    pub fn with_info(info: DebugInfo, form: Form) -> Self {
        SpannedForm { info, form }
    }
}

impl From<SpannedItem> for SpannedForm {
    fn from(item: SpannedItem) -> Self {
        SpannedForm::with_info(item.info, Form::Item(item.item))
    }
}
