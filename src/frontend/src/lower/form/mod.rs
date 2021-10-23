mod builtin;
mod operator;
mod template;

pub use builtin::*;
pub use operator::*;
pub use template::*;

use crate::lower::*;
use serde::Serialize;

#[derive(Clone, Serialize)]
pub enum Form {
    Item(Item),
    Operator(OperatorForm),
    Template(TemplateForm),
    // eventually, types, etc.
}
