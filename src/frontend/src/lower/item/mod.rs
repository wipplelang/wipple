mod apply;
mod block;
mod constant;
mod external;
mod function;
mod function_input;
mod initialize;
mod unit;
mod variable;

pub use apply::*;
pub use block::*;
pub use constant::*;
pub use external::*;
pub use function::*;
pub use function_input::*;
pub use initialize::*;
pub use unit::*;
pub use variable::*;

use crate::{lower::*, typecheck::Type};
use serde::Serialize;

#[derive(Clone, Serialize)]
pub struct Item {
    pub span: Span,
    pub declared_name: Option<LocalIntern<String>>,
    pub ty: Type,
    pub kind: ItemKind,
}

#[derive(Clone, Serialize)]
pub enum ItemKind {
    Error,
    Unit(UnitItem),
    Constant(ConstantItem),
    Block(BlockItem),
    Apply(ApplyItem),
    Initialize(InitializeItem),
    Variable(VariableItem),
    Function(FunctionItem),
    FunctionInput(FunctionInputItem),
    External(ExternalItem),
}

impl Item {
    pub fn new(span: Span, kind: ItemKind) -> Self {
        Item {
            span,
            declared_name: None,
            ty: Type::variable(span),
            kind,
        }
    }

    pub fn error(span: Span) -> Self {
        Item::new(span, ItemKind::Error)
    }
}
