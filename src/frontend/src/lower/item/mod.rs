mod apply;
mod block;
mod constant;
mod external;
mod initialize;
mod unit;
mod variable;

pub use apply::*;
pub use block::*;
pub use constant::*;
pub use external::*;
pub use initialize::*;
pub use unit::*;
pub use variable::*;

use serde::Serialize;
use wipple_diagnostics::Span;

#[derive(Clone, Serialize)]
pub struct SpannedItem {
    pub span: Span,
    pub item: Item,
}

#[derive(Clone, Serialize)]
pub enum Item {
    Unit(UnitItem),
    Constant(ConstantItem),
    Initialize(InitializeItem),
    Block(BlockItem),
    Apply(ApplyItem),
    Variable(VariableItem),
    External(ExternalItem),
    Error,
}

impl SpannedItem {
    pub fn new(span: Span, item: Item) -> Self {
        SpannedItem { span, item }
    }

    pub fn error(span: Span) -> Self {
        SpannedItem::new(span, Item::Error)
    }
}
