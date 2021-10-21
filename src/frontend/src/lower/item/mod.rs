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

use crate::lower::*;
use serde::Serialize;
use wipple_diagnostics::*;

#[derive(Clone, Serialize)]
pub struct SpannedItem {
    pub info: DebugInfo,
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
    Function(FunctionItem),
    FunctionInput(FunctionInputItem),
    External(ExternalItem),
    Error,
}

impl SpannedItem {
    pub fn new(span: Span, item: Item) -> Self {
        SpannedItem::with_info(DebugInfo::new(span), item)
    }

    pub fn with_info(info: DebugInfo, item: Item) -> Self {
        SpannedItem { info, item }
    }

    pub fn error(span: Span) -> Self {
        SpannedItem::new(span, Item::Error)
    }

    pub fn error_with_info(info: DebugInfo) -> Self {
        SpannedItem::with_info(info, Item::Error)
    }
}
