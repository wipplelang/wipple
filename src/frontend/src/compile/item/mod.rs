mod annotate;
mod apply;
mod block;
mod external;
mod function;
mod function_input;
mod initialize;
mod number;
mod text;
mod unit;
mod variable;

pub use annotate::*;
pub use apply::*;
pub use block::*;
pub use external::*;
pub use function::*;
pub use function_input::*;
pub use initialize::*;
pub use number::*;
pub use text::*;
pub use unit::*;
pub use variable::*;

use crate::*;
use serde::Serialize;

#[non_exhaustive]
#[derive(Debug, Clone, Serialize)]
pub struct Item {
    pub id: ItemId,
    pub info: ItemInfo,
    pub kind: ItemKind,
}

impl Item {
    pub fn new(span: Span, kind: ItemKind) -> Self {
        Item {
            id: ItemId::new(),
            info: ItemInfo::new(span),
            kind,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum ItemKind {
    Annotate(AnnotateItem),
    Apply(ApplyItem),
    Block(BlockItem),
    External(ExternalItem),
    Function(FunctionItem),
    FunctionInput(FunctionInputItem),
    Initialize(InitializeItem),
    Number(NumberItem),
    Text(TextItem),
    Unit(UnitItem),
    Variable(VariableItem),
}

#[derive(Debug, Clone, Serialize)]
pub struct ItemInfo {
    pub span: Span,
    pub declared_name: Option<InternedString>,
}

impl ItemInfo {
    pub fn new(span: Span) -> Self {
        ItemInfo {
            span,
            declared_name: None,
        }
    }
}
