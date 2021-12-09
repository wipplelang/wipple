mod annotate;
mod apply;
mod block;
mod data;
mod data_decl;
mod end;
mod external;
mod function;
mod function_input;
mod get;
mod initialize;
mod r#loop;
mod mutable;
mod number;
mod set;
mod text;
mod unit;
mod variable;

pub use annotate::*;
pub use apply::*;
pub use block::*;
pub use data::*;
pub use data_decl::*;
pub use end::*;
pub use external::*;
pub use function::*;
pub use function_input::*;
pub use get::*;
pub use initialize::*;
pub use mutable::*;
pub use number::*;
pub use r#loop::*;
pub use set::*;
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
    Data(DataItem),
    DataDecl(DataDeclItem),
    End(EndItem),
    External(ExternalItem),
    Function(FunctionItem),
    FunctionInput(FunctionInputItem),
    Get(GetItem),
    Initialize(InitializeItem),
    Loop(LoopItem),
    Mutable(MutableItem),
    Number(NumberItem),
    Set(SetItem),
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
