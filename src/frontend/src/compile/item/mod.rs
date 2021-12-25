mod annotate;
mod apply;
mod block;
mod data;
mod data_decl;
mod end;
mod external;
mod field;
mod function;
mod function_input;
mod initialize;
mod r#loop;
mod number;
mod r#return;
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
pub use field::*;
pub use function::*;
pub use function_input::*;
pub use initialize::*;
pub use number::*;
pub use r#loop::*;
pub use r#return::*;
pub use text::*;
pub use unit::*;
pub use variable::*;

use crate::{typecheck::TypeSchema, *};
use serde::{Deserialize, Serialize};

#[non_exhaustive]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Item {
    pub id: ItemId,
    pub info: ItemInfo,
    pub kind: ItemKind,

    /// Unused in lowering, set during typechecking
    pub ty: Option<TypeSchema>,
}

impl Item {
    pub fn new(span: Span, kind: ItemKind) -> Self {
        Item {
            id: ItemId::new(),
            info: ItemInfo::new(span),
            kind,
            ty: None,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ItemKind {
    Annotate(AnnotateItem),
    Apply(ApplyItem),
    Block(BlockItem),
    Data(DataItem),
    DataDecl(DataDeclItem),
    End(EndItem),
    External(ExternalItem),
    Field(FieldItem),
    Function(FunctionItem),
    FunctionInput(FunctionInputItem),
    Initialize(InitializeItem),
    Loop(LoopItem),
    Number(NumberItem),
    Return(ReturnItem),
    Text(TextItem),
    Unit(UnitItem),
    Variable(VariableItem),
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
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
