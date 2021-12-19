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

impl Item {
    pub fn traverse(&mut self, mut f: impl FnMut(&mut Item)) {
        pub fn traverse(item: &mut Item, f: &mut impl FnMut(&mut Item)) {
            f(item);

            match &mut item.kind {
                ItemKind::Annotate(annotate) => {
                    traverse(&mut annotate.item, f);
                }
                ItemKind::Apply(apply) => {
                    traverse(&mut apply.function, f);
                    traverse(&mut apply.input, f);
                }
                ItemKind::Block(block) => {
                    for statement in &mut block.statements {
                        traverse(statement, f);
                    }
                }
                ItemKind::Data(data) => {
                    for field in &mut data.fields {
                        traverse(field, f);
                    }
                }
                ItemKind::DataDecl(_) => {}
                ItemKind::End(end) => {
                    traverse(&mut end.value, f);
                }
                ItemKind::External(external) => {
                    for input in &mut external.inputs {
                        traverse(input, f);
                    }
                }
                ItemKind::Field(field) => {
                    traverse(&mut field.value, f);
                }
                ItemKind::Function(function) => {
                    traverse(&mut function.body, f);
                }
                ItemKind::FunctionInput(_) => {}
                ItemKind::Initialize(initialize) => {
                    traverse(&mut initialize.value, f);
                }
                ItemKind::Loop(r#loop) => {
                    traverse(&mut r#loop.body, f);
                }
                ItemKind::Number(_) => {}
                ItemKind::Return(r#return) => {
                    traverse(&mut r#return.value, f);
                }
                ItemKind::Text(_) => {}
                ItemKind::Unit(_) => {}
                ItemKind::Variable(_) => {}
            }
        }

        traverse(self, &mut f)
    }
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
