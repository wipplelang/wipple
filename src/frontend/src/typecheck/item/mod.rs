mod apply;
mod block;
mod data;
mod end;
mod error;
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

pub use apply::*;
pub use block::*;
pub use data::*;
pub use end::*;
pub use error::*;
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

use crate::{typecheck::*, *};
use serde::{Deserialize, Serialize};

#[non_exhaustive]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Item {
    pub id: ItemId,
    pub compile_info: compile::ItemInfo,
    pub ty: TypeSchema,
    pub kind: ItemKind,
}

impl Item {
    pub fn new(compile_info: compile::ItemInfo, ty: TypeSchema, kind: ItemKind) -> Self {
        Item {
            id: ItemId::new(),
            compile_info,
            ty,
            kind,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ItemKind {
    Apply(ApplyItem),
    Block(BlockItem),
    Data(DataItem),
    End(EndItem),
    Error(ErrorItem),
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
    pub fn traverse(&mut self, mut f: impl FnMut(&mut compile::ItemInfo, &mut TypeSchema)) {
        pub fn traverse(
            item: &mut Item,
            f: &mut impl FnMut(&mut compile::ItemInfo, &mut TypeSchema),
        ) {
            f(&mut item.compile_info, &mut item.ty);

            match &mut item.kind {
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
                ItemKind::End(end) => {
                    traverse(&mut end.value, f);
                }
                ItemKind::Error(_) => {}
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
                    f(&mut initialize.binding_info, &mut initialize.value.ty);
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
