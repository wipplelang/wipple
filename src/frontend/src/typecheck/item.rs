use super::Ty;
use crate::{debug_info::DebugInfo, id::*};
use kind::kind;
use serde::Serialize;
use std::collections::HashSet;
use wipple_parser::{decimal::Decimal, LocalIntern};

#[non_exhaustive]
#[derive(Debug, Clone, Serialize)]
pub struct Item {
    pub debug_info: DebugInfo,
    pub ty: Ty,
    pub kind: ItemKind,
}

impl Item {
    pub fn new(debug_info: DebugInfo, ty: Ty, kind: ItemKind) -> Self {
        Item {
            debug_info,
            ty,
            kind,
        }
    }
}

#[kind(Item::new(debug_info: DebugInfo, ty: Ty))]
#[derive(Debug, Clone, Serialize)]
pub enum ItemKind {
    Unit,
    Number {
        value: LocalIntern<Decimal>,
    },
    Text {
        value: LocalIntern<String>,
    },
    Block {
        statements: Vec<Item>,
    },
    Apply {
        function: Box<Item>,
        input: Box<Item>,
    },
    Initialize {
        variable: VariableId,
        value: Box<Item>,
    },
    Variable {
        variable: VariableId,
    },
    Function {
        input_debug_info: DebugInfo,
        body: Box<Item>,
        captures: HashSet<VariableId>,
    },
    FunctionInput,
    External {
        namespace: LocalIntern<String>,
        identifier: LocalIntern<String>,
    },
}
