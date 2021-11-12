use super::Ty;
use crate::{id::*, lower};
use kind::kind;
use serde::Serialize;
use std::{collections::HashSet, fmt};
use wipple_parser::{decimal::Decimal, LocalIntern};

#[non_exhaustive]
#[derive(Debug, Clone, Serialize)]
pub struct Item {
    pub info: ItemInfo,
    pub kind: ItemKind,
}

impl Item {
    pub fn new(info: ItemInfo, kind: ItemKind) -> Self {
        Item { info, kind }
    }
}

#[kind(Item::new(debug_info: ItemInfo))]
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
        binding_info: ItemInfo,
        variable: VariableId,
        value: Box<Item>,
    },
    Variable {
        variable: VariableId,
    },
    Function {
        body: Box<Item>,
        captures: HashSet<VariableId>,
    },
    FunctionInput,
    External {
        namespace: LocalIntern<String>,
        identifier: LocalIntern<String>,
    },
}

#[derive(Debug, Clone, Serialize)]
pub struct ItemInfo {
    pub info: lower::ItemInfo,
    pub ty: Ty,
}

impl ItemInfo {
    pub fn new(info: lower::ItemInfo, ty: Ty) -> Self {
        ItemInfo { info, ty }
    }
}

impl fmt::Display for ItemInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "```wipple\n{}\n```",
            if let Some(declared_name) = self.info.declared_name {
                format!("{} :: {}", declared_name, self.ty)
            } else {
                self.ty.to_string()
            }
        )
    }
}
