use crate::{compile::*, typecheck::Type};
use kind::kind;
use serde::Serialize;
use std::collections::HashSet;
use wipple_parser::decimal::Decimal;

#[non_exhaustive]
#[derive(Debug, Clone, Serialize)]
pub struct Item {
    pub id: ItemId,
    pub debug_info: ItemInfo,
    pub kind: ItemKind,
}

impl Item {
    pub fn new(span: Span, kind: ItemKind) -> Self {
        Item {
            id: ItemId::new(),
            debug_info: ItemInfo::new(span),
            kind,
        }
    }
}

#[kind(Item::new(span: Span))]
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
    Annotate {
        item: Box<Item>,
        ty: Type,
    },
}

#[derive(Debug, Clone, Serialize)]
pub struct ItemInfo {
    pub span: Span,
    pub declared_name: Option<LocalIntern<String>>,
}

impl ItemInfo {
    pub fn new(span: Span) -> Self {
        ItemInfo {
            span,
            declared_name: None,
        }
    }
}
