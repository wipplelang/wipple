use crate::{debug_info::DebugInfo, lower::*, typecheck::Ty};
use kind::kind;
use serde::Serialize;
use std::collections::HashSet;
use wipple_parser::decimal::Decimal;

#[non_exhaustive]
#[derive(Debug, Clone, Serialize)]
pub struct Item {
    pub debug_info: DebugInfo,
    pub kind: ItemKind,
}

impl Item {
    pub fn new(span: Span, kind: ItemKind) -> Self {
        Item {
            debug_info: DebugInfo {
                span,
                declared_name: None,
            },
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
    Annotate {
        item: Box<Item>,
        ty: Ty,
    },
}
