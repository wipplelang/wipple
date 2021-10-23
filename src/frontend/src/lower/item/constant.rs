use crate::lower::*;
use serde::Serialize;
use wipple_parser::decimal::Decimal;

#[derive(Clone, Copy, Serialize)]
pub struct ConstantItem {
    pub kind: ConstantItemKind,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum ConstantItemKind {
    Number(LocalIntern<Decimal>),
    Text(LocalIntern<String>),
}

impl Item {
    pub fn constant(span: Span, kind: ConstantItemKind) -> Self {
        Item::new(span, ItemKind::Constant(ConstantItem { kind }))
    }
}
