use crate::lower::*;
use serde::Serialize;
use wipple_parser::{decimal::Decimal, LocalIntern};

#[derive(Clone, Serialize)]
pub struct ConstantItem {
    pub kind: ConstantValueExprKind,
}

#[derive(Clone, Serialize)]
pub enum ConstantValueExprKind {
    Number(LocalIntern<Decimal>),
    Text(LocalIntern<String>),
}

impl SpannedItem {
    pub fn constant(span: Span, kind: ConstantValueExprKind) -> Self {
        SpannedItem::new(span, Item::Constant(ConstantItem { kind }))
    }

    pub fn constant_number(span: Span, value: LocalIntern<Decimal>) -> Self {
        SpannedItem::constant(span, ConstantValueExprKind::Number(value))
    }

    pub fn constant_text(span: Span, value: LocalIntern<String>) -> Self {
        SpannedItem::constant(span, ConstantValueExprKind::Text(value))
    }
}
