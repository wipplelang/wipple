use crate::lower::*;
use serde::Serialize;
use wipple_parser::{decimal::Decimal, Intern};

#[derive(Clone, Serialize)]
pub struct ConstantItem {
    pub kind: ConstantValueExprKind,
}

#[derive(Clone, Serialize)]
pub enum ConstantValueExprKind {
    Number(Intern<Decimal>),
    Text(Intern<String>),
}

impl SpannedItem {
    pub fn constant(span: Span, kind: ConstantValueExprKind) -> Self {
        SpannedItem::new(span, Item::Constant(ConstantItem { kind }))
    }

    pub fn constant_number(span: Span, value: Intern<Decimal>) -> Self {
        SpannedItem::constant(span, ConstantValueExprKind::Number(value))
    }

    pub fn constant_text(span: Span, value: Intern<String>) -> Self {
        SpannedItem::constant(span, ConstantValueExprKind::Text(value))
    }
}
