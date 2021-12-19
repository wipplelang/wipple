use crate::*;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnresolvedItem {
    pub expr: Expr,
}

impl UnresolvedItem {
    pub fn new(expr: Expr) -> Self {
        UnresolvedItem { expr }
    }
}

impl Item {
    pub fn unresolved(span: Span, expr: Expr) -> Self {
        Item::new(span, ItemKind::Unresolved(UnresolvedItem::new(expr)))
    }
}
