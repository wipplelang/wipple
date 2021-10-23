use std::collections::HashSet;

use crate::lower::*;
use serde::Serialize;

#[derive(Clone, Serialize)]
pub struct FunctionItem {
    pub input_span: Span,
    pub body: Box<Item>,
    pub captures: HashSet<VariableId>,
}

impl Item {
    pub fn function(span: Span, input_span: Span, body: Item, captures: HashSet<VariableId>) -> Self {
        Item::new(
            span,
            ItemKind::Function(FunctionItem {
                input_span,
                body: Box::new(body),
                captures,
            }),
        )
    }
}
