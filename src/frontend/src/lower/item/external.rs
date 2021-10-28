use crate::lower::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct ExternalItem {
    pub namespace: LocalIntern<String>,
    pub identifier: LocalIntern<String>,
    pub inputs: Vec<Item>,
}

impl Item {
    pub fn external(
        span: Span,
        namespace: LocalIntern<String>,
        identifier: LocalIntern<String>,
        inputs: Vec<Item>,
    ) -> Self {
        Item::new(
            span,
            ItemKind::External(ExternalItem {
                namespace,
                identifier,
                inputs,
            }),
        )
    }
}
