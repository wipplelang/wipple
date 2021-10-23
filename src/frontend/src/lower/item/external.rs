use crate::lower::*;
use serde::Serialize;

#[derive(Clone, Serialize)]
pub struct ExternalItem {
    pub namespace: LocalIntern<String>,
    pub identifier: LocalIntern<String>,
}

impl Item {
    pub fn external(
        span: Span,
        namespace: LocalIntern<String>,
        identifier: LocalIntern<String>,
    ) -> Self {
        Item::new(
            span,
            ItemKind::External(ExternalItem {
                namespace,
                identifier,
            }),
        )
    }
}
