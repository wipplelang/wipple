use crate::lower::*;
use serde::Serialize;

#[derive(Clone, Serialize)]
pub struct ExternalItem {
    pub namespace: LocalIntern<String>,
    pub identifier: LocalIntern<String>,
}

impl SpannedItem {
    pub fn external(
        span: Span,
        namespace: LocalIntern<String>,
        identifier: LocalIntern<String>,
    ) -> Self {
        SpannedItem::new(
            span,
            Item::External(ExternalItem {
                namespace,
                identifier,
            }),
        )
    }
}
