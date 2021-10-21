use crate::lower::*;
use serde::Serialize;
use wipple_parser::Intern;

#[derive(Clone, Serialize)]
pub struct ExternalItem {
    pub namespace: Intern<String>,
    pub identifier: Intern<String>,
}

impl SpannedItem {
    pub fn external(span: Span, namespace: Intern<String>, identifier: Intern<String>) -> Self {
        SpannedItem::new(
            span,
            Item::External(ExternalItem {
                namespace,
                identifier,
            }),
        )
    }
}
