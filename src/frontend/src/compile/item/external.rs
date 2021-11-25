use crate::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct ExternalItem {
    pub namespace: InternedString,
    pub identifier: InternedString,
}

impl ExternalItem {
    pub fn new(namespace: InternedString, identifier: InternedString) -> Self {
        ExternalItem {
            namespace,
            identifier,
        }
    }
}

impl Item {
    pub fn external(span: Span, namespace: InternedString, identifier: InternedString) -> Self {
        Item::new(
            span,
            ItemKind::External(ExternalItem::new(namespace, identifier)),
        )
    }
}
