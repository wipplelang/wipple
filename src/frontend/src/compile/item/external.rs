use crate::{compile::*, *};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExternalItem {
    pub namespace: InternedString,
    pub identifier: InternedString,
    pub inputs: Vec<Item>,
}

impl ExternalItem {
    pub fn new(namespace: InternedString, identifier: InternedString, inputs: Vec<Item>) -> Self {
        ExternalItem {
            namespace,
            identifier,
            inputs,
        }
    }
}

impl Item {
    pub fn external(
        span: Span,
        namespace: InternedString,
        identifier: InternedString,
        inputs: Vec<Item>,
    ) -> Self {
        Item::new(
            span,
            ItemKind::External(ExternalItem::new(namespace, identifier, inputs)),
        )
    }
}
