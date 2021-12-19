use crate::{typecheck::*, *};
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
        compile_info: compile::ItemInfo,
        ty: TypeSchema,
        namespace: InternedString,
        identifier: InternedString,
        inputs: Vec<Item>,
    ) -> Self {
        Item::new(
            compile_info,
            ty,
            ItemKind::External(ExternalItem::new(namespace, identifier, inputs)),
        )
    }
}
