use crate::{compile::*, *};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnnotateItem {
    pub item: Box<Item>,
    pub constructor: Constructor,
}

impl AnnotateItem {
    pub fn new(item: Item, constructor: Constructor) -> Self {
        AnnotateItem {
            item: Box::new(item),
            constructor,
        }
    }
}

impl Item {
    pub fn annotate(span: Span, item: Item, constructor: Constructor) -> Self {
        Item::new(
            span,
            ItemKind::Annotate(AnnotateItem::new(item, constructor)),
        )
    }
}
