use crate::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct AnnotateItem {
    pub item: Box<Item>,
    pub ty: Type,
}

impl AnnotateItem {
    pub fn new(item: Item, ty: Type) -> Self {
        AnnotateItem {
            item: Box::new(item),
            ty,
        }
    }
}

impl Item {
    pub fn annotate(span: Span, item: Item, ty: Type) -> Self {
        Item::new(span, ItemKind::Annotate(AnnotateItem::new(item, ty)))
    }
}
