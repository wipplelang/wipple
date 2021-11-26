use crate::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct DataItem {
    pub id: TypeId,
    pub fields: Vec<Item>,
}

impl DataItem {
    pub fn new(id: TypeId, fields: Vec<Item>) -> Self {
        DataItem { id, fields }
    }
}

impl Item {
    pub fn data(span: Span, id: TypeId, fields: Vec<Item>) -> Self {
        Item::new(span, ItemKind::Data(DataItem::new(id, fields)))
    }
}
