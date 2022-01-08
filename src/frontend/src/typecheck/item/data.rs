use crate::{typecheck::*, *};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
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
    pub fn data(
        compile_info: compile::ItemInfo,
        ty: Scheme,
        id: TypeId,
        fields: Vec<Item>,
    ) -> Self {
        Item::new(compile_info, ty, ItemKind::Data(DataItem::new(id, fields)))
    }
}
