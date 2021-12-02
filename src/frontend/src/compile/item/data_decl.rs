use crate::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct DataDeclItem {
    pub id: TypeId,
    pub fields: Vec<Constructor>,
}

impl DataDeclItem {
    pub fn new(id: TypeId, fields: Vec<Constructor>) -> Self {
        DataDeclItem { id, fields }
    }
}

impl Item {
    pub fn data_decl(span: Span, id: TypeId, fields: Vec<Constructor>) -> Self {
        Item::new(span, ItemKind::DataDecl(DataDeclItem::new(id, fields)))
    }
}
