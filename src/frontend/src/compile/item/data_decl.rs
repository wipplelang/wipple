use crate::{compile::*, *};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataDeclItem {
    pub id: TypeId,
    pub fields: BTreeMap<InternedString, Constructor>,
}

impl DataDeclItem {
    pub fn new(id: TypeId, fields: BTreeMap<InternedString, Constructor>) -> Self {
        DataDeclItem { id, fields }
    }
}

impl Item {
    pub fn data_decl(
        span: Span,
        id: TypeId,
        fields: BTreeMap<InternedString, Constructor>,
    ) -> Self {
        Item::new(span, ItemKind::DataDecl(DataDeclItem::new(id, fields)))
    }
}
