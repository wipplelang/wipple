use crate::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct DataField {
    pub info: DataFieldInfo,
    pub ty: Type,
}

impl DataField {
    pub fn new(info: DataFieldInfo, ty: Type) -> Self {
        DataField { info, ty }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct DataFieldInfo {
    pub span: Span,
    pub name: InternedString,
}

impl DataFieldInfo {
    pub fn new(span: Span, name: InternedString) -> Self {
        DataFieldInfo { span, name }
    }
}
