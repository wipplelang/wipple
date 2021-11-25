use crate::*;
use serde::Serialize;
use std::collections::BTreeMap;

#[derive(Debug, Clone, Serialize)]
pub enum Constructor {
    Placeholder,
    Number,
    Text,
    Unit, // TODO: Tuples
    Function {
        input: Box<Constructor>,
        output: Box<Constructor>,
    },
    DataStruct {
        id: TypeId,
        fields: BTreeMap<InternedString, DataStructField>,
    },
    // TODO: DataTuple
    // TODO: Enum
}

#[derive(Debug, Clone, Serialize)]
pub struct DataStructField {
    pub info: DataStructFieldInfo,
    pub constructor: Constructor,
}

impl DataStructField {
    pub fn new(info: DataStructFieldInfo, constructor: Constructor) -> Self {
        DataStructField { info, constructor }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct DataStructFieldInfo {
    pub span: Span,
    pub name: InternedString,
}

impl DataStructFieldInfo {
    pub fn new(span: Span, name: InternedString) -> Self {
        DataStructFieldInfo { span, name }
    }
}
