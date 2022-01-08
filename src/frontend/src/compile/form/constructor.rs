use crate::{compile::*, *};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Constructor {
    Placeholder,
    Never,
    Number,
    Text,
    Unit, // TODO: Tuples
    Function {
        input: Box<Constructor>,
        output: Box<Constructor>,
    },
    DataStruct {
        id: TypeId,
        fields: BTreeMap<InternedString, DataStructFieldDecl>,
    },
    // TODO: DataTuple
    // TODO: Enum
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataStructFieldDecl {
    pub info: DataStructFieldDeclInfo,
    pub constructor: Constructor,
}

impl DataStructFieldDecl {
    pub fn new(info: DataStructFieldDeclInfo, constructor: Constructor) -> Self {
        DataStructFieldDecl { info, constructor }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataStructFieldDeclInfo {
    pub span: Span,
    pub name: InternedString,
}

impl DataStructFieldDeclInfo {
    pub fn new(span: Span, name: InternedString) -> Self {
        DataStructFieldDeclInfo { span, name }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataStructField {
    pub info: DataStructFieldInfo,
    pub name: InternedString,
    pub value: Item,
}

impl DataStructField {
    pub fn new(info: DataStructFieldInfo, name: InternedString, value: Item) -> Self {
        DataStructField { info, name, value }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataStructFieldInfo {
    pub span: Span,
}

impl DataStructFieldInfo {
    pub fn new(span: Span) -> Self {
        DataStructFieldInfo { span }
    }
}
