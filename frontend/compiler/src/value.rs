use crate::typecheck::Type;
use serde::{Deserialize, Serialize};
use std::borrow::Cow;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Value<'a> {
    Number(f64),
    Text(Cow<'a, str>),
    // TODO
}

impl Value<'_> {
    pub fn r#type(&self) -> Type {
        match self {
            Value::Number(_) => Type::Number,
            Value::Text(_) => Type::Text,
            // TODO
        }
    }
}
