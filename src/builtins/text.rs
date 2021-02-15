use crate::fundamentals::*;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Text(pub String);

simple_trait! {
    name: text,
    type: Text,
    label: "Text",
}
