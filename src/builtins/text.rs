use crate::fundamentals::*;

#[derive(Clone, Eq, PartialEq)]
pub struct Text(pub String);

simple_trait! {
    name: text,
    type: Text,
    label: "Text",
}
