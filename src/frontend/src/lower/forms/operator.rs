use crate::lower::Template;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct Operator {
    pub precedence: OperatorPrecedence,
    pub associativity: OperatorAssociativity,
    pub template: Template,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct OperatorPrecedence(u8);

impl OperatorPrecedence {
    pub fn new(value: u8) -> Self {
        debug_assert!(matches!(value, 0..=9));

        OperatorPrecedence(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum OperatorAssociativity {
    Left,
    Right,
    None,
}
