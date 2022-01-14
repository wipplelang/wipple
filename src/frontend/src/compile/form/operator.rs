use crate::compile::*;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct Operator {
    pub precedence: OperatorPrecedence,
    pub template: Template,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[repr(u8)]
pub enum OperatorPrecedence {
    Assignment = 9,
    Function = 8,
    Field = 7,
    Annotation = 6,
}

impl OperatorPrecedence {
    pub const fn associativity(&self) -> OperatorAssociativity {
        match self {
            OperatorPrecedence::Assignment => OperatorAssociativity::Right,
            OperatorPrecedence::Function => OperatorAssociativity::Right,
            OperatorPrecedence::Field => OperatorAssociativity::Left,
            OperatorPrecedence::Annotation => OperatorAssociativity::Left,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum OperatorAssociativity {
    Left,
    Right,
    None,
}
