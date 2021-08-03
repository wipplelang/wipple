use crate::*;
use serde::{Deserialize, Serialize};
use std::borrow::Cow;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct File<'a> {
    pub constants: Vec<constant::Constant<'a>>,
    pub blocks: Vec<Block<'a>>,
    pub entrypoint: usize,
}

pub type Block<'a> = crate::Block<BlockReference<'a>, ValueReference>;
pub type Instruction<'a> = crate::Instruction<BlockReference<'a>, ValueReference>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BlockReference<'a> {
    Block(usize),
    External {
        namespace: Option<Cow<'a, str>>,
        name: Cow<'a, str>,
    },
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum ValueReference {
    Variable(usize),
    Constant(usize),
}
