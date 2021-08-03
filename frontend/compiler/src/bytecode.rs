use crate::*;
use serde::{Deserialize, Serialize};
use std::borrow::Cow;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct File<'a> {
    pub constants: Vec<value::Value<'a>>,
    pub blocks: Vec<Block<'a>>,
    pub entrypoint: Option<usize>,
}

pub type Block<'a> = linker::Block<BlockReference<'a>, ValueReference>;
pub type Instruction<'a> = linker::Instruction<BlockReference<'a>, ValueReference>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BlockReference<'a> {
    File(FileReference, usize),
    External {
        namespace: Cow<'a, str>,
        name: Cow<'a, str>,
    },
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum ValueReference {
    Variable(usize),
    Constant(FileReference, usize),
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum FileReference {
    Current,
    Dependency(usize),
}
