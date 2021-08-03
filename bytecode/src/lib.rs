pub mod constant;
pub mod linker;
pub mod module;
pub mod object;

use serde::{Deserialize, Serialize};
use std::{cell::RefCell, collections::HashMap, mem};

pub type Block<BlockReference, ValueReference> = Vec<Instruction<BlockReference, ValueReference>>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Instruction<BlockReference, ValueReference> {
    Enter(BlockReference, Vec<ValueReference>),
    Exit(Vec<ValueReference>),
}
