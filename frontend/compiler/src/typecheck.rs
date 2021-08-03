use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Type {
    Number,
    Text,
    List(Id),
    Tuple(Vec<Id>),
    Function(Id, Id),
    Trait(Id),
    Struct(Id, Vec<Id>),
    Enum(Id, Vec<Vec<Id>>),
    // TODO
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Id(usize); // TODO

pub struct RelationGraph {} // TODO

impl Type {
    pub fn is_assignable_to(&self, other: &Self, graph: &RelationGraph) -> bool {
        todo!()
    }
}
