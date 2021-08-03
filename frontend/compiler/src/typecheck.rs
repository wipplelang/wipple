use wipple_bytecode::constant::Constant;

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

pub struct Id(usize); // TODO

pub struct RelationGraph {} // TODO

impl Type {
    pub fn of_constant(constant: Constant) -> Self {
        match constant {
            Constant::Number(_) => Type::Number,
            Constant::Text(_) => Type::Text,
            // TODO
        }
    }

    pub fn is_assignable_to(&self, other: &Self, graph: &RelationGraph) -> bool {
        todo!()
    }
}
