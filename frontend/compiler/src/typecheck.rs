use wipple_bytecode::constant::Constant;

pub(crate) enum Type {
    Number,
    Text,
    Import,
    List(Id),
    Tuple(Vec<Id>),
    Function(Id, Id),
    Trait(Id),
    Struct(Id, Vec<Id>),
    Enum(Id, Vec<Vec<Id>>),
    // TODO
}

pub(crate) struct Id(usize); // TODO

#[derive(Default)]
pub(crate) struct RelationGraph {} // TODO

impl Type {
    pub fn of_constant(constant: Constant) -> Self {
        match constant {
            Constant::Number(_) => Type::Number,
            Constant::Text(_) => Type::Text,
            Constant::Import(_) => Type::Import,
            // TODO
        }
    }

    pub fn is_assignable_to(&self, other: &Self, graph: &RelationGraph) -> bool {
        todo!()
    }
}
