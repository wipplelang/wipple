use crate::*;
use uuid::Uuid;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Id {
    Primitive(DynamicType),
    Runtime(Uuid),
}

impl Id {
    pub fn of<T: TypeInfo>() -> Self {
        Id::Primitive(DynamicType::of::<T>())
    }

    pub fn new() -> Self {
        Id::Runtime(Uuid::new_v4())
    }
}
