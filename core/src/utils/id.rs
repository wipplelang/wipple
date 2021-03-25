use crate::*;
use uuid::Uuid;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ID {
    Primitive(TypeInfo),
    Runtime(Uuid),
}

impl ID {
    pub fn of<T: 'static>() -> Self {
        ID::Primitive(TypeInfo::of::<T>())
    }

    pub fn new() -> Self {
        ID::Runtime(Uuid::new_v4())
    }
}
