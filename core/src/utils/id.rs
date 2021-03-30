use crate::*;
use uuid::Uuid;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Id {
    Primitive(TypeInfo),
    Runtime(Uuid),
}

impl Id {
    pub fn of<T: 'static>() -> Self {
        Id::Primitive(TypeInfo::of::<T>())
    }

    pub fn new() -> Self {
        Id::Runtime(Uuid::new_v4())
    }
}
