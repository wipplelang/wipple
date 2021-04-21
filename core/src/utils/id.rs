use dynamic::*;
use ref_thread_local::{ref_thread_local, RefThreadLocal};

ref_thread_local! {
    static managed COUNTER: usize = 0;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Id {
    Primitive(DynamicType),
    Runtime(usize),
}

impl Id {
    pub fn of<T: TypeInfo>() -> Self {
        Id::Primitive(DynamicType::of::<T>())
    }

    pub fn new() -> Self {
        let mut counter = COUNTER.borrow_mut();
        let id = Id::Runtime(*counter);
        *counter += 1;
        id
    }
}
