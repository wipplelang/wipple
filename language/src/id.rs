use crate::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Id {
    Type(DynamicType),
    Counter(usize),
}

thread_local! {
    static COUNTER: Rc<RefCell<usize>> = Default::default();
}

impl Id {
    #![allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let counter = COUNTER.with(Clone::clone);
        let mut counter = counter.borrow_mut();
        let id = *counter;
        *counter += 1;
        Id::Counter(id)
    }

    pub fn of<T: TypeInfo>() -> Self {
        Id::Type(DynamicType::of::<T>())
    }
}
