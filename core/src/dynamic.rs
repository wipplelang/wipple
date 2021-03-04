use std::{
    any::{Any, TypeId},
    fmt,
    ops::{Deref, DerefMut},
    rc::Rc,
};

#[cfg(debug_assertions)]
use std::any::type_name;

/// Wrapper around `Any` that allows cloning.
pub struct Dynamic {
    pub type_id: TypeId,
    value: Box<dyn Any>,
    clone: Rc<dyn Fn() -> Box<dyn Any>>,

    #[cfg(debug_assertions)]
    pub type_name: &'static str,
}

impl Dynamic {
    pub fn new<T: Clone + 'static>(value: T) -> Self {
        Dynamic {
            type_id: value.type_id(),
            value: Box::new(value.clone()),
            clone: Rc::new(move || Box::new(value.clone())),

            #[cfg(debug_assertions)]
            type_name: type_name::<T>(),
        }
    }
}

impl Clone for Dynamic {
    fn clone(&self) -> Self {
        Dynamic {
            type_id: self.type_id,
            value: (self.clone)(),
            clone: self.clone.clone(),

            #[cfg(debug_assertions)]
            type_name: self.type_name,
        }
    }
}

impl Deref for Dynamic {
    type Target = Box<dyn Any>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl DerefMut for Dynamic {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl fmt::Debug for Dynamic {
    #[cfg(debug_assertions)]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(Dynamic {})", self.type_name)
    }

    #[cfg(not(debug_assertions))]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(Dynamic)")
    }
}
