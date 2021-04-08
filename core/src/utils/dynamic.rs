use std::{
    any::{Any, TypeId},
    fmt,
    rc::Rc,
};

#[cfg(debug_assertions)]
use std::any::type_name;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeInfo {
    pub id: TypeId,

    #[cfg(debug_assertions)]
    pub name: &'static str,
}

impl TypeInfo {
    pub fn of<T: 'static>() -> Self {
        TypeInfo {
            id: TypeId::of::<T>(),

            #[cfg(debug_assertions)]
            name: type_name::<T>(),
        }
    }
}

impl fmt::Debug for TypeInfo {
    #[cfg(debug_assertions)]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }

    #[cfg(not(debug_assertions))]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(TypeInfo)")
    }
}

/// Wrapper around `Any` that allows cloning.
pub struct Dynamic {
    pub type_info: TypeInfo,
    value: Box<dyn Any>,
    clone: Rc<dyn Fn(&dyn Any) -> Box<dyn Any>>,

    #[cfg(debug_assertions)]
    #[allow(clippy::clippy::type_complexity)]
    debug: Rc<dyn Fn(&dyn Any, &mut std::fmt::Formatter) -> std::fmt::Result>,
}

impl Dynamic {
    #[cfg(debug_assertions)]
    pub fn new<T: std::fmt::Debug + Clone + 'static>(value: T) -> Self {
        Dynamic {
            type_info: TypeInfo::of::<T>(),
            value: Box::new(value),
            clone: Rc::new(move |value| Box::new(value.downcast_ref::<T>().unwrap().clone())),
            debug: Rc::new(|value, f| value.downcast_ref::<T>().unwrap().clone().fmt(f)),
        }
    }

    #[cfg(not(debug_assertions))]
    pub fn new<T: Clone + 'static>(value: T) -> Self {
        Dynamic {
            type_info: TypeInfo::of::<T>(),
            value: Box::new(value),
            clone: Rc::new(move |value| Box::new(value.downcast_ref::<T>().unwrap().clone())),
        }
    }

    pub fn try_into_cast<T: Clone + 'static>(self) -> Option<T> {
        self.value.downcast::<T>().ok().map(|x| *x)
    }

    pub fn into_cast<T: Clone + 'static>(self) -> T {
        let type_info = self.type_info;

        self.try_into_cast().unwrap_or_else(|| {
            panic!(
                "Cannot cast from {:?} to {:?}",
                type_info,
                TypeInfo::of::<T>()
            )
        })
    }

    pub fn try_cast<T: Clone + 'static>(&self) -> Option<&T> {
        self.value.downcast_ref()
    }

    pub fn cast<T: Clone + 'static>(&self) -> &T {
        let type_info = self.type_info;

        self.try_cast().unwrap_or_else(|| {
            panic!(
                "Cannot cast from {:?} to {:?}",
                type_info,
                TypeInfo::of::<T>()
            )
        })
    }

    pub fn try_cast_mut<T: Clone + 'static>(&mut self) -> Option<&mut T> {
        self.value.downcast_mut()
    }

    pub fn cast_mut<T: Clone + 'static>(&mut self) -> &mut T {
        let type_info = self.type_info;

        self.try_cast_mut().unwrap_or_else(|| {
            panic!(
                "Cannot cast from {:?} to {:?}",
                type_info,
                TypeInfo::of::<T>()
            )
        })
    }
}

impl Clone for Dynamic {
    fn clone(&self) -> Self {
        Dynamic {
            type_info: self.type_info,
            value: (self.clone)(&*self.value),
            clone: self.clone.clone(),

            #[cfg(debug_assertions)]
            debug: self.debug.clone(),
        }
    }
}

impl fmt::Debug for Dynamic {
    #[cfg(debug_assertions)]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Dynamic(")?;
        (self.debug)(&*self.value, f)?;
        write!(f, ")")?;
        Ok(())
    }

    #[cfg(not(debug_assertions))]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(Dynamic)")
    }
}
