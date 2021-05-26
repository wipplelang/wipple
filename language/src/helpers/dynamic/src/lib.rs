pub use derive::TypeInfo;

use std::{fmt, rc::Rc};

pub trait TypeInfo: Clone + 'static {
    const DYNAMIC_TYPE: DynamicType;
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DynamicType {
    pub crate_name: &'static str,
    pub crate_version: &'static str,
    pub module: &'static str,
    pub type_name: &'static str,
    pub generics: &'static [DynamicType],
}

impl DynamicType {
    #[inline]
    pub fn of<T: TypeInfo>() -> Self {
        T::DYNAMIC_TYPE
    }
}

impl std::fmt::Debug for DynamicType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(self.type_name)
    }
}

#[derive(Clone)]
pub struct Any(Inner);

impl<T: TypeInfo> From<T> for Any {
    fn from(value: T) -> Self {
        Any(Inner {
            r#type: DynamicType::of::<T>(),
            value: Box::into_raw(Box::new(value)) as *mut (),
            clone: Rc::new(|value| {
                let value = unsafe { &*(value as *mut T) };
                let cloned = value.clone();

                Box::into_raw(Box::new(cloned)) as *mut ()
            }),
            drop: Some(Rc::new(|value| {
                let value = *unsafe { Box::from_raw(value as *mut T) };
                drop(value)
            })),
        })
    }
}

impl std::fmt::Debug for Any {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("Dynamic")
    }
}

struct Inner {
    r#type: DynamicType,
    value: *mut (),
    clone: Rc<dyn Fn(*mut ()) -> *mut ()>,
    drop: Option<Rc<dyn Fn(*mut ())>>,
}

impl Drop for Inner {
    fn drop(&mut self) {
        if let Some(drop) = &self.drop {
            drop(self.value);
        }
    }
}

impl Any {
    pub fn dynamic_type(&self) -> DynamicType {
        self.0.r#type
    }

    pub fn try_into_cast<T: TypeInfo>(mut self) -> Option<T> {
        if self.0.r#type == DynamicType::of::<T>() {
            let value = *unsafe { Box::from_raw(self.0.value as *mut T) };
            self.0.drop = None; // prevent double free
            Some(value)
        } else {
            None
        }
    }

    pub fn into_cast<T: TypeInfo>(self) -> T {
        let type_info = self.0.r#type;

        self.try_into_cast().unwrap_or_else(|| {
            panic!(
                "Cannot cast from {:?} to {:?}",
                type_info,
                DynamicType::of::<T>()
            )
        })
    }

    pub fn try_cast<T: TypeInfo>(&self) -> Option<&T> {
        if self.0.r#type == DynamicType::of::<T>() {
            let value = unsafe { &*(self.0.value as *mut T) };
            Some(value)
        } else {
            None
        }
    }

    pub fn cast<T: TypeInfo>(&self) -> &T {
        let type_info = self.0.r#type;

        self.try_cast().unwrap_or_else(|| {
            panic!(
                "Cannot cast from {:?} to {:?}",
                type_info,
                DynamicType::of::<T>()
            )
        })
    }

    pub fn try_cast_mut<T: TypeInfo>(&mut self) -> Option<&mut T> {
        if self.0.r#type == DynamicType::of::<T>() {
            let value = unsafe { &mut *(self.0.value as *mut T) };
            Some(value)
        } else {
            None
        }
    }

    pub fn cast_mut<T: TypeInfo>(&mut self) -> &mut T {
        let type_info = self.0.r#type;

        self.try_cast_mut().unwrap_or_else(|| {
            panic!(
                "Cannot cast from {:?} to {:?}",
                type_info,
                DynamicType::of::<T>()
            )
        })
    }
}

impl Clone for Inner {
    fn clone(&self) -> Self {
        Inner {
            r#type: self.r#type,
            value: (self.clone)(self.value),
            clone: self.clone.clone(),
            drop: self.drop.clone(),
        }
    }
}

impl fmt::Debug for Inner {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(Dynamic {:?})", self.r#type)
    }
}

#[cfg(test)]
mod tests;
