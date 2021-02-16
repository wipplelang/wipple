use std::{convert::identity, rc::Rc};

#[derive(Clone)]
pub struct Any {
    pub value: Rc<dyn std::any::Any>,

    #[cfg(debug_assertions)]
    pub type_name: &'static str,
}

impl Any {
    pub fn from<T: 'static + Clone>(value: T) -> Any {
        // Prevent having nested 'Any's
        if let Some(existing) = identity::<&dyn std::any::Any>(&value).downcast_ref::<Any>() {
            return existing.clone();
        }

        Any {
            value: Rc::new(value.clone()),

            #[cfg(debug_assertions)]
            type_name: std::any::type_name::<T>(),
        }
    }

    pub fn try_cast<T: 'static + Clone>(&self) -> Option<&T> {
        self.value.downcast_ref::<T>()
    }

    pub fn cast<T: 'static + Clone>(&self) -> &T {
        if cfg!(debug_assertions) {
            self.try_cast().expect(&format!(
                "Cannot cast from {} to {}",
                self.type_name,
                std::any::type_name::<T>(),
            ))
        } else {
            self.try_cast().expect("Cannot cast")
        }
    }
}
