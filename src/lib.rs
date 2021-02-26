#[macro_export]
macro_rules! primitive {
    ($name:ident for $Type:ident) => {
        impl crate::Primitive for $Type {}

        impl crate::TraitID {
            pub fn $name() -> Self {
                crate::TraitID::new_primitive::<$Type>()
            }
        }

        impl crate::Trait {
            pub fn $name(value: $Type) -> Self {
                crate::Trait {
                    id: crate::TraitID::$name(),
                    value: crate::Value::Primitive(std::rc::Rc::new(value)),
                }
            }
        }
    };
}

mod builtins;
mod fundamentals;
mod prelude;

pub use builtins::*;
pub use fundamentals::*;
pub use prelude::*;
