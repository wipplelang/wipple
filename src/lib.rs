#[macro_export]
macro_rules! primitive {
    ($name:ident for $Type:ident) => {
        impl $crate::Primitive for $Type {}

        impl $crate::TraitID {
            pub fn $name() -> Self {
                $crate::TraitID::new_primitive::<$Type>()
            }
        }
    };
}

#[macro_export]
macro_rules! env_key {
    ($name:ident for $Type:ident { $key:expr }) => {
        impl $crate::EnvironmentKey {
            pub fn $name() -> Self {
                thread_local! {
                    static KEY: EnvironmentKey = $key;
                }

                KEY.with(|k| k.clone())
            }
        }

        impl $crate::Environment {
            pub fn $name(&mut self) -> &mut $Type {
                self.get_or_insert(&EnvironmentKey::$name(), Dynamic::new($Type::default()))
                    .downcast_mut::<$Type>()
                    .unwrap()
            }
        }
    };
}

mod builtins;
mod dynamic;
mod fundamentals;
mod prelude;

pub use builtins::*;
pub use dynamic::*;
pub use fundamentals::*;
pub use prelude::*;
