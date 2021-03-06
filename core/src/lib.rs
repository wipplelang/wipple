macro_rules! fundamental_primitive {
    ($name:ident for $Type:ty) => {
        impl $crate::Primitive for $Type {}

        impl $crate::TraitID {
            pub fn $name() -> Self {
                $crate::TraitID::new_primitive::<$Type>()
            }
        }
    };
}

#[macro_export]
macro_rules! primitive {
    ($vis:vis $name:ident for $Type:ty) => {
        impl $crate::Primitive for $Type {}

        $vis fn $name() -> Self {
            $crate::TraitID::new_primitive::<$Type>()
        }
    };
}

macro_rules! fundamental_env_key {
    ($name:ident for $Type:ty { $key:expr }) => {
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
                self.get_or_insert(&EnvironmentKey::$name(), Dynamic::new(<$Type>::default()))
                    .downcast_mut::<$Type>()
                    .unwrap()
            }
        }
    };
}

pub use paste::paste;

#[macro_export]
macro_rules! env_key {
    ($vis:vis $name:ident for $Type:ty { $key:expr }) => {
        $crate::paste! {
            $vis fn [<$name _key>]() -> EnvironmentKey {
                thread_local! {
                    static KEY: EnvironmentKey = $key;
                }

                KEY.with(|k| k.clone())
            }

            $vis fn [<get_ $name>](env: &mut $crate::Environment) -> &mut $Type {
                env.get_or_insert(&[<$name _key>](), Dynamic::new(<$Type>::default()))
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
