#[macro_export]
macro_rules! fn_wrapper_struct {
    {
        $(#[$attr:meta])*
        $vis:vis type $name:ident ($($args:ty),*) $(-> $return:ty)?;
    } => {
        $(#[$attr])*
        #[derive(Clone)]
        $vis struct $name(std::rc::Rc<dyn Fn($($args),*) $(-> $return)?>);

        impl $name {
            $vis fn new(func: impl Fn($($args),*) $(-> $return)? + 'static) -> Self {
                $name(std::rc::Rc::new(func))
            }
        }

        impl std::ops::Deref for $name {
            type Target = dyn Fn($($args),*) $(-> $return)?;

            fn deref(&self) -> &Self::Target {
                &*self.0
            }
        }
    };
}

macro_rules! fundamental_primitive {
    ($vis:vis $name:ident for $Type:ty) => {
        impl $crate::Primitive for $Type {}

        impl $crate::TraitID {
            $vis fn $name() -> Self {
                $crate::TraitID::of::<$Type>()
            }
        }
    };
}

#[macro_export]
macro_rules! primitive {
    ($vis:vis $name:ident for $Type:ty) => {
        impl $crate::Primitive for $Type {}

        $vis fn $name() -> Self {
            $crate::TraitID::of::<$Type>()
        }
    };
}

macro_rules! fundamental_env_key {
    ($vis:vis $name:ident for $Type:ty { $key:expr }) => {
        impl $crate::EnvironmentKey {
            $vis fn $name() -> Self {
                thread_local! {
                    static KEY: EnvironmentKey = $key;
                }

                KEY.with(|k| k.clone())
            }
        }

        impl $crate::Environment {
            $vis fn $name(&mut self) -> &mut $Type {
                self.get_or_insert(&EnvironmentKey::$name(), Dynamic::new(<$Type>::default()))
                    .cast_mut::<$Type>()
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
                    .cast_mut::<$Type>()
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
pub use prelude::{setup, *};
