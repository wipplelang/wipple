pub use paste::paste;

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

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "({})", stringify!($name))
            }
        }
    };
}

macro_rules! core_primitive {
    ($vis:vis $name:ident for $Type:ty) => {
        impl $crate::Primitive for $Type {}

        impl $crate::Trait {
            $vis fn $name() -> Self {
                $crate::Trait::of::<$Type>()
            }
        }
    };
}

#[macro_export]
macro_rules! primitive {
    ($vis:vis $name:ident for $Type:ty) => {
        impl $crate::Primitive for $Type {}

        $crate::paste! {
            $vis fn [<$name _trait>]() -> $crate::Trait {
                $crate::Trait::of::<$Type>()
            }
        }
    };
}

macro_rules! core_env_key {
    ($vis:vis $name:ident for $Type:ty { visibility: $visibility:expr $(,)? }) => {
        impl $crate::EnvironmentKey {
            $vis fn $name() -> Self {
                EnvironmentKey::of::<$Type>($visibility)
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

#[macro_export]
macro_rules! env_key {
    ($vis:vis $name:ident for $Type:ty { visibility: $visibility:expr $(,)? }) => {
        $crate::paste! {
            $vis fn [<$name _env_key>]() -> $crate::EnvironmentKey {
                $crate::EnvironmentKey::of::<$Type>($visibility)
            }

            $vis fn [<$name _in>](env: &mut $crate::Environment) -> &mut $Type {
                env.get_or_insert(&[<$name _env_key>](), $crate::Dynamic::new(<$Type>::default()))
                    .cast_mut::<$Type>()
            }
        }
    };
}

macro_rules! core_stack_key {
    ($vis:vis $name:ident for $Type:ty) => {
        impl $crate::StackKey {
            $vis fn $name() -> Self {
                StackKey::of::<$Type>()
            }
        }

        impl $crate::Stack {
            #![allow(dead_code)]

            $crate::paste! {
                $vis fn $name(&self) -> $Type {
                    self.get(StackKey::$name(), Dynamic::new(<$Type>::default()))
                        .into_cast::<$Type>()
                }

                $vis fn [<$name _mut>](&mut self) -> &mut $Type {
                    self.get_mut(StackKey::$name(), Dynamic::new(<$Type>::default()))
                        .cast_mut::<$Type>()
                }
            }
        }
    };
}

#[macro_export]
macro_rules! stack_key {
    ($vis:vis $name:ident for $Type:ty) => {
        $crate::paste! {
            $vis fn [<$name _stack_key>]() -> $crate::StackKey {
                $crate::StackKey::of::<$Type>()
            }

            $vis fn [<$name _in>](stack: &$crate::Stack) -> $Type {
                stack.get([<$name _stack_key>](), $crate::Dynamic::new(<$Type>::default()))
                    .into_cast::<$Type>()
            }

            $vis fn [<$name _mut_in>](stack: &mut $crate::Stack) -> &mut $Type {
                stack.get_mut([<$name _stack_key>](), $crate::Dynamic::new(<$Type>::default()))
                    .cast_mut::<$Type>()
            }
        }
    };
}
