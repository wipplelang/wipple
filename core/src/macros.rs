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

        $vis fn $name() -> Self {
            $crate::ID::of::<$Type>()
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
            $vis fn [<$name _env_key>]() -> EnvironmentKey {
                EnvironmentKey::of::<$Type>($visibility)
            }

            $vis fn [<get_ $name _in>](env: &mut $crate::Environment) -> &mut $Type {
                env.get_or_insert(&[<$name _env_key>](), Dynamic::new(<$Type>::default()))
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
                $vis fn [<get_ $name>](self) -> $Type {
                    self.get_or(StackKey::$name(), Dynamic::new(<$Type>::default()))
                        .into_cast::<$Type>()
                }

                $vis fn [<with_ $name>](self, value: $Type) -> Self {
                    self.with(StackKey::$name(), Dynamic::new(value))
                }

                $vis fn [<update_ $name>](self, update: impl FnOnce($Type) -> $Type) -> Self {
                    let value = self.clone().[<get_ $name>]().clone();
                    let value = update(value);
                    self.[<with_ $name>](value)
                }
            }
        }
    };
}

#[macro_export]
macro_rules! stack_key {
    ($vis:vis $name:ident for $Type:ty) => {
        $crate::paste! {
            $vis fn [<$name _stack_key>]() -> StackKey {
                StackKey::of::<$Type>()
            }

            $vis fn [<get_ $name _in>](stack: $crate::Stack) -> $Type {
                stack
                    .get_or([<$name _stack_key>](), Dynamic::new(<$Type>::default()))
                    .into_cast::<$Type>()
            }

            $vis fn [<with_ $name _in>](stack: $crate::Stack, value: $Type) -> $crate::Stack {
                stack.with([<$name _stack_key>](), Dynamic::new(value))
            }

            $vis fn [<update_ $name>](stack: $crate::Stack, update: impl FnOnce($Type) -> $Type) -> $crate::Stack {
                let value = [<get_ $name _in>](stack.clone()).clone();
                let value = update(value);
                [<with_ $name _in>](stack, value)
            }
        }
    };
}
