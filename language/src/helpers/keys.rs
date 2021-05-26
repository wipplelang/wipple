#[macro_export]
macro_rules! stack_key {
    ($vis:vis $name:ident: $type:ty) => {
        $crate::paste! {
            thread_local! {
                static [<$name:upper _STACK_KEY>]: $crate::StackKey = $crate::StackKey::new();
            }

            #[$crate::ext($vis, name = [<$name:camel StackExt>])]
            impl $crate::Stack {
                fn $name(&self) -> $crate::Cow<$type> {
                    self.get::<$type>([<$name:upper _STACK_KEY>].with(|key| *key))
                }

                fn [<$name _mut>](&mut self) -> &mut $type {
                    self.get_mut::<$type>([<$name:upper _STACK_KEY>].with(|key| *key))
                }
            }
        }
    };
}

#[macro_export]
macro_rules! env_key {
    ($vis:vis $name:ident: $type:ty { visibility: $visibility:expr, }) => {
        $crate::paste! {
            thread_local! {
                static [<$name:upper _ENV_KEY>]: $crate::EnvKey = $crate::EnvKey::new($visibility);
            }

            #[ext($vis, name = [<$name:camel EnvExt>])]
            impl $crate::Env {
                fn $name(&self) -> $type {
                    self.get(&[<$name:upper _ENV_KEY>].with(Clone::clone))
                }

                fn [<set_ $name>](&self, value: $type) {
                    self.set(&[<$name:upper _ENV_KEY>].with(Clone::clone), value)
                }

                fn [<update_ $name>]<T>(&self, update: impl FnOnce(&mut $type) -> T) -> T {
                    self.update(&[<$name:upper _ENV_KEY>].with(Clone::clone), update)
                }
            }
        }
    };
}
