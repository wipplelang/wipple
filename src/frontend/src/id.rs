macro_rules! id {
    ($($(#[$meta:meta])* $name:ident,)*) => {
        $(::paste::paste! {
            $(#[$meta])*
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, ::serde::Serialize)]
            pub struct $name(pub usize);

            thread_local! {
                static [<NEXT_ $name:upper _ID>]: ::std::cell::Cell<usize> = Default::default();
            }

            impl $name {
                #[allow(clippy::new_without_default)]
                pub fn new() -> Self {
                    [<NEXT_ $name:upper _ID>].with(|next_id| {
                        let id = next_id.get();
                        next_id.set(id + 1);
                        Self(id)
                    })
                }

                pub fn reset() {
                    [<NEXT_ $name:upper _ID>].with(|next_id| next_id.set(0))
                }
            }
        })*

        pub fn reset() {
            $($name::reset();)*
        }
    };
}

id! {
    VariableId,
}
