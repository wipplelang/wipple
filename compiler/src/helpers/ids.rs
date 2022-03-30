macro_rules! ids {
    ($($(#[$meta:meta])* $name:ident,)*) => {
        $(::paste::paste! {
            $(#[$meta])*
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ::serde::Serialize, ::serde::Deserialize)]
            pub struct $name(pub usize);

            thread_local! {
                static [<NEXT_ $name:snake:upper>]: ::std::cell::Cell<usize> = Default::default();
            }

            impl $name {
                pub fn new() -> Self {
                    [<NEXT_ $name:snake:upper>].with(|next_id| {
                        let id = next_id.get();
                        next_id.set(id + 1);
                        Self(id)
                    })
                }

                fn reset() {
                    [<NEXT_ $name:snake:upper>].with(|next_id| next_id.set(0))
                }
            }

            impl Default for $name {
                fn default() -> Self {
                    Self::new()
                }
            }
        })*

        pub(crate) fn reset_ids() {
            $($name::reset();)*
        }
    };
}

ids! {
    TypeId,
    TypeParameterId,
    OperatorId,
    VariableId,
    ConstantId,
}
