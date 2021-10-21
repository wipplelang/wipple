#[macro_export]
macro_rules! id {
    ($(#[$meta:meta])* $vis:vis struct $name:ident;) => {
        $(#[$meta])*
        #[derive(Clone, Copy, PartialEq, Eq, Hash, ::serde::Serialize)]
        $vis struct $name(usize);

        ::lazy_static::lazy_static! {
            static ref NEXT_ID: ::std::sync::atomic::AtomicUsize = Default::default();
        }

        impl $name {
            #[allow(clippy::new_without_default)]
            $vis fn new() -> Self {
                Self(NEXT_ID.fetch_add(1, ::std::sync::atomic::Ordering::Relaxed))
            }

            $vis fn all() -> impl Iterator<Item = Self> {
                (0..NEXT_ID.load(::std::sync::atomic::Ordering::Release))
                    .into_iter()
                    .map($name)
            }
        }
    };
}
