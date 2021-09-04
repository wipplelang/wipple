#[macro_export]
#[doc(hidden)]
macro_rules! id {
    ($(#[$meta:meta])* $vis:vis $name:ident;) => {
        $(#[$meta])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        $vis struct $name(u64);

        impl $name {
            /// Create a new, unique identifier.
            #[allow(clippy::new_without_default)]
            $vis fn new() -> Self {
                use ::std::sync::atomic::{AtomicU64, Ordering};

                static COUNTER: AtomicU64 = AtomicU64::new(0);

                Self(COUNTER.fetch_add(1, Ordering::Relaxed))
            }
        }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! parser_expression_kind {
    ($(#[$meta:meta])* $vis:vis $t:ident of $e:ident;) => {
        $(#[$meta])*
        #[derive(Debug, Clone)]
        $vis enum $t {
            /// A name, eg. `x`.
            Name(&'static str),

            /// A number, eg. `42`.
            Number(f64),

            /// A piece of text, eg. `"hello"`.
            Text(&'static str),

            /// A list, eg. `(a b c)`.
            List(Vec<$e>),

            /// A block, eg. `{ a b c }`.
            Block(Vec<Vec<$e>>),

            // TODO
        }
    };
}
