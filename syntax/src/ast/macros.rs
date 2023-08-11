macro_rules! definitions {
    ($(mod $name:ident;)*) => {
        $(mod $name;)*

        $(pub use $name::*;)*

        pub fn builtin_syntax_definitions() -> Vec<$crate::ast::BuiltinSyntaxDefinition> {
            let definitions: Vec<Vec<$crate::ast::BuiltinSyntaxDefinition>> =
                vec![$($name::builtin_syntax_definitions()),*];

            definitions
                .into_iter()
                .flatten()
                .collect()
        }
    };
}

macro_rules! syntax_group {
    ($(#[$attr:meta])* $vis:vis type $name:ident<$context:ty> {
        $(shallow: $shallow:expr,)?
        non_terminal: {
            $($wrapping_kind:ident),* $(,)?
        },
        terminal: {
            $($terminal_kind:ident),* $(,)?
        },
    }) => {
        paste::paste! {
            $crate::ast::macros::group! {
                $(#[$attr])*
                $vis enum $name {
                    $($wrapping_kind([<$wrapping_kind $name>]),)*
                    $($terminal_kind([<$terminal_kind $name>]),)*
                }
            }

            $vis struct [<$name Syntax>];

            impl<D: $crate::Driver> $crate::ast::syntax::Syntax<D> for [<$name Syntax>] {
                type Context = $context<D>;
                $(const SHALLOW: bool = $shallow;)?

                fn rules() -> $crate::ast::syntax::SyntaxRules<D, Self> {
                    $crate::ast::syntax::SyntaxRules::new()
                    $(
                        .combine([<$wrapping_kind $name Syntax>]::rules())
                    )*
                }
            }
        }
    };
}

macro_rules! group {
    ($(#[$attr:meta])* $vis:vis enum $name:ident {
        $($kind:ident($data:ident)),* $(,)?
    }) => {
        $(#[$attr])*
        #[derive(Debug, Clone)]
        $vis enum $name<D: $crate::Driver> {
            $($kind($data<D>),)*
        }

        impl<D: $crate::Driver> $name<D> {
            pub fn span(&self) -> D::Span {
                match self {
                    $(
                        $name::$kind(value) => value.span(),
                    )*
                }
            }
        }

        $(
            impl<D: $crate::Driver> From<$data<D>> for $name<D> {
                fn from(value: $data<D>) -> Self {
                    $name::$kind(value)
                }
            }
        )*

        impl<D: $crate::Driver> $crate::ast::format::Format<D> for $name<D> {
            fn format(self) -> Result<String, SyntaxError<D>> {
                match self {
                    $(
                        $name::$kind(value) => $crate::ast::format::Format::format(value),
                    )*
                }
            }
        }
    };
}

pub(crate) use definitions;
pub(crate) use group;
pub(crate) use syntax_group;
