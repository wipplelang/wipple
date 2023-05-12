#[macro_export]
macro_rules! syntax_group {
    ($(#[$attr:meta])* $vis:vis type $name:ident<$context:ty> {
        non_terminal: {
            $($wrapping_kind:ident),* $(,)?
        },
        terminal: {
            $($terminal_kind:ident),* $(,)?
        },
    }) => {
        paste::paste! {
            group! {
                $(#[$attr])*
                $vis enum $name {
                    $($wrapping_kind([<$wrapping_kind $name>]),)*
                    $($terminal_kind([<$terminal_kind $name>]),)*
                }
            }

            $vis struct [<$name Syntax>];

            impl<D: $crate::Driver> $crate::ast::syntax::Syntax<D> for [<$name Syntax>] {
                type Context = $context<D>;

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
        #[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
        #[cfg_attr(feature = "arbitrary", arbitrary(bound = "D: crate::FuzzDriver"))]
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
    };
}
