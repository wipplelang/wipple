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

            impl $crate::analysis::ast::syntax::Syntax for [<$name Syntax>] {
                type Context = $context;

                fn rules() -> $crate::analysis::ast::syntax::SyntaxRules<Self> {
                    $crate::analysis::ast::syntax::SyntaxRules::new()
                    $(
                        .combine([<$wrapping_kind $name Syntax>]::rules())
                    )*
                }
            }
        }
    };
}

#[macro_export]
macro_rules! group {
    ($(#[$attr:meta])* $vis:vis enum $name:ident { $($kind:ident($data:ty)),* $(,)? }) => {
        $(#[$attr])*
        $vis enum $name {
            $($kind($data),)*
        }

        $(
            impl From<$data> for $name {
                fn from(value: $data) -> Self {
                    $name::$kind(value)
                }
            }
        )*
    };
}
