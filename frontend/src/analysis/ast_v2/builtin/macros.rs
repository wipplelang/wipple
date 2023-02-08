#[macro_export]
macro_rules! root_syntax_group {
    ($vis:vis enum $name:ident<$context:ty> { $($kind:ident),* $(,)? }) => {
        paste::paste! {
            $vis struct [<$name Syntax>];

            $vis enum $name {
                $($kind([<$kind>]),)*
            }

            $(
                impl From<$kind> for $name {
                    fn from(value: $kind) -> Self {
                        $name::$kind(value)
                    }
                }
            )*

            impl<'a> $crate::analysis::ast_v2::builtin::Syntax<'a> for [<$name Syntax>] {
                type Context = $context<'a>;
                type Body = $name;

                fn rules() -> $crate::analysis::ast_v2::builtin::SyntaxRules<'a, Self> {
                    $crate::analysis::ast_v2::builtin::SyntaxRules::new()
                    $(
                        .combine([<$kind Syntax>]::rules())
                    )*
                }
            }
        }
    };
}

#[macro_export]
macro_rules! syntax_group {
    ($vis:vis enum $name:ident<$context:ty> { $($kind:ident),* $(,)? }) => {
        paste::paste! {
            $vis struct [<$name Syntax>];

            $vis enum $name {
                $($kind([<$kind $name>]),)*
            }

            $(
                impl From<[<$kind $name>]> for $name {
                    fn from(value: [<$kind $name>]) -> Self {
                        $name::$kind(value)
                    }
                }
            )*

            impl<'a> $crate::analysis::ast_v2::builtin::Syntax<'a> for [<$name Syntax>] {
                type Context = $context<'a>;
                type Body = $name;

                fn rules() -> $crate::analysis::ast_v2::builtin::SyntaxRules<'a, Self> {
                    $crate::analysis::ast_v2::builtin::SyntaxRules::new()
                    $(
                        .combine([<$kind $name Syntax>]::rules())
                    )*
                }
            }
        }
    };
}

#[macro_export]
macro_rules! syntax_context_group {
    ($vis:vis enum $name:ident { $($kind:ident),* $(,)? }) => {
        paste::paste! {
            $vis enum $name<'a> {
                $($kind(<[<$kind Syntax>] as $crate::analysis::ast_v2::builtin::Syntax<'a>>::Context),)*
            }

            $(
                impl<'a> TryFrom<$name<'a>> for <[<$kind Syntax>] as $crate::analysis::ast_v2::builtin::Syntax<'a>>::Context {
                    type Error = ();

                    fn try_from(value: $name<'a>) -> Result<Self, Self::Error> {
                        #[allow(unreachable_patterns)]
                        match value {
                            $name::$kind(value) => Ok(value),
                            _ => Err(()),
                        }
                    }
                }
            )*
        }
    };
}
