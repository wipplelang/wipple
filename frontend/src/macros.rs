#[macro_export]
macro_rules! non_error_kind {
    ($(#[$attr:meta])* $vis:vis enum $t:ident { $($variant:ident$(($($name:ident: $value:ty),* $(,)?))?),* $(,)? }) => {
        paste::paste! {
            $(#[$attr])*
            $vis enum [<NonError $t>] {
                $($variant$(($($value),*))?),*
            }

            $(#[$attr])*
            $vis enum $t {
                Error($crate::helpers::Backtrace),
                $($variant$(($($value),*))?),*
            }

            impl From<[<NonError $t>]> for $t {
                fn from(value: [<NonError $t>]) -> Self {
                    match value {
                        $([<NonError $t>]::$variant$(($($name),*))? => $t::$variant$(($($name),*))?,)*
                    }
                }
            }
        }
    };
}
