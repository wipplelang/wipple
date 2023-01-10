#[macro_export]
macro_rules! parse_number {
    ($number:ident, $number_ty:ident, $ty:expr, $expr_ty:ident) => {
        (|| {
            use $crate::analysis::typecheck::engine::{BuiltinType, TypeError};

            let builtin = match $ty {
                $expr_ty::Builtin(builtin) => builtin,
                _ => return None,
            };

            macro_rules! parse {
                ($kind:ident) => {
                    Some(
                        $number
                            .parse()
                            .map($number_ty::$kind)
                            .map_err(|_| TypeError::InvalidNumericLiteral($ty.clone().into())),
                    )
                };
            }

            match builtin {
                BuiltinType::Number => parse!(Number),
                BuiltinType::Integer => parse!(Integer),
                BuiltinType::Natural => parse!(Natural),
                BuiltinType::Byte => parse!(Byte),
                BuiltinType::Signed => parse!(Signed),
                BuiltinType::Unsigned => parse!(Unsigned),
                BuiltinType::Float => parse!(Float),
                BuiltinType::Double => parse!(Double),
                _ => return None,
            }
        })()
    };
}
