#[macro_export]
macro_rules! parse_number {
    ($number:ident, $number_ty:ident, $ty:expr, $expr_ty:ident) => {{
        let builtin = match $ty {
            $expr_ty::Builtin(builtin) => builtin,
            x => unreachable!("{:#?}", x),
        };

        macro_rules! parse {
            ($kind:ident) => {
                $number
                    .parse()
                    .map($number_ty::$kind)
                    .map_err(|_| TypeError::InvalidNumericLiteral($ty.clone().into()))
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
            _ => panic!(
                "`parse_number` was provided a non-number type: {:#?}",
                builtin
            ),
        }
    }};
}
