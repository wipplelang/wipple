use wipple::*;

pub trait FromValue
where
    Self: Sized,
{
    fn from_value(value: Value, env: &Env, stack: &Stack) -> Result<Self>;
}

pub trait IntoValue {
    fn into_value(self, env: &Env, stack: &Stack) -> Result<Value>;
}

#[cfg(feature = "stdlib")]
mod stdlib_conversions {
    use super::*;
    use rust_decimal::prelude::{Decimal, FromPrimitive, ToPrimitive};
    use wipple_stdlib::*;

    impl FromValue for () {
        fn from_value(value: Value, _: &Env, stack: &Stack) -> Result<Self> {
            if value.is_empty() {
                Ok(())
            } else {
                Err(error("Expected empty value", stack))
            }
        }
    }

    impl IntoValue for () {
        fn into_value(self, _: &Env, _: &Stack) -> Result<Value> {
            Ok(Value::empty())
        }
    }

    impl FromValue for String {
        fn from_value(value: Value, env: &Env, stack: &Stack) -> Result<Self> {
            value
                .get_or::<Text>("Expected text", env, stack)
                .map(|x| x.text.clone())
        }
    }

    impl IntoValue for String {
        fn into_value(self, _: &Env, _: &Stack) -> Result<Value> {
            Ok(Value::of(Text::new(self)))
        }
    }

    macro_rules! number_conversions {
        ($($type:ty => $desc:expr);*;) => {
            $(impl FromValue for $type {
                fn from_value(value: Value, env: &Env, stack: &Stack) -> Result<Self> {
                    let number = value.get_or::<Number>("Expected number", env, stack)?;

                    paste!(number.number.[<to_ $type>]()).ok_or_else(|| {
                        error(
                            &format!("Cannot represent this number as {}", $desc),
                            stack
                        )
                    })
                }
            }

            impl IntoValue for $type {
                fn into_value(self, _: &Env, stack: &Stack) -> Result<Value> {
                    paste!(Decimal::[<from _$type>](self))
                        .map(|number| Value::of(Number::new(number)))
                        .ok_or_else(|| error("Cannot represent this number in Wipple", stack))
                }
            })*
        };
    }

    number_conversions! {
        isize => "a platform-sized integer";
        i8 => "an 8-bit integer";
        i16 => "a 16-bit integer";
        i32 => "a 32-bit integer";
        i64 => "a 64-bit integer";
        i128 => "a 128-bit integer";
        usize => "a platform-sized unsigned integer";
        u8 => "an 8-bit unsigned integer";
        u16 => "a 16-bit unsigned integer";
        u32 => "a 32-bit unsigned integer";
        u64 => "a 64-bit unsigned integer";
        u128 => "a 128-bit unsigned integer";
        f32 => "a 32-bit floating-point number";
        f64 => "a 64-bit floating-point number";
    }
}

#[cfg(feature = "stdlib")]
pub use stdlib_conversions::*;
