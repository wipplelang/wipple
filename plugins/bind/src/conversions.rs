use wipple::*;

pub trait FromValue
where
    Self: Sized,
{
    fn from_value(value: Value, env: &EnvironmentRef, stack: &Stack) -> Result<Self>;
}

pub trait AsValue {
    fn as_value(&self, env: &EnvironmentRef, stack: &Stack) -> Result;
}

impl FromValue for () {
    fn from_value(value: Value, _: &EnvironmentRef, stack: &Stack) -> Result<Self> {
        if value.is_empty() {
            Ok(())
        } else {
            Err(Return::error("Expected empty value", stack))
        }
    }
}

// Conversions for primitive types

impl AsValue for () {
    fn as_value(&self, _: &EnvironmentRef, _: &Stack) -> Result {
        Ok(Value::empty())
    }
}

impl FromValue for String {
    fn from_value(value: Value, env: &EnvironmentRef, stack: &Stack) -> Result<Self> {
        value
            .get_or::<Text>("Expected text", env, stack)
            .map(|x| x.text)
    }
}

impl AsValue for String {
    fn as_value(&self, _: &EnvironmentRef, _: &Stack) -> Result {
        Ok(Value::of(Text::new(self)))
    }
}

macro_rules! number_conversion {
    ($type:ty, $desc:expr) => {
        impl FromValue for $type {
            fn from_value(value: Value, env: &EnvironmentRef, stack: &Stack) -> Result<Self> {
                let number = value.get_or::<Number>("Expected number", env, stack)?;

                Ok(number.number as $type)
            }
        }

        impl AsValue for $type {
            fn as_value(&self, _: &EnvironmentRef, _: &Stack) -> Result {
                Ok(Value::of(Number::new(*self as f64)))
            }
        }
    };
}

number_conversion!(isize, "a platform-sized integer");
number_conversion!(i8, "an 8-bit integer");
number_conversion!(i16, "a 16-bit integer");
number_conversion!(i32, "a 32-bit integer");
number_conversion!(i64, "a 64-bit integer");
number_conversion!(i128, "a 128-bit integer");
number_conversion!(usize, "a platform-sized unsigned integer");
number_conversion!(u8, "an 8-bit unsigned integer");
number_conversion!(u16, "a 16-bit unsigned integer");
number_conversion!(u32, "a 32-bit unsigned integer");
number_conversion!(u64, "a 64-bit unsigned integer");
number_conversion!(u128, "a 128-bit unsigned integer");
number_conversion!(f32, "a 32-bit floating-point number");
number_conversion!(f64, "a 64-bit floating-point number");
