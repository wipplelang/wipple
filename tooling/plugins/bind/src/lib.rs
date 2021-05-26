mod conversions;

pub use conversions::*;

#[macro_export]
macro_rules! bind {
    (fn $func:ident()) => {
        bind!(fn $func() -> ());
    };

    (fn $func:ident() -> $returntype:ty) => {
        |env, stack| {
            #[allow(unused_unsafe)]
            let result: $returntype = unsafe { $func() };
            AsValue::as_value(&result, env, stack)
        }
    };

    (fn $func:ident($($args:ty),* $(,)?)) => {
        bind!(fn $func($($args),*) -> ())
    };

    (fn $func:ident($arg:ty $(, $restargs:ty)* $(,)?) -> $returntype:ty) => {
        ::wipple::Value::of(::wipple_stdlib::Function::new(move |value, env, stack| {
            let arg: $arg = FromValue::from_value(value, env, stack)?;
            bind!(@expand fn $func($($restargs),*) -> $returntype, [arg], env, stack)
        }))
    };

    (@expand fn $func:ident($arg:ty $(, $restargs:ty)*) -> $returntype:ty, [$($var:ident),+], $env:expr, $stack:expr) => {
        Ok(::wipple::Value::of(::wipple_stdlib::Function::new(move |value, env, stack| {
            let arg: $arg = FromValue::from_value(value, env, stack)?;
            bind!(@expand fn $func($($restargs),*) -> $returntype, [$($var),+, arg], env, stack)
        })))
    };

    (@expand fn $func:ident() -> $returntype:ty, [$($var:ident),+], $env:expr, $stack:expr) => {{
        #[allow(unused_unsafe)]
        let result: $returntype = unsafe { $func($($var),+) };
        IntoValue::into_value(result, $env, $stack)
    }};
}

#[macro_export]
macro_rules! define_bindings {
    ($env:expr, { $($($name:expr =>)? fn $func:ident($($arg:ty),* $(,)?) $(-> $returntype:ty)?);*; }) => {{
        $(define_bindings!(@define $env, $($name =>)? fn $func($($arg),*) $(-> $returntype)?);)*
    }};

    (@define $env:expr, fn $func:ident($($arg:ty),* $(,)?) $(-> $returntype:ty)?) => {
        define_bindings!(@define $env, stringify!($func) => fn $func($($arg),*) $(-> $returntype)?)
    };

    (@define $env:expr, $name:expr => fn $func:ident() $(-> $returntype:ty)?) => {
        $env.set_computed_variable($name, bind!(fn $func() $(-> $returntype)?))
    };

    (@define $env:expr, $name:expr => fn $func:ident($($arg:ty),+ $(,)?) $(-> $returntype:ty)?) => {
        $env.set_variable($name, bind!(fn $func($($arg),+) $(-> $returntype)?))
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use wipple::*;
    use wipple_stdlib::*;

    #[test]
    fn typecheck() {
        fn add(a: i32, b: i32) -> i32 {
            a + b
        }

        let env = Env::global();

        env.set_variable("add", bind!(fn add(i32, i32) -> i32));
    }
}
