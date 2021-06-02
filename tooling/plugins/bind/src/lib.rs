mod conversions;

pub use conversions::*;

#[macro_export]
macro_rules! bind {
    (fn $func:ident()) => {
        bind!(fn $func() -> ());
    };

    (fn $func:ident() -> $returntype:ty) => {
        ::wipple_stdlib::ComputeFn::new(|env, stack| {
            #[allow(unused_unsafe)]
            let result: $returntype = unsafe { $func() };
            $crate::IntoValue::into_value(result, env, stack)
        })
    };

    (fn $func:ident($($args:ty),* $(,)?)) => {
        bind!(fn $func($($args),*) -> ())
    };

    (fn $func:ident($arg:ty $(, $restargs:ty)* $(,)?) -> $returntype:ty) => {
        ::wipple::Value::of(::wipple_stdlib::Function::new(move |value, env, stack| {
            let arg: $arg = $crate::FromValue::from_value(value, env, stack)?;
            bind!(@expand fn $func($($restargs),*) -> $returntype, [arg], env, stack)
        }))
    };

    (@expand fn $func:ident($arg:ty $(, $restargs:ty)*) -> $returntype:ty, [$($var:ident),+], $env:expr, $stack:expr) => {
        Ok(::wipple::Value::of(::wipple_stdlib::Function::new(move |value, env, stack| {
            let arg: $arg = $crate::FromValue::from_value(value, env, stack)?;
            bind!(@expand fn $func($($restargs),*) -> $returntype, [$($var),+, arg], env, stack)
        })))
    };

    (@expand fn $func:ident() -> $returntype:ty, [$($var:ident),+], $env:expr, $stack:expr) => {{
        #[allow(unused_unsafe)]
        let result: $returntype = unsafe { $func($($var),+) };
        $crate::IntoValue::into_value(result, $env, $stack)
    }};
}

#[macro_export]
macro_rules! define_bindings {
    ($env:expr, $stack:expr, { $($($name:expr =>)? fn $func:ident($($arg:ty),* $(,)?) $(-> $returntype:ty)?);*; }) => {{
        (|| -> ::wipple::Result<()> {
            $(define_bindings!(@define $env, $stack, $($name =>)? fn $func($($arg),*) $(-> $returntype)?)?;)*
            Ok(())
        })()
    }};

    (@define $env:expr, $stack:expr, fn $func:ident($($arg:ty),* $(,)?) $(-> $returntype:ty)?) => {
        define_bindings!(@define $env, $stack, stringify!($func) => fn $func($($arg),*) $(-> $returntype)?)
    };

    (@define $env:expr, $stack:expr, $name:expr => fn $func:ident() $(-> $returntype:ty)?) => {
        $env.set_computed_variable($stack, $name, bind!(fn $func() $(-> $returntype)?))
    };

    (@define $env:expr, $stack:expr, $name:expr => fn $func:ident($($arg:ty),+ $(,)?) $(-> $returntype:ty)?) => {
        $env.set_variable($stack, $name, bind!(fn $func($($arg),+) $(-> $returntype)?))
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use wipple::*;
    use wipple_stdlib::*;

    #[test]
    fn typecheck() -> Result<()> {
        fn f() -> i32 {
            0
        }

        fn g(_: i32) {}

        fn add(a: i32, b: i32) -> i32 {
            a + b
        }

        let env = Env::global();
        let stack = Stack::default();

        define_bindings!(env, &stack, {
            fn add(i32, i32) -> i32;
            fn f() -> i32;
            fn g(i32);
        })?;

        Ok(())
    }
}
