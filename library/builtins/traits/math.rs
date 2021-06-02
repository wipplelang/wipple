use crate::*;
use rust_decimal::{Decimal, MathematicalOps};
use std::ops::*;
use wipple::*;

macro_rules! define {
    { $($name:ident => $description:expr);*; } => {
        $(paste! {
            stored_closure!(struct [<$name:camel Fn>](Value, &Env, &Stack) -> Result<Value>);

            impl Primitive for [<$name:camel Fn>] {}

            #[ext(pub, name = [<Value $name Ext>])]
            impl Value {
                fn $name(&self, value: Value, env: &Env, stack: &Stack) -> Result<Value> {
                    let operation = self.get_or::<[<$name:camel Fn>]>(
                        &format!(
                            "Cannot {} because it does not have the {} trait",
                            $description,
                            stringify!([<$name:camel>]),
                        ),
                        env,
                        &stack,
                    )?;

                    operation(value, env, stack)
                }
            }
        })*
    };
}

macro_rules! setup {
    ($env:expr, $stack:expr, $name:ident => $operation:ident $(,)?) => {
        setup!($env, $stack, $name, |a: Decimal, b: Decimal, _| {
            Ok(a.$operation(b))
        });
    };
    ($env:expr, $stack:expr, $name:ident, $operation:expr $(,)?) => {
        paste! {
            $env.set_variable($stack, stringify!($name:camel), Value::of(Trait::of::<[<$name:camel Fn>]>()))?;

            $env.add_relation_between($stack, |number: Number| {
                [<$name:camel Fn>]::new(move |other, env, stack| {
                    let other = other.evaluate(env, stack)?;
                    let other = other.get_or::<Number>("Expected number", env, stack)?;

                    #[allow(clippy::redundant_closure_call)]
                    let result = ($operation)(number.number, other.number, stack)?;

                    Ok(Value::of(Number::new(result)))
                })
            })?;
        }
    };
}

define! {
    add => "add this value";
    subtract => "subtract this value";
    multiply => "multiply this value";
    divide => "divide this value";
    modulo => "divide this value with remainder";
    power => "raise this value to a power";
}

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    setup!(env, stack, add => add);
    setup!(env, stack, subtract => sub);
    setup!(env, stack, multiply => mul);
    setup!(env, stack, power => powd);

    setup!(env, stack, divide, |a: Decimal, b: Decimal, stack| {
        if b.is_zero() {
            Err(error("Cannot divide by zero", stack))
        } else {
            Ok(a / b)
        }
    });

    setup!(env, stack, modulo, |a: Decimal, b: Decimal, stack| {
        if b.is_zero() {
            Err(error("Cannot divide with remainder by zero", stack))
        } else {
            Ok(a % b)
        }
    });

    Ok(())
}
