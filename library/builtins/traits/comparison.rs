use crate::*;
use std::ops::*;
use wipple::*;

macro_rules! define {
    ($($name:ident => $trait:expr);*;) => {
        $(paste! {
            stored_closure!(struct [<$name:camel Fn>](Value, &Env, &Stack) -> Result<Value>);

            impl Primitive for [<$name:camel Fn>] {}

            #[ext(pub, name = [<Value $name Ext>])]
            impl Value {
                fn $name(&self, value: Value, env: &Env, stack: &Stack) -> Result<Value> {
                    let operation = self.get_or::<[<$name:camel Fn>]>(
                        &format!(
                            "Cannot compare this value because it does not have the {} trait",
                            $trait,
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
    ($env:expr, $stack:expr, $name:ident, $operation:tt $(,)?) => {
        paste! {
            $env.set_variable($stack, stringify!($name:camel), Value::of(Trait::of::<[<$name:camel Fn>]>()))?;

            $env.add_relation_between($stack, |number: Number| {
                [<$name:camel Fn>]::new(move |other, env, stack| {
                    let other = other.evaluate(env, stack)?;
                    let other = other.get_or::<Number>("Expected number", env, stack)?;

                    let result = number.number $operation other.number;

                    Ok(Value::of(Variant::condition(result)))
                })
            })?;
        }
    };
}

define! {
    equal_to => "Equal";
    less_than => "Less-Than";
}

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    setup!(env, stack, equal_to, ==);
    setup!(env, stack, less_than, <);

    Ok(())
}
