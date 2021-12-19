use std::collections::HashMap;

use crate::*;
use itertools::Itertools;
use lazy_static::lazy_static;

pub fn call(identifier: &str, inputs: Vec<Arc<Value>>) -> Result<Arc<Value>, Diverge> {
    let builtin = BUILTINS
        .get(identifier)
        .ok_or_else(|| Diverge::Error(format!("Unknown builtin function '{}'", identifier)))?;

    builtin(inputs)
}

type BuiltinFunction = fn(Vec<Arc<Value>>) -> Result<Arc<Value>, Diverge>;

lazy_static! {
    static ref BUILTINS: HashMap<&'static str, BuiltinFunction> = {
        macro_rules! builtins {
            ($($name:expr => $f:expr,)*) => {{
                let mut builtins = HashMap::<&'static str, BuiltinFunction>::default();

                $(
                    builtins.insert(
                        $name,
                        |inputs: Vec<Arc<Value>>| {
                            $f(
                                inputs
                                    .into_iter()
                                    .collect_tuple()
                                    .expect("Wrong number of inputs to builtin function")
                            )
                        });
                )*

                builtins
            }};
        }

        builtins! {
            "show" => builtin_show,
        }
    };
}

fn builtin_show((text,): (Arc<Value>,)) -> Result<Arc<Value>, Diverge> {
    let text = match text.as_ref() {
        Value::Text(text) => text,
        _ => unreachable!(),
    };

    println!("{}", text);

    Ok(Arc::new(Value::Unit))
}
