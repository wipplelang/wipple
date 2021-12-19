use std::collections::HashMap;

use crate::*;
use itertools::Itertools;
use lazy_static::lazy_static;

pub fn call(identifier: &str, inputs: Vec<Arc<Value>>) -> Result<Arc<Value>, Diverge> {
    let builtin = BUILTINS
        .get(identifier)
        .ok_or_else(|| Diverge::Error(Error::from("Unknown builtin function")))?;

    builtin(inputs)
}

type BuiltinFunction = fn(Vec<Arc<Value>>) -> Result<Arc<Value>, Diverge>;

lazy_static! {
    static ref BUILTINS: HashMap<InternedString, BuiltinFunction> = {
        macro_rules! builtins {
            ($($name:expr => $f:expr,)*) => {{
                let mut variables = HashMap::<InternedString, BuiltinFunction>::default();

                $({
                    let name = InternedString::new($name);
                    variables.insert(name, |inputs: Vec<Arc<Value>>| $f(inputs.into_iter().collect_tuple().expect("Wrong number of inputs to builtin function")));
                })*

                variables
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
