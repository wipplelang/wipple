use crate::*;
use itertools::Itertools;
use lazy_static::lazy_static;
use std::collections::HashMap;

pub fn call(identifier: &str, inputs: Vec<Arc<Value>>, info: &Info) -> Result<Arc<Value>, Diverge> {
    let builtin = BUILTINS.get(identifier).ok_or_else(|| {
        Diverge::new(
            &info.callstack,
            DivergeKind::Error(format!("Unknown builtin function '{}'", identifier)),
        )
    })?;

    builtin(inputs, info)
}

type BuiltinFunction = fn(Vec<Arc<Value>>, &Info) -> Result<Arc<Value>, Diverge>;

lazy_static! {
    static ref BUILTINS: HashMap<&'static str, BuiltinFunction> = {
        macro_rules! builtins {
            ($($name:expr => $f:expr,)*) => {{
                let mut builtins = HashMap::<&'static str, BuiltinFunction>::default();

                $(
                    builtins.insert(
                        $name,
                        |inputs, info| {
                            $f(
                                inputs
                                    .into_iter()
                                    .collect_tuple()
                                    .expect("Wrong number of inputs to builtin function"),
                                info
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

fn builtin_show((text,): (Arc<Value>,), info: &Info) -> Result<Arc<Value>, Diverge> {
    let text = match text.as_ref() {
        Value::Text(text) => text,
        _ => unreachable!(),
    };

    OUTPUT.read().unwrap().as_deref().ok_or_else(|| {
        Diverge::new(
            &info.callstack,
            DivergeKind::Error(Error::from("Output not configured")),
        )
    })?(text);

    Ok(Arc::new(Value::Unit))
}
