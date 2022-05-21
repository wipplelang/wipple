use crate::*;
use itertools::Itertools;
use lazy_static::lazy_static;
use std::collections::HashMap;

pub(crate) fn call(
    interpreter: &Interpreter,
    identifier: &str,
    inputs: Vec<Rc<Value>>,
    info: &Info,
) -> Result<Rc<Value>, Diverge> {
    let builtin = BUILTINS.get(identifier).ok_or_else(|| {
        Diverge::new(
            &info.stack,
            DivergeKind::Error(format!("Unknown builtin function '{}'", identifier)),
        )
    })?;

    builtin(interpreter, inputs, info)
}

type BuiltinFunction = fn(&Interpreter, Vec<Rc<Value>>, &Info) -> Result<Rc<Value>, Diverge>;

lazy_static! {
    static ref BUILTINS: HashMap<&'static str, BuiltinFunction> = {
        macro_rules! builtins {
            ($($name:expr => $f:expr,)*) => {{
                let mut builtins = HashMap::<&'static str, BuiltinFunction>::default();

                $(
                    builtins.insert(
                        $name,
                        |interpreter, inputs, info| {
                            $f(
                                interpreter,
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
            "number-to-text" => builtin_number_to_text,
            "add-numbers" => builtin_add_numbers,
        }
    };
}

fn builtin_show(
    interpreter: &Interpreter,
    (text,): (Rc<Value>,),
    info: &Info,
) -> Result<Rc<Value>, Diverge> {
    let text = match text.as_ref() {
        Value::Text(text) => text,
        _ => unreachable!(),
    };

    interpreter
        .output
        .as_ref()
        .ok_or_else(|| {
            Diverge::new(
                &info.stack,
                DivergeKind::Error(Error::from("Output not configured")),
            )
        })?
        .borrow_mut()(text, &info.stack);

    Ok(Rc::new(Value::Marker))
}

fn builtin_number_to_text(
    _: &Interpreter,
    (number,): (Rc<Value>,),
    _: &Info,
) -> Result<Rc<Value>, Diverge> {
    let number = match number.as_ref() {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    Ok(Rc::new(Value::Text(number.to_string())))
}

fn builtin_add_numbers(
    _: &Interpreter,
    (lhs, rhs): (Rc<Value>, Rc<Value>),
    _: &Info,
) -> Result<Rc<Value>, Diverge> {
    let lhs = match lhs.as_ref() {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs.as_ref() {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    Ok(Rc::new(Value::Number(lhs + rhs)))
}
