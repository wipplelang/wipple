use crate::*;
use itertools::Itertools;
use lazy_static::lazy_static;
use num_traits::pow::Pow;
use std::collections::HashMap;

pub(crate) fn call(
    interpreter: &Interpreter,
    identifier: &str,
    inputs: Vec<Rc<Value>>,
    info: &Info,
) -> Result<Rc<Value>, Diverge> {
    let builtin = BUILTINS.get(identifier).ok_or_else(|| {
        Diverge::new(
            info.stack.clone(),
            DivergeKind::Error(format!("unknown builtin function '{}'", identifier)),
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
                                    .expect("wrong number of inputs to builtin function"),
                                info
                            )
                        });
                )*

                builtins
            }};
        }

        builtins! {
            "show" => builtin_show,
            "format" => builtin_format,
            "number-to-text" => builtin_number_to_text,
            "add" => builtin_add,
            "subtract" => builtin_subtract,
            "multiply" => builtin_multiply,
            "divide" => builtin_divide,
            "power" => builtin_power,
            "number-equality" => builtin_number_equality,
            "text-equality" => builtin_text_equality,
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
                info.stack.clone(),
                DivergeKind::Error(Error::from("output not configured")),
            )
        })?
        .borrow_mut()(text, &info.stack);

    Ok(Rc::new(Value::Marker))
}

fn builtin_format(
    _: &Interpreter,
    (text, inputs): (Rc<Value>, Rc<Value>),
    _: &Info,
) -> Result<Rc<Value>, Diverge> {
    let text = match text.as_ref() {
        Value::Text(text) => text,
        _ => unreachable!(),
    };

    let inputs = match inputs.as_ref() {
        Value::List(list) => list
            .iter()
            .map(|input| match input.as_ref() {
                Value::Text(text) => text.as_str(),
                _ => unreachable!(),
            })
            .collect::<Vec<_>>(),
        _ => unreachable!(),
    };

    let formatted = if text.is_empty() {
        String::new()
    } else {
        let mut text = text.split('_').collect::<Vec<_>>();
        let last = text.pop().unwrap();

        text.into_iter()
            .zip(inputs)
            .map(|(part, value)| part.to_string() + value)
            .chain(std::iter::once(last.to_string()))
            .collect()
    };

    Ok(Rc::new(Value::Text(formatted)))
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

macro_rules! builtin_math {
    ($($f:ident => $op:tt),* $(,)?) => {
        $(
            fn $f(
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

                Ok(Rc::new(Value::Number(lhs $op rhs)))
            }
        )*
    };
}

builtin_math!(
    builtin_add => +,
    builtin_subtract => -,
    builtin_multiply => *,
);

fn builtin_divide(
    _: &Interpreter,
    (lhs, rhs): (Rc<Value>, Rc<Value>),
    info: &Info,
) -> Result<Rc<Value>, Diverge> {
    let lhs = match lhs.as_ref() {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs.as_ref() {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    if rhs.is_zero() {
        return Err(Diverge::new(
            info.stack.clone(),
            DivergeKind::Error(String::from("division by zero is undefined")),
        ));
    }

    Ok(Rc::new(Value::Number(lhs / rhs)))
}

fn builtin_power(
    _: &Interpreter,
    (lhs, rhs): (Rc<Value>, Rc<Value>),
    info: &Info,
) -> Result<Rc<Value>, Diverge> {
    let lhs = match lhs.as_ref() {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs.as_ref() {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    if lhs.is_zero() && rhs.is_zero() {
        return Err(Diverge::new(
            info.stack.clone(),
            DivergeKind::Error(String::from(
                "raising zero to the power of zero is undefined",
            )),
        ));
    }

    Ok(Rc::new(Value::Number(lhs.pow(*rhs))))
}

fn builtin_number_equality(
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

    let index = if lhs == rhs { 1 } else { 0 };

    Ok(Rc::new(Value::Variant(index, Vec::new())))
}

fn builtin_text_equality(
    _: &Interpreter,
    (lhs, rhs): (Rc<Value>, Rc<Value>),
    _: &Info,
) -> Result<Rc<Value>, Diverge> {
    let lhs = match lhs.as_ref() {
        Value::Text(text) => text,
        _ => unreachable!(),
    };

    let rhs = match rhs.as_ref() {
        Value::Text(text) => text,
        _ => unreachable!(),
    };

    let index = if lhs == rhs { 1 } else { 0 };

    Ok(Rc::new(Value::Variant(index, Vec::new())))
}
