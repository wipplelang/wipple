use crate::*;
use itertools::Itertools;
use lazy_static::lazy_static;
use num_traits::{pow::Pow, ToPrimitive};
use rust_decimal::MathematicalOps;
use std::collections::HashMap;

pub(crate) fn call(
    interpreter: &Interpreter,
    identifier: &str,
    inputs: Vec<Rc<Value>>,
    info: &mut Info,
) -> Result<Rc<Value>, Diverge> {
    let builtin = BUILTINS.get(identifier).ok_or_else(|| {
        Diverge::new(
            info.stack.clone(),
            DivergeKind::Error(format!("unknown builtin function '{}'", identifier)),
        )
    })?;

    builtin(interpreter, inputs, info)
}

type BuiltinFunction = fn(&Interpreter, Vec<Rc<Value>>, &mut Info) -> Result<Rc<Value>, Diverge>;

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
            "crash" => builtin_crash,
            "print" => builtin_print,
            "format" => builtin_format,
            "number-to-text" => builtin_number_to_text,
            "add" => builtin_add,
            "subtract" => builtin_subtract,
            "multiply" => builtin_multiply,
            "divide" => builtin_divide,
            "power" => builtin_power,
            "text-equality" => builtin_text_equality,
            "number-ordering" => builtin_number_ordering,
            "make-mutable" => builtin_make_mutable,
            "get-mutable" => builtin_get_mutable,
            "set-mutable" => builtin_set_mutable,
            "loop" => builtin_loop,
            "floor" => builtin_floor,
            "ceil" => builtin_ceil,
            "sqrt" => builtin_sqrt,
            "list-first" => builtin_list_first,
            "list-last" => builtin_list_last,
            "list-initial" => builtin_list_initial,
            "list-tail" => builtin_list_tail,
            "list-at" => builtin_list_at,
        }
    };
}

fn builtin_crash(
    _: &Interpreter,
    (text,): (Rc<Value>,),
    info: &mut Info,
) -> Result<Rc<Value>, Diverge> {
    let text = match text.as_ref() {
        Value::Text(text) => text,
        _ => unreachable!(),
    };

    Err(Diverge::new(
        info.stack.clone(),
        DivergeKind::Error(text.clone()),
    ))
}

fn builtin_print(
    interpreter: &Interpreter,
    (text,): (Rc<Value>,),
    info: &mut Info,
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
    info: &mut Info,
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
    info: &mut Info,
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

fn builtin_floor(
    _: &Interpreter,
    (value,): (Rc<Value>,),
    _: &mut Info,
) -> Result<Rc<Value>, Diverge> {
    let number = match value.as_ref() {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    Ok(Rc::new(Value::Number(number.floor())))
}

fn builtin_ceil(
    _: &Interpreter,
    (value,): (Rc<Value>,),
    _: &mut Info,
) -> Result<Rc<Value>, Diverge> {
    let number = match value.as_ref() {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    Ok(Rc::new(Value::Number(number.ceil())))
}

fn builtin_sqrt(
    _: &Interpreter,
    (value,): (Rc<Value>,),
    info: &mut Info,
) -> Result<Rc<Value>, Diverge> {
    let number = match value.as_ref() {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    let sqrt = match number.sqrt() {
        Some(sqrt) => sqrt,
        None => {
            return Err(Diverge::new(
                info.stack.clone(),
                DivergeKind::Error(String::from(
                    "the square root of a negative number is undefined",
                )),
            ))
        }
    };

    Ok(Rc::new(Value::Number(sqrt)))
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

fn builtin_number_ordering(
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

    let index = match lhs.cmp(rhs) {
        std::cmp::Ordering::Less => 0,
        std::cmp::Ordering::Equal => 1,
        std::cmp::Ordering::Greater => 2,
    };

    Ok(Rc::new(Value::Variant(index, Vec::new())))
}

fn builtin_make_mutable(
    _: &Interpreter,
    (value,): (Rc<Value>,),
    _: &Info,
) -> Result<Rc<Value>, Diverge> {
    Ok(Rc::new(Value::Mutable(RefCell::new(value))))
}

fn builtin_get_mutable(
    _: &Interpreter,
    (value,): (Rc<Value>,),
    _: &Info,
) -> Result<Rc<Value>, Diverge> {
    let value = match value.as_ref() {
        Value::Mutable(value) => value,
        _ => unreachable!(),
    };

    Ok(value.borrow().clone())
}

fn builtin_set_mutable(
    _: &Interpreter,
    (value, new_value): (Rc<Value>, Rc<Value>),
    _: &Info,
) -> Result<Rc<Value>, Diverge> {
    let value = match value.as_ref() {
        Value::Mutable(value) => value,
        _ => unreachable!(),
    };

    *value.borrow_mut() = new_value;

    Ok(Rc::new(Value::Marker))
}

fn builtin_loop(
    interpreter: &Interpreter,
    (func,): (Rc<Value>,),
    info: &mut Info,
) -> Result<Rc<Value>, Diverge> {
    let (pattern, body, scope) = match func.as_ref() {
        Value::Function {
            pattern,
            body,
            scope,
        } => (pattern, body, scope),
        _ => unreachable!(),
    };

    let input = Rc::new(Value::Marker);

    Ok(loop {
        match interpreter
            .call_function(pattern, body, scope.clone(), input.clone(), info)?
            .as_ref()
        {
            Value::Variant(0, _) => continue,
            Value::Variant(1, values) => break values[0].clone(),
            _ => unreachable!(),
        }
    })
}

fn builtin_list_first(
    _: &Interpreter,
    (list,): (Rc<Value>,),
    _: &Info,
) -> Result<Rc<Value>, Diverge> {
    let list = match list.as_ref() {
        Value::List(list) => list,
        _ => unreachable!(),
    };

    Ok(Rc::new(match list.first() {
        None => Value::Variant(0, Vec::new()),
        Some(first) => Value::Variant(1, vec![first.clone()]),
    }))
}

fn builtin_list_last(
    _: &Interpreter,
    (list,): (Rc<Value>,),
    _: &Info,
) -> Result<Rc<Value>, Diverge> {
    let list = match list.as_ref() {
        Value::List(list) => list,
        _ => unreachable!(),
    };

    Ok(Rc::new(match list.last() {
        None => Value::Variant(0, Vec::new()),
        Some(first) => Value::Variant(1, vec![first.clone()]),
    }))
}

fn builtin_list_initial(
    _: &Interpreter,
    (list,): (Rc<Value>,),
    _: &Info,
) -> Result<Rc<Value>, Diverge> {
    let list = match list.as_ref() {
        Value::List(list) => list,
        _ => unreachable!(),
    };

    Ok(Rc::new(Value::List(list[0..(list.len() - 1)].to_vec())))
}

fn builtin_list_tail(
    _: &Interpreter,
    (list,): (Rc<Value>,),
    _: &Info,
) -> Result<Rc<Value>, Diverge> {
    let list = match list.as_ref() {
        Value::List(list) => list,
        _ => unreachable!(),
    };

    Ok(Rc::new(Value::List(list[1..list.len()].to_vec())))
}

fn builtin_list_at(
    _: &Interpreter,
    (list, index): (Rc<Value>, Rc<Value>),
    info: &Info,
) -> Result<Rc<Value>, Diverge> {
    let list = match list.as_ref() {
        Value::List(list) => list,
        _ => unreachable!(),
    };

    let index = match index.as_ref() {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    let index = match index.to_usize() {
        Some(index) => index,
        None => {
            return Err(Diverge::new(
                info.stack.clone(),
                DivergeKind::Error(String::from("list index must be an integer")),
            ))
        }
    };

    Ok(Rc::new(match list.get(index) {
        None => Value::Variant(0, Vec::new()),
        Some(value) => Value::Variant(1, vec![value.clone()]),
    }))
}
