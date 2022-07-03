use crate::*;
use itertools::Itertools;
use lazy_static::lazy_static;
use num_traits::{pow::Pow, ToPrimitive};
use rust_decimal::MathematicalOps;
use std::collections::HashMap;

pub(crate) fn call(
    interpreter: &Interpreter,
    identifier: &str,
    inputs: Vec<Value>,
    info: &mut Info,
) -> Result<Value, Diverge> {
    let builtin = BUILTINS.get(identifier).ok_or_else(|| {
        Diverge::new(
            info.stack.clone(),
            DivergeKind::Error(format!("unknown builtin function '{}'", identifier)),
        )
    })?;

    builtin(interpreter, inputs, info)
}

type BuiltinFunction = fn(&Interpreter, Vec<Value>, &mut Info) -> Result<Value, Diverge>;

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
            "make-list" => builtin_make_list,
            "list-first" => builtin_list_first,
            "list-last" => builtin_list_last,
            "list-initial" => builtin_list_initial,
            "list-tail" => builtin_list_tail,
            "list-at" => builtin_list_at,
            "list-append" => builtin_list_append,
            "list-insert" => builtin_list_insert,
            "list-remove" => builtin_list_remove,
        }
    };
}

fn r#false() -> Value {
    Value::Variant(0, im::Vector::new())
}

fn r#true() -> Value {
    Value::Variant(1, im::Vector::new())
}

fn none() -> Value {
    Value::Variant(0, im::Vector::new())
}

fn some(value: Value) -> Value {
    Value::Variant(1, im::vector![value])
}

fn ok(value: Value) -> Value {
    Value::Variant(0, im::vector![value])
}

fn error(value: Value) -> Value {
    Value::Variant(1, im::vector![value])
}

fn builtin_crash(_: &Interpreter, (text,): (Value,), info: &mut Info) -> Result<Value, Diverge> {
    let text = match text {
        Value::Text(text) => text,
        _ => unreachable!(),
    };

    Err(Diverge::new(
        info.stack.clone(),
        DivergeKind::Error(text.to_string()),
    ))
}

fn builtin_print(
    interpreter: &Interpreter,
    (text,): (Value,),
    info: &mut Info,
) -> Result<Value, Diverge> {
    let text = match text {
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
        .borrow_mut()(&text, &info.stack);

    Ok(Value::Marker)
}

fn builtin_format(
    _: &Interpreter,
    (text, inputs): (Value, Value),
    _: &Info,
) -> Result<Value, Diverge> {
    let text = match text {
        Value::Text(text) => text,
        _ => unreachable!(),
    };

    let inputs = match inputs {
        Value::List(list) => list
            .into_iter()
            .map(|input| match input {
                Value::Text(text) => text,
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
            .map(|(part, value)| part.to_string() + value.as_ref())
            .chain(std::iter::once(last.to_string()))
            .collect()
    };

    Ok(Value::Text(Rc::from(formatted)))
}

fn builtin_number_to_text(
    _: &Interpreter,
    (number,): (Value,),
    _: &Info,
) -> Result<Value, Diverge> {
    let number = match number {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    Ok(Value::Text(Rc::from(number.to_string())))
}

macro_rules! builtin_math {
    ($($f:ident => $op:tt),* $(,)?) => {
        $(
            fn $f(
                _: &Interpreter,
                (lhs, rhs): (Value, Value),
                _: &Info,
            ) -> Result<Value, Diverge> {
                let lhs = match lhs {
                    Value::Number(number) => number,
                    _ => unreachable!(),
                };

                let rhs = match rhs {
                    Value::Number(number) => number,
                    _ => unreachable!(),
                };

                Ok(Value::Number(lhs $op rhs))
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
    (lhs, rhs): (Value, Value),
    info: &mut Info,
) -> Result<Value, Diverge> {
    let lhs = match lhs {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    if rhs.is_zero() {
        return Err(Diverge::new(
            info.stack.clone(),
            DivergeKind::Error(String::from("division by zero is undefined")),
        ));
    }

    Ok(Value::Number(lhs / rhs))
}

fn builtin_power(
    _: &Interpreter,
    (lhs, rhs): (Value, Value),
    info: &mut Info,
) -> Result<Value, Diverge> {
    let lhs = match lhs {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs {
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

    Ok(Value::Number(lhs.pow(rhs)))
}

fn builtin_floor(_: &Interpreter, (value,): (Value,), _: &mut Info) -> Result<Value, Diverge> {
    let number = match value {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    Ok(Value::Number(number.floor()))
}

fn builtin_ceil(_: &Interpreter, (value,): (Value,), _: &mut Info) -> Result<Value, Diverge> {
    let number = match value {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    Ok(Value::Number(number.ceil()))
}

fn builtin_sqrt(_: &Interpreter, (value,): (Value,), info: &mut Info) -> Result<Value, Diverge> {
    let number = match value {
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

    Ok(Value::Number(sqrt))
}

fn builtin_text_equality(
    _: &Interpreter,
    (lhs, rhs): (Value, Value),
    _: &Info,
) -> Result<Value, Diverge> {
    let lhs = match lhs {
        Value::Text(text) => text,
        _ => unreachable!(),
    };

    let rhs = match rhs {
        Value::Text(text) => text,
        _ => unreachable!(),
    };

    Ok(if lhs == rhs { r#true() } else { r#false() })
}

fn builtin_number_ordering(
    _: &Interpreter,
    (lhs, rhs): (Value, Value),
    _: &Info,
) -> Result<Value, Diverge> {
    let lhs = match lhs {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    let index = match lhs.cmp(&rhs) {
        std::cmp::Ordering::Less => 0,
        std::cmp::Ordering::Equal => 1,
        std::cmp::Ordering::Greater => 2,
    };

    Ok(Value::Variant(index, im::Vector::new()))
}

fn builtin_make_mutable(_: &Interpreter, (value,): (Value,), _: &Info) -> Result<Value, Diverge> {
    Ok(Value::Mutable(Rc::new(RefCell::new(value))))
}

fn builtin_get_mutable(_: &Interpreter, (value,): (Value,), _: &Info) -> Result<Value, Diverge> {
    let value = match value {
        Value::Mutable(value) => value.borrow().clone(),
        _ => unreachable!(),
    };

    Ok(value)
}

fn builtin_set_mutable(
    _: &Interpreter,
    (value, new_value): (Value, Value),
    _: &Info,
) -> Result<Value, Diverge> {
    let value = match value {
        Value::Mutable(value) => value,
        _ => unreachable!(),
    };

    *value.borrow_mut() = new_value;

    Ok(Value::Marker)
}

fn builtin_loop(
    interpreter: &Interpreter,
    (func,): (Value,),
    info: &mut Info,
) -> Result<Value, Diverge> {
    let function = match func {
        Value::Function(function) => function,
        _ => unreachable!(),
    };

    Ok(loop {
        match interpreter.call_function(function.as_ref(), Value::Marker, info)? {
            Value::Variant(0, _) => continue,
            Value::Variant(1, values) => break values[0].clone(),
            _ => unreachable!(),
        }
    })
}

fn builtin_make_list(_: &Interpreter, (tuple,): (Value,), _: &Info) -> Result<Value, Diverge> {
    // This is OK because tuples and lists have the same representation in the
    // interpreter
    Ok(tuple)
}

fn builtin_list_first(_: &Interpreter, (list,): (Value,), _: &Info) -> Result<Value, Diverge> {
    let list = match list {
        Value::List(list) => list,
        _ => unreachable!(),
    };

    Ok(match list.front() {
        None => none(),
        Some(first) => some(first.clone()),
    })
}

fn builtin_list_last(_: &Interpreter, (list,): (Value,), _: &Info) -> Result<Value, Diverge> {
    let list = match list {
        Value::List(list) => list,
        _ => unreachable!(),
    };

    Ok(match list.back() {
        None => none(),
        Some(last) => some(last.clone()),
    })
}

fn builtin_list_initial(_: &Interpreter, (list,): (Value,), _: &Info) -> Result<Value, Diverge> {
    let mut list = match list {
        Value::List(list) => list,
        _ => unreachable!(),
    };

    Ok(if list.is_empty() {
        none()
    } else {
        some(Value::List(list.slice(0..(list.len() - 1))))
    })
}

fn builtin_list_tail(_: &Interpreter, (list,): (Value,), _: &Info) -> Result<Value, Diverge> {
    let mut list = match list {
        Value::List(list) => list,
        _ => unreachable!(),
    };

    Ok(if list.is_empty() {
        none()
    } else {
        some(Value::List(list.slice(1..list.len())))
    })
}

fn builtin_list_at(
    _: &Interpreter,
    (list, index): (Value, Value),
    _: &Info,
) -> Result<Value, Diverge> {
    let list = match list {
        Value::List(list) => list,
        _ => unreachable!(),
    };

    let index = match index {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    let index = match index.to_usize() {
        Some(index) => index,
        None => return Ok(error(Value::Marker)),
    };

    Ok(match list.get(index) {
        Some(value) => ok(value.clone()),
        None => error(Value::Marker),
    })
}

fn builtin_list_append(
    _: &Interpreter,
    (list, value): (Value, Value),
    _: &Info,
) -> Result<Value, Diverge> {
    let mut list = match list {
        Value::List(list) => list,
        _ => unreachable!(),
    };

    list.push_back(value);

    Ok(Value::List(list))
}

fn builtin_list_insert(
    _: &Interpreter,
    (list, index, value): (Value, Value, Value),
    _: &Info,
) -> Result<Value, Diverge> {
    let mut list = match list {
        Value::List(list) => list,
        _ => unreachable!(),
    };

    let index = match index {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    let index = match index.to_usize() {
        Some(index) if !(0..list.len()).contains(&index) => index,
        _ => return Ok(error(Value::Marker)),
    };

    list.insert(index, value);

    Ok(ok(Value::List(list)))
}

fn builtin_list_remove(
    _: &Interpreter,
    (list, index): (Value, Value),
    _: &Info,
) -> Result<Value, Diverge> {
    let mut list = match list {
        Value::List(list) => list,
        _ => unreachable!(),
    };

    let index = match index {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    let index = match index.to_usize() {
        Some(index) if !(0..list.len()).contains(&index) => index,
        _ => return Ok(error(Value::Marker)),
    };

    Ok(ok(list.remove(index)))
}
