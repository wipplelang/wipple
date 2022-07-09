use crate::*;
use itertools::Itertools;
use lazy_static::lazy_static;
use num_integer::Roots;
use num_traits::{pow::Pow, FromPrimitive, ToPrimitive, Zero};
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
            "make-integer" => builtin_make_integer,
            "make-positive" => builtin_make_positive,
            "number-to-text" => builtin_number_to_text,
            "integer-to-number" => builtin_integer_to_number,
            "positive-to-number" => builtin_positive_to_number,
            "add-numbers" => builtin_add_numbers,
            "subtract-numbers" => builtin_subtract_numbers,
            "multiply-numbers" => builtin_multiply_numbers,
            "divide-numbers" => builtin_divide_numbers,
            "power-numbers" => builtin_power_numbers,
            "floor-number" => builtin_floor_number,
            "ceil-number" => builtin_ceil_number,
            "sqrt-number" => builtin_sqrt_number,
            "add-integers" => builtin_add_integers,
            "subtract-integers" => builtin_subtract_integers,
            "multiply-integers" => builtin_multiply_integers,
            "divide-integers" => builtin_divide_integers,
            "power-integer-positive" => builtin_power_integer_positive,
            "sqrt-integer" => builtin_sqrt_integer,
            "add-positives" => builtin_add_positives,
            "subtract-positives" => builtin_subtract_positives,
            "multiply-positives" => builtin_multiply_positives,
            "divide-positives" => builtin_divide_positives,
            "power-positives" => builtin_power_positives,
            "sqrt-positive" => builtin_sqrt_positive,
            "text-equality" => builtin_text_equality,
            "number-ordering" => builtin_number_ordering,
            "make-mutable" => builtin_make_mutable,
            "get-mutable" => builtin_get_mutable,
            "set-mutable" => builtin_set_mutable,
            "loop" => builtin_loop,
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
    Value::Variant(0, Vec::new().into_boxed_slice())
}

fn r#true() -> Value {
    Value::Variant(1, Vec::new().into_boxed_slice())
}

fn none() -> Value {
    Value::Variant(0, Vec::new().into_boxed_slice())
}

fn some(value: Value) -> Value {
    Value::Variant(1, vec![value].into_boxed_slice())
}

fn ok(value: Value) -> Value {
    Value::Variant(0, vec![value].into_boxed_slice())
}

fn error(value: Value) -> Value {
    Value::Variant(1, vec![value].into_boxed_slice())
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

fn builtin_make_integer(_: &Interpreter, (number,): (Value,), _: &Info) -> Result<Value, Diverge> {
    let number = match number {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    Ok(match number.to_i64() {
        Some(n) => some(Value::Integer(n)),
        None => none(),
    })
}

fn builtin_make_positive(_: &Interpreter, (number,): (Value,), _: &Info) -> Result<Value, Diverge> {
    let number = match number {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    Ok(match number.to_u64() {
        Some(n) => some(Value::Positive(n)),
        None => none(),
    })
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

fn builtin_integer_to_number(
    _: &Interpreter,
    (integer,): (Value,),
    _: &Info,
) -> Result<Value, Diverge> {
    let integer = match integer {
        Value::Integer(integer) => integer,
        _ => unreachable!(),
    };

    Ok(Value::Number(Decimal::from_i64(integer).unwrap()))
}

fn builtin_positive_to_number(
    _: &Interpreter,
    (positive,): (Value,),
    _: &Info,
) -> Result<Value, Diverge> {
    let positive = match positive {
        Value::Positive(positive) => positive,
        _ => unreachable!(),
    };

    Ok(Value::Number(Decimal::from_u64(positive).unwrap()))
}

fn builtin_add_numbers(
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

    Ok(Value::Number(lhs + rhs))
}

fn builtin_subtract_numbers(
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

    Ok(Value::Number(lhs - rhs))
}

fn builtin_multiply_numbers(
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

    Ok(Value::Number(lhs * rhs))
}

fn builtin_divide_numbers(
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

fn builtin_power_numbers(
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

fn builtin_floor_number(
    _: &Interpreter,
    (value,): (Value,),
    _: &mut Info,
) -> Result<Value, Diverge> {
    let number = match value {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    Ok(Value::Number(number.floor()))
}

fn builtin_ceil_number(
    _: &Interpreter,
    (value,): (Value,),
    _: &mut Info,
) -> Result<Value, Diverge> {
    let number = match value {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    Ok(Value::Number(number.ceil()))
}

fn builtin_sqrt_number(
    _: &Interpreter,
    (value,): (Value,),
    info: &mut Info,
) -> Result<Value, Diverge> {
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
                    "cannot calculate the square root of a negative number",
                )),
            ))
        }
    };

    Ok(Value::Number(sqrt))
}

////////////

fn builtin_add_integers(
    _: &Interpreter,
    (lhs, rhs): (Value, Value),
    _: &Info,
) -> Result<Value, Diverge> {
    let lhs = match lhs {
        Value::Integer(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs {
        Value::Integer(number) => number,
        _ => unreachable!(),
    };

    Ok(Value::Integer(lhs + rhs))
}

fn builtin_subtract_integers(
    _: &Interpreter,
    (lhs, rhs): (Value, Value),
    _: &Info,
) -> Result<Value, Diverge> {
    let lhs = match lhs {
        Value::Integer(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs {
        Value::Integer(number) => number,
        _ => unreachable!(),
    };

    Ok(Value::Integer(lhs - rhs))
}

fn builtin_multiply_integers(
    _: &Interpreter,
    (lhs, rhs): (Value, Value),
    _: &Info,
) -> Result<Value, Diverge> {
    let lhs = match lhs {
        Value::Integer(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs {
        Value::Integer(number) => number,
        _ => unreachable!(),
    };

    Ok(Value::Integer(lhs * rhs))
}

fn builtin_divide_integers(
    _: &Interpreter,
    (lhs, rhs): (Value, Value),
    info: &mut Info,
) -> Result<Value, Diverge> {
    let lhs = match lhs {
        Value::Integer(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs {
        Value::Integer(number) => number,
        _ => unreachable!(),
    };

    if rhs.is_zero() {
        return Err(Diverge::new(
            info.stack.clone(),
            DivergeKind::Error(String::from("division by zero is undefined")),
        ));
    }

    Ok(Value::Integer(lhs / rhs))
}

fn builtin_power_integer_positive(
    _: &Interpreter,
    (lhs, rhs): (Value, Value),
    info: &mut Info,
) -> Result<Value, Diverge> {
    let lhs = match lhs {
        Value::Integer(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs {
        Value::Positive(number) => number,
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

    // FIXME: Remove cast once num-traits supports `i32: Pow<u64>`
    let rhs = rhs as u32;

    Ok(Value::Integer(Pow::pow(lhs, rhs)))
}

fn builtin_sqrt_integer(
    _: &Interpreter,
    (value,): (Value,),
    info: &mut Info,
) -> Result<Value, Diverge> {
    let number = match value {
        Value::Integer(number) => number,
        _ => unreachable!(),
    };

    if number.is_negative() {
        return Err(Diverge::new(
            info.stack.clone(),
            DivergeKind::Error(String::from(
                "cannot calculate the square root of a negative number",
            )),
        ));
    }

    Ok(Value::Integer(number.sqrt()))
}

//////////////

fn builtin_add_positives(
    _: &Interpreter,
    (lhs, rhs): (Value, Value),
    _: &Info,
) -> Result<Value, Diverge> {
    let lhs = match lhs {
        Value::Positive(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs {
        Value::Positive(number) => number,
        _ => unreachable!(),
    };

    Ok(Value::Positive(lhs + rhs))
}

fn builtin_subtract_positives(
    _: &Interpreter,
    (lhs, rhs): (Value, Value),
    _: &Info,
) -> Result<Value, Diverge> {
    let lhs = match lhs {
        Value::Positive(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs {
        Value::Positive(number) => number,
        _ => unreachable!(),
    };

    Ok(Value::Integer((lhs as i64) - (rhs as i64)))
}

fn builtin_multiply_positives(
    _: &Interpreter,
    (lhs, rhs): (Value, Value),
    _: &Info,
) -> Result<Value, Diverge> {
    let lhs = match lhs {
        Value::Positive(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs {
        Value::Positive(number) => number,
        _ => unreachable!(),
    };

    Ok(Value::Positive(lhs * rhs))
}

fn builtin_divide_positives(
    _: &Interpreter,
    (lhs, rhs): (Value, Value),
    info: &mut Info,
) -> Result<Value, Diverge> {
    let lhs = match lhs {
        Value::Positive(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs {
        Value::Positive(number) => number,
        _ => unreachable!(),
    };

    if rhs.is_zero() {
        return Err(Diverge::new(
            info.stack.clone(),
            DivergeKind::Error(String::from("division by zero is undefined")),
        ));
    }

    Ok(Value::Positive(lhs / rhs))
}

fn builtin_power_positives(
    _: &Interpreter,
    (lhs, rhs): (Value, Value),
    info: &mut Info,
) -> Result<Value, Diverge> {
    let lhs = match lhs {
        Value::Positive(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs {
        Value::Positive(number) => number,
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

    // FIXME: Remove cast once num-traits supports `u64: Pow<u64>`
    let rhs = rhs as u32;

    Ok(Value::Positive(Pow::pow(lhs, rhs)))
}

fn builtin_sqrt_positive(
    _: &Interpreter,
    (value,): (Value,),
    _: &mut Info,
) -> Result<Value, Diverge> {
    let number = match value {
        Value::Positive(number) => number,
        _ => unreachable!(),
    };

    Ok(Value::Positive(number.sqrt()))
}

/////////

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

    Ok(Value::Variant(index, Vec::new().into_boxed_slice()))
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
