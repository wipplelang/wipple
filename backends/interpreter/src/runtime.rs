use crate::{Error, Interpreter, Value};
use itertools::Itertools;
use lazy_static::lazy_static;
use paste::paste;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

impl<'a> Interpreter<'a> {
    pub(crate) fn call_runtime(
        &self,
        identifier: &str,
        inputs: Vec<Value>,
    ) -> Result<Value, Error> {
        let f = RUNTIME
            .get(identifier)
            .ok_or_else(|| format!("unknown runtime function '{}'", identifier))?;

        f(self, inputs)
    }
}

type BuiltinFunction = fn(&Interpreter, Vec<Value>) -> Result<Value, Error>;

lazy_static! {
    static ref RUNTIME: HashMap<&'static str, BuiltinFunction> = {
        macro_rules! fns {
            ($($name:literal,)*) => {{
                let mut builtins = HashMap::<&'static str, BuiltinFunction>::default();

                paste! {
                    $(
                        builtins.insert(
                            $name,
                            |interpreter, inputs| {
                                [<$name:snake>](
                                    interpreter,
                                    inputs
                                        .into_iter()
                                        .collect_tuple()
                                        .expect("wrong number of inputs to builtin function"),
                                )
                            });
                    )*
                }

                builtins
            }};
        }

        fns!(
            "crash",
            "write-stdout",
            "format",
            "number-to-text",
            "add",
            "subtract",
            "multiply",
            "divide",
            "power",
            "floor",
            "ceil",
            "sqrt",
            "text-equality",
            "number-equality",
            "number-ordering",
            "make-mutable",
            "get-mutable",
            "set-mutable",
            "make-list",
            "list-first",
            "list-last",
            "list-initial",
            "list-tail",
            "list-at",
            "list-append",
            "list-insert",
            "list-remove",
        )
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

fn crash(_: &Interpreter, (text,): (Value,)) -> Result<Value, Error> {
    let text = match text {
        Value::Text(text) => text,
        _ => unreachable!(),
    };

    Err(Error::from(text.as_ref()))
}

fn write_stdout(interpreter: &Interpreter, (text,): (Value,)) -> Result<Value, Error> {
    let text = match text {
        Value::Text(text) => text,
        _ => unreachable!(),
    };

    interpreter
        .output
        .as_ref()
        .ok_or_else(|| Error::from("output not configured"))?
        .borrow_mut()(&text);

    Ok(Value::Marker)
}

fn format(_: &Interpreter, (text, inputs): (Value, Value)) -> Result<Value, Error> {
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

fn number_to_text(_: &Interpreter, (number,): (Value,)) -> Result<Value, Error> {
    let number = match number {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    Ok(Value::Text(Rc::from(number.to_string())))
}

fn add(_: &Interpreter, (lhs, rhs): (Value, Value)) -> Result<Value, Error> {
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

fn subtract(_: &Interpreter, (lhs, rhs): (Value, Value)) -> Result<Value, Error> {
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

fn multiply(_: &Interpreter, (lhs, rhs): (Value, Value)) -> Result<Value, Error> {
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

fn divide(_: &Interpreter, (lhs, rhs): (Value, Value)) -> Result<Value, Error> {
    let lhs = match lhs {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    if rhs == 0. {
        return Err(Error::from("division by zero is undefined"));
    }

    Ok(Value::Number(lhs / rhs))
}

fn power(_: &Interpreter, (lhs, rhs): (Value, Value)) -> Result<Value, Error> {
    let lhs = match lhs {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    if lhs == 0. && rhs == 0. {
        return Err(Error::from(
            "raising zero to the power of zero is undefined",
        ));
    }

    Ok(Value::Number(lhs.powf(rhs)))
}

fn floor(_: &Interpreter, (value,): (Value,)) -> Result<Value, Error> {
    let number = match value {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    Ok(Value::Number(number.floor()))
}

fn ceil(_: &Interpreter, (value,): (Value,)) -> Result<Value, Error> {
    let number = match value {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    Ok(Value::Number(number.ceil()))
}

fn sqrt(_: &Interpreter, (value,): (Value,)) -> Result<Value, Error> {
    let number = match value {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    if number < 0. {
        return Err(Error::from(
            "cannot calculate the square root of a negative number",
        ));
    }

    Ok(Value::Number(number.sqrt()))
}

fn text_equality(_: &Interpreter, (lhs, rhs): (Value, Value)) -> Result<Value, Error> {
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

fn number_equality(_: &Interpreter, (lhs, rhs): (Value, Value)) -> Result<Value, Error> {
    let lhs = match lhs {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    Ok(if lhs == rhs { r#true() } else { r#false() })
}

fn number_ordering(_: &Interpreter, (lhs, rhs): (Value, Value)) -> Result<Value, Error> {
    let lhs = match lhs {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    let rhs = match rhs {
        Value::Number(number) => number,
        _ => unreachable!(),
    };

    let index = match lhs.partial_cmp(&rhs).expect("unexpected NaN") {
        std::cmp::Ordering::Less => 0,
        std::cmp::Ordering::Equal => 1,
        std::cmp::Ordering::Greater => 2,
    };

    Ok(Value::Variant(index, Vec::new().into_boxed_slice()))
}

fn make_mutable(_: &Interpreter, (value,): (Value,)) -> Result<Value, Error> {
    Ok(Value::Mutable(Rc::new(RefCell::new(value))))
}

fn get_mutable(_: &Interpreter, (value,): (Value,)) -> Result<Value, Error> {
    let value = match value {
        Value::Mutable(value) => value.borrow().clone(),
        _ => unreachable!(),
    };

    Ok(value)
}

fn set_mutable(_: &Interpreter, (value, new_value): (Value, Value)) -> Result<Value, Error> {
    let value = match value {
        Value::Mutable(value) => value,
        _ => unreachable!(),
    };

    *value.borrow_mut() = new_value;

    Ok(Value::Marker)
}

fn make_list(_: &Interpreter, (tuple,): (Value,)) -> Result<Value, Error> {
    // This is OK because tuples and lists have the same representation in the
    // interpreter
    Ok(tuple)
}

fn list_first(_: &Interpreter, (list,): (Value,)) -> Result<Value, Error> {
    let list = match list {
        Value::List(list) => list,
        _ => unreachable!(),
    };

    Ok(match list.front() {
        None => none(),
        Some(first) => some(first.clone()),
    })
}

fn list_last(_: &Interpreter, (list,): (Value,)) -> Result<Value, Error> {
    let list = match list {
        Value::List(list) => list,
        _ => unreachable!(),
    };

    Ok(match list.back() {
        None => none(),
        Some(last) => some(last.clone()),
    })
}

fn list_initial(_: &Interpreter, (list,): (Value,)) -> Result<Value, Error> {
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

fn list_tail(_: &Interpreter, (list,): (Value,)) -> Result<Value, Error> {
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

fn list_at(_: &Interpreter, (list, index): (Value, Value)) -> Result<Value, Error> {
    let list = match list {
        Value::List(list) => list,
        _ => unreachable!(),
    };

    let index = match index {
        Value::Number(number) => number as usize,
        _ => unreachable!(),
    };

    let index = if (0..list.len()).contains(&index) {
        index
    } else {
        return Ok(error(Value::Marker));
    };

    Ok(match list.get(index) {
        Some(value) => ok(value.clone()),
        None => error(Value::Marker),
    })
}

fn list_append(_: &Interpreter, (list, value): (Value, Value)) -> Result<Value, Error> {
    let mut list = match list {
        Value::List(list) => list,
        _ => unreachable!(),
    };

    list.push_back(value);

    Ok(Value::List(list))
}

fn list_insert(
    _: &Interpreter,
    (list, index, value): (Value, Value, Value),
) -> Result<Value, Error> {
    let mut list = match list {
        Value::List(list) => list,
        _ => unreachable!(),
    };

    let index = match index {
        Value::Number(number) => number as usize,
        _ => unreachable!(),
    };

    let index = if (0..list.len()).contains(&index) {
        index
    } else {
        return Ok(error(Value::Marker));
    };

    list.insert(index, value);

    Ok(ok(Value::List(list)))
}

fn list_remove(_: &Interpreter, (list, index): (Value, Value)) -> Result<Value, Error> {
    let mut list = match list {
        Value::List(list) => list,
        _ => unreachable!(),
    };

    let index = match index {
        Value::Number(number) => number as usize,
        _ => unreachable!(),
    };

    let index = if (0..list.len()).contains(&index) {
        index
    } else {
        return Ok(error(Value::Marker));
    };

    Ok(ok(list.remove(index)))
}
