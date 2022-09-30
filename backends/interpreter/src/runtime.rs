use crate::{Error, Interpreter, Value};
use itertools::Itertools;
use lazy_static::lazy_static;
use num_traits::pow::Pow;
use rust_decimal::MathematicalOps;
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

type RuntimeFunction = fn(&Interpreter, Vec<Value>) -> Result<Value, Error>;

fn r#false() -> Value {
    Value::Variant(0, Vec::new())
}

fn r#true() -> Value {
    Value::Variant(1, Vec::new())
}

fn none() -> Value {
    Value::Variant(0, Vec::new())
}

fn some(value: Value) -> Value {
    Value::Variant(1, vec![value])
}

fn ok(value: Value) -> Value {
    Value::Variant(0, vec![value])
}

fn error(value: Value) -> Value {
    Value::Variant(1, vec![value])
}

lazy_static! {
    static ref RUNTIME: HashMap<&'static str, RuntimeFunction> = initialize_runtime();
}

fn initialize_runtime() -> HashMap<&'static str, RuntimeFunction> {
    let mut builtins = HashMap::<&'static str, RuntimeFunction>::default();

    macro_rules! runtime_fn {
        ($name:expr, ($interpreter:pat, $($input:pat),*) => $result:expr) => {
            builtins.insert($name, |$interpreter, inputs| {
                #[allow(unreachable_patterns)]
                match inputs
                    .into_iter()
                    .collect_tuple()
                    .expect("wrong number of inputs to builtin function")
                {
                    ($($input,)*) => $result,
                    _ => unreachable!(),
                }
            });
        };
    }

    macro_rules! runtime_math_fn {
        ($name:expr, Value::$ty:ident, ($($input:pat),*) => $result:expr) => {
            runtime_fn!($name, (_, $(Value::$ty($input)),*) => $result.map(Value::$ty))
        };
        ($name:expr, Value::$ty:ident) => {
            runtime_math_fn!(concat!("add-", $name), Value::$ty, (lhs, rhs) => Ok(lhs + rhs));
            runtime_math_fn!(concat!("subtract-", $name), Value::$ty, (lhs, rhs) => Ok(lhs - rhs));
            runtime_math_fn!(concat!("multiply-", $name), Value::$ty, (lhs, rhs) => Ok(lhs * rhs));
            runtime_math_fn!(concat!("divide-", $name), Value::$ty, (lhs, rhs) => Ok(lhs / rhs));
        }
    }

    macro_rules! runtime_text_fn {
        ($name:literal, ($($input:pat),*) => $result:expr) => {
            runtime_fn!($name, (_, $($input),*) => Ok(Value::Text(Rc::from($result))));
        };
        ($name:literal, Value::$ty:ident) => {
            runtime_text_fn!($name, (Value::$ty(x)) => x.to_string());
        };
    }

    macro_rules! runtime_cmp_fn {
        ($name:literal, ($($input:pat),*) => $result:expr) => {
            runtime_fn!($name, (_, $($input),*) => {
                let index = match $result {
                    std::cmp::Ordering::Less => 0,
                    std::cmp::Ordering::Equal => 1,
                    std::cmp::Ordering::Greater => 2,
                };

                Ok(Value::Variant(index, Vec::new()))
            });
        };
        ($name:literal, Value::$ty:ident) => {
            runtime_cmp_fn!($name, (Value::$ty(lhs), Value::$ty(rhs)) => lhs.cmp(&rhs));
        };
    }

    macro_rules! runtime_eq_fn {
        ($name:expr, Value::$ty:ident) => {
            runtime_fn!($name, (_, Value::$ty(lhs), Value::$ty(rhs)) => {
                Ok(if lhs == rhs { r#true() } else { r#false() })
            });
        };
    }

    runtime_fn!("crash", (_, Value::Text(text)) => Err(Error::from(text.as_ref())));

    runtime_fn!("write-stdout", (interpreter, Value::Text(text)) => {
        interpreter
            .output
            .as_ref()
            .ok_or_else(|| Error::from("output not configured"))?
            .borrow_mut()(&text);

        Ok(Value::Tuple(Vec::new()))
    });

    runtime_fn!("format", (_, Value::Text(text), Value::List(list)) => {
        let inputs = list
            .into_iter()
            .map(|input| match input {
                Value::Text(text) => text,
                _ => unreachable!(),
            })
            .collect::<Vec<_>>();

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
    });

    runtime_text_fn!("number-to-text", (Value::Number(n)) => n.normalize().to_string());
    runtime_text_fn!("integer-to-text", Value::Integer);
    runtime_text_fn!("natural-to-text", Value::Natural);
    runtime_text_fn!("byte-to-text", Value::Byte);
    runtime_text_fn!("signed-to-text", Value::Signed);
    runtime_text_fn!("unsigned-to-text", Value::Unsigned);
    runtime_text_fn!("float-to-text", Value::Float);
    runtime_text_fn!("double-to-text", Value::Double);

    runtime_math_fn!("number", Value::Number);
    runtime_math_fn!("power-number", Value::Number, (lhs, rhs) => Ok(lhs.pow(rhs)));
    runtime_math_fn!("floor-number", Value::Number, (n) => Ok(n.floor()));
    runtime_math_fn!("ceil-number", Value::Number, (n) => Ok(n.ceil()));
    runtime_math_fn!("sqrt-number", Value::Number, (n) => Ok(n.sqrt().unwrap()));

    runtime_math_fn!("integer", Value::Integer);
    runtime_math_fn!("power-integer", Value::Integer, (lhs, rhs) => Ok(lhs.pow(rhs as u32)));

    runtime_math_fn!("natural", Value::Natural);
    runtime_math_fn!("power-natural", Value::Natural, (lhs, rhs) => Ok(lhs.pow(rhs as u32)));

    runtime_math_fn!("byte", Value::Byte);
    runtime_math_fn!("power-byte", Value::Byte, (lhs, rhs) => Ok(lhs.pow(rhs as u32)));

    runtime_math_fn!("signed", Value::Signed);
    runtime_math_fn!("power-signed", Value::Signed, (lhs, rhs) => Ok(lhs.pow(rhs as u32)));

    runtime_math_fn!("unsigned", Value::Unsigned);
    runtime_math_fn!("power-unsigned", Value::Unsigned, (lhs, rhs) => Ok(lhs.pow(rhs)));

    runtime_math_fn!("float", Value::Float);
    runtime_math_fn!("power-float", Value::Float, (lhs, rhs) => Ok(lhs.pow(rhs)));
    runtime_math_fn!("floor-float", Value::Float, (n) => Ok(n.floor()));
    runtime_math_fn!("ceil-float", Value::Float, (n) => Ok(n.ceil()));
    runtime_math_fn!("sqrt-float", Value::Float, (n) => Ok(n.sqrt()));

    runtime_math_fn!("double", Value::Double);
    runtime_math_fn!("power-double", Value::Double, (lhs, rhs) => Ok(lhs.pow(rhs)));
    runtime_math_fn!("floor-double", Value::Double, (n) => Ok(n.floor()));
    runtime_math_fn!("ceil-double", Value::Double, (n) => Ok(n.ceil()));
    runtime_math_fn!("sqrt-double", Value::Double, (n) => Ok(n.sqrt()));

    runtime_eq_fn!("text-equality", Value::Text);
    runtime_eq_fn!("number-equality", Value::Number);
    runtime_eq_fn!("integer-equality", Value::Integer);
    runtime_eq_fn!("natural-equality", Value::Natural);
    runtime_eq_fn!("byte-equality", Value::Byte);
    runtime_eq_fn!("signed-equality", Value::Signed);
    runtime_eq_fn!("unsigned-equality", Value::Unsigned);
    runtime_eq_fn!("float-equality", Value::Float);
    runtime_eq_fn!("double-equality", Value::Double);

    runtime_cmp_fn!("text-ordering", Value::Text);
    runtime_cmp_fn!("number-ordering", Value::Number);
    runtime_cmp_fn!("integer-ordering", Value::Integer);
    runtime_cmp_fn!("natural-ordering", Value::Natural);
    runtime_cmp_fn!("byte-ordering", Value::Byte);
    runtime_cmp_fn!("signed-ordering", Value::Signed);
    runtime_cmp_fn!("unsigned-ordering", Value::Unsigned);
    runtime_cmp_fn!("float-ordering", (Value::Float(lhs), Value::Float(rhs)) => {
        lhs.partial_cmp(&rhs).expect("unexpected NaN")
    });
    runtime_cmp_fn!("double-ordering", (Value::Double(lhs), Value::Double(rhs)) => {
        lhs.partial_cmp(&rhs).expect("unexpected NaN")
    });

    runtime_fn!("make-mutable", (_, value) => {
        Ok(Value::Mutable(Rc::new(RefCell::new(value))))
    });

    runtime_fn!("get-mutable", (_, Value::Mutable(value)) => Ok(value.borrow().clone()));

    runtime_fn!("set-mutable", (_, Value::Mutable(value), new_value) => {
        *value.borrow_mut() = new_value;
        Ok(Value::Tuple(Vec::new()))
    });

    runtime_fn!("make-list", (_, Value::Tuple(tuple)) => Ok(Value::List(tuple.into())));

    runtime_fn!("list-first", (_, Value::List(list)) => {
        Ok(match list.front() {
            Some(first) => some(first.clone()),
            None => none(),
        })
    });

    runtime_fn!("list-last", (_, Value::List(list)) => {
        Ok(match list.back() {
            Some(last) => some(last.clone()),
            None => none(),
        })
    });

    runtime_fn!("list-initial", (_, Value::List(mut list)) => {
        Ok(if list.is_empty() {
            none()
        } else {
            some(Value::List(list.slice(0..(list.len() - 1))))
        })
    });

    runtime_fn!("list-tail", (_, Value::List(mut list)) => {
        Ok(if list.is_empty() {
            none()
        } else {
            some(Value::List(list.slice(1..list.len())))
        })
    });

    runtime_fn!("list-nth", (_, Value::List(list), Value::Natural(index)) => {
        let index = index as usize;
        let index = if (0..list.len()).contains(&index) {
            index
        } else {
            return Ok(error(Value::Marker));
        };

        Ok(match list.get(index) {
            Some(value) => ok(value.clone()),
            None => error(Value::Marker),
        })
    });

    runtime_fn!("list-append", (_, Value::List(mut list), value) => {
        list.push_back(value);
        Ok(Value::List(list))
    });

    runtime_fn!("list-prepend", (_, Value::List(mut list), value) => {
        list.push_front(value);
        Ok(Value::List(list))
    });

    runtime_fn!("list-insert", (_, Value::List(mut list), Value::Natural(index), value) => {
        let index = index as usize;
        let index = if (0..list.len()).contains(&index) {
            index
        } else {
            return Ok(error(Value::Marker));
        };

        list.insert(index, value);

        Ok(ok(Value::List(list)))
    });

    runtime_fn!("list-remove", (_, Value::List(mut list), Value::Natural(index)) => {
        let index = index as usize;
        let index = if (0..list.len()).contains(&index) {
            index
        } else {
            return Ok(error(Value::Marker));
        };

        Ok(ok(list.remove(index)))
    });

    builtins
}
