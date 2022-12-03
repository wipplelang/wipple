use crate::{Error, Interpreter, Value};
use itertools::Itertools;
use num_traits::pow::Pow;
use rust_decimal::Decimal;
use rust_decimal::MathematicalOps;
use std::{cell::RefCell, rc::Rc};
use wipple_frontend::ir;

fn r#false() -> Value {
    Value::Variant(0, Vec::new().into())
}

fn r#true() -> Value {
    Value::Variant(1, Vec::new().into())
}

fn ok(value: Value) -> Value {
    Value::Variant(0, vec![value].into())
}

fn error(value: Value) -> Value {
    Value::Variant(1, vec![value].into())
}

impl<'a> Interpreter<'a> {
    pub(crate) fn call_runtime(
        &self,
        func: ir::RuntimeFunction,
        inputs: Vec<Value>,
    ) -> Result<Value, Error> {
        #![allow(unreachable_patterns)]

        macro_rules! runtime_fn {
            (($($input:pat),*) => $result:expr) => {
                match inputs
                    .into_iter()
                    .collect_tuple()
                    .expect("wrong number of inputs to builtin function")
                {
                    ($($input,)*) => $result,
                    _ => unreachable!(),
                }
            };
        }

        macro_rules! runtime_math_fn {
            (Value::$ty:ident, ($($input:pat),*) => $result:expr) => {
                runtime_fn!(($(Value::$ty($input)),*) => $result.map(Value::$ty))
            };
        }

        macro_rules! runtime_div_fn {
            (Value::$ty:ident, $zero:expr) => {
                runtime_math_fn!(Value::$ty, (lhs, rhs) => {
                    if rhs != $zero {
                        Ok(lhs / rhs)
                    } else {
                        Err(Error::from("cannot divide by zero"))
                    }
                })
            };
        }

        macro_rules! runtime_text_fn {
            (($($input:pat),*) => $result:expr) => {
                runtime_fn!(($($input),*) => Ok(Value::Text(Rc::from($result))))
            };
            (Value::$ty:ident) => {
                runtime_text_fn!((Value::$ty(x)) => x.to_string())
            };
        }

        macro_rules! runtime_cmp_fn {
            (($($input:pat),*) => $result:expr) => {
                runtime_fn!(($($input),*) => {
                    let index = match $result {
                        std::cmp::Ordering::Less => 0,
                        std::cmp::Ordering::Equal => 1,
                        std::cmp::Ordering::Greater => 2,
                    };

                    Ok(Value::Variant(index, Vec::new().into()))
                })
            };
            (Value::$ty:ident) => {
                runtime_cmp_fn!((Value::$ty(lhs), Value::$ty(rhs)) => lhs.cmp(&rhs))
            };
        }

        macro_rules! runtime_eq_fn {
            (Value::$ty:ident) => {
                runtime_fn!((Value::$ty(lhs), Value::$ty(rhs)) => {
                    Ok(if lhs == rhs { r#true() } else { r#false() })
                })
            };
        }

        match func {
            ir::RuntimeFunction::Crash => {
                runtime_fn!((Value::Text(text)) => Err(Error::from(text.as_ref())))
            }
            ir::RuntimeFunction::WriteStdout => runtime_fn!((Value::Text(text)) => {
                self.output
                    .as_ref()
                    .ok_or_else(|| Error::from("output not configured"))?
                    .borrow_mut()(&text);

                Ok(Value::Tuple(Vec::new().into()))
            }),
            ir::RuntimeFunction::NumberToText => {
                runtime_text_fn!((Value::Number(n)) => n.normalize().to_string())
            }
            ir::RuntimeFunction::IntegerToText => runtime_text_fn!(Value::Integer),
            ir::RuntimeFunction::NaturalToText => runtime_text_fn!(Value::Natural),
            ir::RuntimeFunction::ByteToText => runtime_text_fn!(Value::Byte),
            ir::RuntimeFunction::SignedToText => runtime_text_fn!(Value::Signed),
            ir::RuntimeFunction::UnsignedToText => runtime_text_fn!(Value::Unsigned),
            ir::RuntimeFunction::FloatToText => runtime_text_fn!(Value::Float),
            ir::RuntimeFunction::DoubleToText => runtime_text_fn!(Value::Double),
            ir::RuntimeFunction::AddNumber => {
                runtime_math_fn!(Value::Number, (lhs, rhs) => Ok(lhs + rhs))
            }
            ir::RuntimeFunction::SubtractNumber => {
                runtime_math_fn!(Value::Number, (lhs, rhs) => Ok(lhs - rhs))
            }
            ir::RuntimeFunction::MultiplyNumber => {
                runtime_math_fn!(Value::Number, (lhs, rhs) => Ok(lhs * rhs))
            }
            ir::RuntimeFunction::DivideNumber => runtime_div_fn!(Value::Number, Decimal::ZERO),
            ir::RuntimeFunction::PowerNumber => {
                runtime_math_fn!(Value::Number, (lhs, rhs) => {
                    if lhs != Decimal::ZERO && rhs != Decimal::ZERO {
                        Ok(lhs.pow(rhs))
                    } else {
                        Err(Error::from("cannot raise zero to the power of zero"))
                    }
                })
            }
            ir::RuntimeFunction::FloorNumber => {
                runtime_math_fn!(Value::Number, (n) => Ok(n.floor()))
            }
            ir::RuntimeFunction::CeilNumber => runtime_math_fn!(Value::Number, (n) => Ok(n.ceil())),
            ir::RuntimeFunction::SqrtNumber => {
                runtime_math_fn!(Value::Number, (n) => Ok(n.sqrt().unwrap()))
            }
            ir::RuntimeFunction::AddInteger => {
                runtime_math_fn!(Value::Integer, (lhs, rhs) => Ok(lhs + rhs))
            }
            ir::RuntimeFunction::SubtractInteger => {
                runtime_math_fn!(Value::Integer, (lhs, rhs) => Ok(lhs - rhs))
            }
            ir::RuntimeFunction::MultiplyInteger => {
                runtime_math_fn!(Value::Integer, (lhs, rhs) => Ok(lhs * rhs))
            }
            ir::RuntimeFunction::DivideInteger => runtime_div_fn!(Value::Integer, 0),
            ir::RuntimeFunction::PowerInteger => runtime_math_fn!(Value::Integer, (lhs, rhs) => {
                if lhs != 0 && rhs != 0 {
                    Ok(lhs.pow(rhs as u32))
                } else {
                    Err(Error::from("cannot raise zero to the power of zero"))
                }
            }),
            ir::RuntimeFunction::AddNatural => {
                runtime_math_fn!(Value::Natural, (lhs, rhs) => Ok(lhs + rhs))
            }
            ir::RuntimeFunction::SubtractNatural => {
                runtime_math_fn!(Value::Natural, (lhs, rhs) => Ok(lhs - rhs))
            }
            ir::RuntimeFunction::MultiplyNatural => {
                runtime_math_fn!(Value::Natural, (lhs, rhs) => Ok(lhs * rhs))
            }
            ir::RuntimeFunction::DivideNatural => runtime_div_fn!(Value::Natural, 0),
            ir::RuntimeFunction::PowerNatural => {
                runtime_math_fn!(Value::Natural, (lhs, rhs) => {
                    if lhs != 0 && rhs != 0 {
                        Ok(lhs.pow(rhs as u32))
                    } else {
                        Err(Error::from("cannot raise zero to the power of zero"))
                    }
                })
            }
            ir::RuntimeFunction::AddByte => {
                runtime_math_fn!(Value::Byte, (lhs, rhs) => Ok(lhs + rhs))
            }
            ir::RuntimeFunction::SubtractByte => {
                runtime_math_fn!(Value::Byte, (lhs, rhs) => Ok(lhs - rhs))
            }
            ir::RuntimeFunction::MultiplyByte => {
                runtime_math_fn!(Value::Byte, (lhs, rhs) => Ok(lhs * rhs))
            }
            ir::RuntimeFunction::DivideByte => runtime_div_fn!(Value::Byte, 0),
            ir::RuntimeFunction::PowerByte => {
                runtime_math_fn!(Value::Byte, (lhs, rhs) => {
                    if lhs != 0 && rhs != 0 {
                        Ok(lhs.pow(rhs as u32))
                    } else {
                        Err(Error::from("cannot raise zero to the power of zero"))
                    }
                })
            }
            ir::RuntimeFunction::AddSigned => {
                runtime_math_fn!(Value::Signed, (lhs, rhs) => Ok(lhs + rhs))
            }
            ir::RuntimeFunction::SubtractSigned => {
                runtime_math_fn!(Value::Signed, (lhs, rhs) => Ok(lhs - rhs))
            }
            ir::RuntimeFunction::MultiplySigned => {
                runtime_math_fn!(Value::Signed, (lhs, rhs) => Ok(lhs * rhs))
            }
            ir::RuntimeFunction::DivideSigned => runtime_div_fn!(Value::Signed, 0),
            ir::RuntimeFunction::PowerSigned => runtime_math_fn!(Value::Signed, (lhs, rhs) => {
                if lhs != 0 && rhs != 0 {
                    Ok(lhs.pow(rhs as u32))
                } else {
                    Err(Error::from("cannot raise zero to the power of zero"))
                }
            }),
            ir::RuntimeFunction::AddUnsigned => {
                runtime_math_fn!(Value::Unsigned, (lhs, rhs) => Ok(lhs + rhs))
            }
            ir::RuntimeFunction::SubtractUnsigned => {
                runtime_math_fn!(Value::Unsigned, (lhs, rhs) => Ok(lhs - rhs))
            }
            ir::RuntimeFunction::MultiplyUnsigned => {
                runtime_math_fn!(Value::Unsigned, (lhs, rhs) => Ok(lhs * rhs))
            }
            ir::RuntimeFunction::DivideUnsigned => runtime_div_fn!(Value::Unsigned, 0),
            ir::RuntimeFunction::PowerUnsigned => {
                runtime_math_fn!(Value::Unsigned, (lhs, rhs) => {
                    if lhs != 0 && rhs != 0 {
                        Ok(lhs.pow(rhs))
                    } else {
                        Err(Error::from("cannot raise zero to the power of zero"))
                    }
                })
            }
            ir::RuntimeFunction::AddFloat => {
                runtime_math_fn!(Value::Float, (lhs, rhs) => Ok(lhs + rhs))
            }
            ir::RuntimeFunction::SubtractFloat => {
                runtime_math_fn!(Value::Float, (lhs, rhs) => Ok(lhs - rhs))
            }
            ir::RuntimeFunction::MultiplyFloat => {
                runtime_math_fn!(Value::Float, (lhs, rhs) => Ok(lhs * rhs))
            }
            ir::RuntimeFunction::DivideFloat => {
                runtime_math_fn!(Value::Float, (lhs, rhs) => Ok(lhs / rhs))
            }
            ir::RuntimeFunction::PowerFloat => {
                runtime_math_fn!(Value::Float, (lhs, rhs) => Ok(lhs.pow(rhs)))
            }
            ir::RuntimeFunction::FloorFloat => {
                runtime_math_fn!(Value::Double, (n) => Ok(n.floor()))
            }
            ir::RuntimeFunction::CeilFloat => runtime_math_fn!(Value::Double, (n) => Ok(n.ceil())),
            ir::RuntimeFunction::SqrtFloat => runtime_math_fn!(Value::Double, (n) => Ok(n.sqrt())),
            ir::RuntimeFunction::AddDouble => {
                runtime_math_fn!(Value::Double, (lhs, rhs) => Ok(lhs + rhs))
            }
            ir::RuntimeFunction::SubtractDouble => {
                runtime_math_fn!(Value::Double, (lhs, rhs) => Ok(lhs - rhs))
            }
            ir::RuntimeFunction::MultiplyDouble => {
                runtime_math_fn!(Value::Double, (lhs, rhs) => Ok(lhs * rhs))
            }
            ir::RuntimeFunction::DivideDouble => {
                runtime_math_fn!(Value::Double, (lhs, rhs) => Ok(lhs / rhs))
            }
            ir::RuntimeFunction::PowerDouble => {
                runtime_math_fn!(Value::Double, (lhs, rhs) => Ok(lhs.pow(rhs)))
            }
            ir::RuntimeFunction::FloorDouble => {
                runtime_math_fn!(Value::Double, (n) => Ok(n.floor()))
            }
            ir::RuntimeFunction::CeilDouble => runtime_math_fn!(Value::Double, (n) => Ok(n.ceil())),
            ir::RuntimeFunction::SqrtDouble => runtime_math_fn!(Value::Double, (n) => Ok(n.sqrt())),
            ir::RuntimeFunction::TextEquality => runtime_eq_fn!(Value::Text),
            ir::RuntimeFunction::NumberEquality => runtime_eq_fn!(Value::Number),
            ir::RuntimeFunction::IntegerEquality => runtime_eq_fn!(Value::Integer),
            ir::RuntimeFunction::NaturalEquality => runtime_eq_fn!(Value::Natural),
            ir::RuntimeFunction::ByteEquality => runtime_eq_fn!(Value::Byte),
            ir::RuntimeFunction::SignedEquality => runtime_eq_fn!(Value::Signed),
            ir::RuntimeFunction::UnsignedEquality => runtime_eq_fn!(Value::Unsigned),
            ir::RuntimeFunction::FloatEquality => runtime_eq_fn!(Value::Float),
            ir::RuntimeFunction::DoubleEquality => runtime_eq_fn!(Value::Double),
            ir::RuntimeFunction::TextOrdering => runtime_cmp_fn!(Value::Text),
            ir::RuntimeFunction::NumberOrdering => runtime_cmp_fn!(Value::Number),
            ir::RuntimeFunction::IntegerOrdering => runtime_cmp_fn!(Value::Integer),
            ir::RuntimeFunction::NaturalOrdering => runtime_cmp_fn!(Value::Natural),
            ir::RuntimeFunction::ByteOrdering => runtime_cmp_fn!(Value::Byte),
            ir::RuntimeFunction::SignedOrdering => runtime_cmp_fn!(Value::Signed),
            ir::RuntimeFunction::UnsignedOrdering => runtime_cmp_fn!(Value::Unsigned),
            ir::RuntimeFunction::FloatOrdering => {
                runtime_cmp_fn!((Value::Float(lhs), Value::Float(rhs)) => {
                    lhs.partial_cmp(&rhs).expect("unexpected NaN")
                })
            }
            ir::RuntimeFunction::DoubleOrdering => {
                runtime_cmp_fn!((Value::Double(lhs), Value::Double(rhs)) => {
                    lhs.partial_cmp(&rhs).expect("unexpected NaN")
                })
            }
            ir::RuntimeFunction::MakeMutable => {
                runtime_fn!((value) => {
                    Ok(Value::Mutable(Rc::new(RefCell::new(value))))
                })
            }
            ir::RuntimeFunction::GetMutable => {
                runtime_fn!((Value::Mutable(value)) => Ok(value.borrow().clone()))
            }
            ir::RuntimeFunction::SetMutable => {
                runtime_fn!((Value::Mutable(value), new_value) => {
                    *value.borrow_mut() = new_value;
                    Ok(Value::Tuple(Vec::new().into()))
                })
            }
            ir::RuntimeFunction::MakeSlice => {
                runtime_fn!((Value::Natural(length)) => {
                    Ok(Value::Slice(vec![Default::default(); length as usize].into()))
                })
            }
            ir::RuntimeFunction::SliceLength => {
                runtime_fn!((Value::Slice(slice)) => {
                    Ok(Value::Natural(slice.len() as u64))
                })
            }
            ir::RuntimeFunction::SliceIndex => {
                runtime_fn!((Value::Slice(slice), Value::Natural(index)) => {
                    let index = index as usize;
                    let index = if (0..slice.len()).contains(&index) {
                        index
                    } else {
                        return Ok(error(Value::Marker));
                    };

                    let slot = match slice.get(index) {
                        Some(slot) => slot,
                        None => return Ok(error(Value::Marker)),
                    };

                    match slot.borrow().as_ref() {
                        Some(value) => Ok(ok(value.clone())),
                        None =>  Err(Error::from("reference to uninitialized value in slice"))
                    }
                })
            }
            ir::RuntimeFunction::SetSliceIndex => {
                runtime_fn!((Value::Slice(slice), Value::Natural(index), value) => {
                    let index = index as usize;
                    let index = if (0..slice.len()).contains(&index) {
                        index
                    } else {
                        return Ok(error(Value::Marker));
                    };

                    let slot = match slice.get(index) {
                        Some(value) => value,
                        None => return Ok(error(Value::Marker)),
                    };

                    slot.replace(Some(value));

                    Ok(ok(Value::Tuple(Vec::new().into())))
                })
            }
            ir::RuntimeFunction::DeleteSliceIndex => {
                runtime_fn!((Value::Slice(slice), Value::Natural(index)) => {
                    let index = index as usize;
                    let index = if (0..slice.len()).contains(&index) {
                        index
                    } else {
                        return Ok(error(Value::Marker));
                    };

                    let slot = match slice.get(index) {
                        Some(value) => value,
                        None => return Ok(error(Value::Marker)),
                    };

                    slot.replace(None);

                    Ok(ok(Value::Tuple(Vec::new().into())))
                })
            }
        }
    }
}
