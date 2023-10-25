#![allow(clippy::redundant_closure_call)]

use crate::{Context, Error, Interpreter, IoRequest, Number, Stack, TaskGroup, UiHandle, Value};
use futures::channel::oneshot;
use itertools::Itertools;
use num_traits::{pow::Pow, FromPrimitive, ToPrimitive};
use parking_lot::Mutex;
use rand::{Rng, SeedableRng};
use rust_decimal::{Decimal, MathematicalOps};
use std::{mem, sync::Arc};
use tokio::sync::mpsc::channel;
use unicode_segmentation::UnicodeSegmentation;
use wipple_frontend::{helpers::Shared, ir, VariantIndex};

fn r#false() -> Value {
    Value::Variant(VariantIndex::new(0), Vec::new())
}

fn r#true() -> Value {
    Value::Variant(VariantIndex::new(1), Vec::new())
}

fn maybe(value: Option<Value>) -> Value {
    match value {
        Some(value) => some(value),
        None => none(),
    }
}

fn maybe_from(value: Value) -> Option<Value> {
    match value {
        Value::Variant(index, mut values) => match index.into_inner() {
            0 => None,
            1 => {
                assert!(values.len() == 1);
                Some(values.pop().unwrap())
            }
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

fn none() -> Value {
    Value::Variant(VariantIndex::new(0), Vec::new())
}

fn some(value: Value) -> Value {
    Value::Variant(VariantIndex::new(1), vec![value])
}

impl Interpreter {
    pub(crate) async fn call_runtime(
        &self,
        func: ir::Intrinsic,
        inputs: Vec<Value>,
        stack: &mut Stack,
        context: &Context,
    ) -> Result<(), Error> {
        #![allow(unreachable_patterns)]

        macro_rules! runtime_fn {
            (() => $result:expr) => {{
                assert!(inputs.is_empty(), "wrong number of inputs to builtin function");
                $result.await
            }};
            (($($input:pat),*) => $result:expr) => {
                match inputs
                    .into_iter()
                    .collect_tuple()
                    .expect("wrong number of inputs to builtin function")
                {
                    ($($input,)*) => $result.await,
                    _ => panic!("wrong type of inputs to builtin function"),
                }
            };
        }

        macro_rules! runtime_math_fn {
            (Value::$ty:ident, ($($input:pat),*) => $result:expr) => {
                runtime_fn!(($(Value::$ty($input)),*) => async { $result.await.map(Value::$ty) })
            };
        }

        macro_rules! runtime_div_fn {
            (Value::$ty:ident, $zero:expr) => {
                runtime_math_fn!(Value::$ty, (lhs, rhs) => async {
                    if rhs != $zero {
                        Ok(lhs / rhs)
                    } else {
                        Err(Error::from("cannot divide by zero"))
                    }
                })
            };
        }

        macro_rules! runtime_mod_fn {
            (Value::$ty:ident, $zero:expr) => {
                runtime_math_fn!(Value::$ty, (lhs, rhs) => async {
                    if rhs != $zero {
                        Ok(lhs % rhs)
                    } else {
                        Err(Error::from("cannot divide by zero"))
                    }
                })
            };
        }

        macro_rules! runtime_negate_fn {
            (Value::$ty:ident) => {
                runtime_fn!((Value::$ty(x)) => async {
                    Ok(Value::$ty(-x))
                })
            };
        }

        macro_rules! runtime_text_fn {
            (($($input:pat),*) => $result:expr) => {
                runtime_fn!(($($input),*) => async { Ok(Value::Text(Arc::from($result.await))) })
            };
            (Value::$ty:ident) => {
                runtime_text_fn!((Value::$ty(x)) => async { x.to_string() })
            };
        }

        macro_rules! runtime_parse_fn {
            (Value::$ty:ident) => {
                runtime_fn!((Value::Text(text)) => async { Ok(maybe(text.parse().ok().map(Value::$ty))) })
            };
        }

        macro_rules! runtime_cmp_fn {
            (($($input:pat),*) => $result:expr) => {
                runtime_fn!(($($input),*) => async {
                    let index = match $result.await {
                        std::cmp::Ordering::Less => VariantIndex::new(0),
                        std::cmp::Ordering::Equal => VariantIndex::new(1),
                        std::cmp::Ordering::Greater => VariantIndex::new(2),
                    };

                    Ok(Value::Variant(index, Vec::new()))
                })
            };
            (Value::$ty:ident) => {
                runtime_cmp_fn!((Value::$ty(lhs), Value::$ty(rhs)) => async { lhs.cmp(&rhs) })
            };
        }

        macro_rules! runtime_eq_fn {
            (Value::$ty:ident) => {
                runtime_fn!((Value::$ty(lhs), Value::$ty(rhs)) => async {
                    Ok(if lhs == rhs { r#true() } else { r#false() })
                })
            };
        }

        macro_rules! runtime_rand_fn {
            (Value::$ty:ident) => {
                runtime_fn!((Value::$ty(min), Value::$ty(max)) => async {
                    if min == max {
                        return Ok(Value::$ty(min));
                    }

                    let mut seed = [0; 32];
                    getrandom::getrandom(&mut seed).expect("failed to seed random number generator");
                    let mut rng = rand::rngs::StdRng::from_seed(seed);

                    Ok(Value::$ty(rng.gen_range(min..max)))
                })
            };
        }

        let value = (|| async {
            match func {
                ir::Intrinsic::Crash => {
                    runtime_fn!((Value::Text(text)) => async {
                        Err(Error::from(text.as_ref()))
                    })
                }
                ir::Intrinsic::Display => runtime_fn!((Value::Text(text)) => async {
                    let io = self.lock().io.clone();

                    let (completion_tx, completion_rx) = tokio::sync::oneshot::channel();
                    io(IoRequest::Display(self.clone(), &text, Box::new(|| {
                        completion_tx.send(()).unwrap()
                    }))).await?;

                    completion_rx.await.map_err(|_| Error::from("program exited"))?;

                    Ok(Value::Tuple(Vec::new()))
                }),
                ir::Intrinsic::Prompt => {
                    runtime_fn!((Value::Text(prompt), Value::Function(captures, label)) => async {
                        // So we don't have to deal with loading any captured variables. `prompt`
                        // will be called with an instance of `Read` anyway, which has no captures
                        assert!(captures.0.is_empty(), "`prompt` requires a function with no captures");

                        // FIXME: `context` is silently ignored -- implement a way to preserve them

                        let io = self.lock().io.clone();

                        let (input_tx, mut input_rx) = channel(1);
                        let (valid_tx, valid_rx) = channel(1);

                        let (completion_tx, completion_rx) = tokio::sync::oneshot::channel();
                        io(IoRequest::Prompt(self.clone(), &prompt, input_tx, valid_rx, Box::new(|| {
                            completion_tx.send(()).unwrap()
                        }))).await?;

                        let input = loop {
                            let input = input_rx.recv().await.unwrap();

                            stack.push(Value::Text(Arc::from(input)));
                            self.evaluate_label(label, stack, context).await.unwrap();

                            match maybe_from(stack.pop()) {
                                Some(value) => {
                                    valid_tx.send(true).await.unwrap();
                                    input_rx.close();
                                    break value;
                                },
                                None => {
                                    valid_tx.send(false).await.unwrap();
                                    continue;
                                }
                            }
                        };

                        completion_rx.await.map_err(|_| Error::from("program exited"))?;

                        Ok(input)
                    })
                }
                ir::Intrinsic::Choice => runtime_fn!((Value::Text(prompt), Value::List(list)) => async {
                    let choices = list
                        .iter()
                        .map(|value| match value {
                            Value::Text(text) => text.as_ref(),
                            _ => unreachable!(),
                        })
                        .collect::<Vec<_>>();

                    let io = self.lock().io.clone();

                    let (completion_tx, completion_rx) = tokio::sync::oneshot::channel();
                    io(IoRequest::Choice(self.clone(), &prompt, choices, Box::new(|index| {
                        completion_tx.send(index).unwrap()
                    }))).await?;

                    let index = completion_rx.await.map_err(|_| Error::from("program exited"))?;

                    Ok(Value::Natural(index as u64))
                }),
                ir::Intrinsic::WithUi => runtime_fn!((Value::Text(url), Value::Function(scope, label)) => async {
                    let io = self.lock().io.clone();

                    let (completion_tx, completion_rx) = tokio::sync::oneshot::channel();

                    io(IoRequest::Ui(self.clone(), &url, {
                        let interpreter = self.clone();
                        let context = context.deep_clone();

                        Box::new(move |on_message, on_finish| Box::pin(async move {
                            let handle = UiHandle {
                                on_message: Arc::new(Mutex::new(on_message)),
                                on_finish: Arc::new(Mutex::new(Some(on_finish))),
                            };

                            let result = interpreter.call_function(label, scope, &context, Value::UiHandle(handle.clone())).await?;

                            handle.on_finish.lock().take().unwrap()();

                            completion_tx.send(result).map_err(|_| Error::from("program exited"))?;

                            Ok(())
                        }))
                    })).await?;

                    let result = completion_rx.await.map_err(|_| Error::from("program exited"))?;

                    Ok(result)
                }),
                ir::Intrinsic::MessageUi => runtime_fn!((Value::UiHandle(handle), Value::Text(message), value) => async {
                    let (completion_tx, completion_rx) = tokio::sync::oneshot::channel();

                    handle.on_message.lock()(message.to_string(), value, context.deep_clone(), Box::new(move |result| {
                        completion_tx.send(result).map_err(|_| Error::from("program exited"))
                    }));

                    completion_rx.await.map_err(|_| Error::from("program exited"))?
                }),
                ir::Intrinsic::WithContinuation => runtime_fn!((Value::Function(scope, label)) => async {
                    let (completion_tx, completion_rx) = tokio::sync::oneshot::channel();

                    let completion_tx = Arc::new(Mutex::new(Some(completion_tx)));
                    let function = Value::NativeFunction(Arc::new(move |value| {
                        let completion_tx = completion_tx.clone();

                        Box::pin(async move {
                            let completion_tx = match completion_tx.lock().take() {
                                Some(tx) => tx,
                                None => return Err(Error::from("cannot call `with-continuation` callback more than once")),
                            };

                            completion_tx.send(value).map_err(|_| Error::from("program exited"))?;
                            Ok(Value::Tuple(Vec::new()))
                        })
                    }));

                    self.call_function(label, scope, context, function).await?;

                    match completion_rx.await {
                        Ok(result) => Ok(result),
                        Err(_) => {
                            // The program is in an infinite loop, wait for it to be cancelled
                            std::future::pending().await
                        },
                    }
                }),
                ir::Intrinsic::WithTaskGroup => runtime_fn!((Value::Function(scope, label)) => async {
                    let group = TaskGroup::default();

                    self.call_function(label, scope, context, Value::TaskGroup(group.clone())).await?;

                    let tasks = match group.0.try_lock() {
                        Some(mut group) => mem::take(&mut *group).into_iter().map(|f| f()).collect::<Vec<_>>(),
                        None => return Err(Error::from("task group used outside of `with-task-group`")),
                    };

                    for task in tasks {
                        task.await?;
                    }

                    Ok(Value::Tuple(Vec::new()))
                }),
                ir::Intrinsic::Task => runtime_fn!((Value::TaskGroup(group), Value::Function(scope, label)) => async {
                    let io = self.lock().io.clone();

                    let interpreter = self.clone();
                    let context = context.deep_clone();

                    let (completion_tx, completion_rx) = oneshot::channel();
                    let fut = Box::pin(async move {
                        let result = interpreter
                            .call_function(label, scope, &context, Value::Tuple(Vec::new()))
                            .await;

                        completion_tx.send(result.map(|_| ())).expect("failed to signal task completion");

                        Ok::<_, Error>(())
                    });

                    let start = {
                        let (completion_tx, completion_rx) = tokio::sync::oneshot::channel();
                        io(IoRequest::Schedule(self.clone(), fut, Box::new(|start| {
                            completion_tx
                                .send(start)
                                .unwrap_or_else(|_| panic!("failed to signal task completion"));
                        }))).await?;

                        completion_rx.await.map_err(|_| Error::from("program exited"))?
                    };

                    let mut group = match group.0.try_lock() {
                        Some(group) => group,
                        None => return Err(Error::from("task group used outside of `with-task-group`")),
                    };

                    group.push(Box::new(move || {
                        start();

                        Box::pin(async move {
                            completion_rx.await.map_err(|_| Error::from("program exited"))?
                        })
                    }));

                    Ok(Value::Tuple(Vec::new()))
                }),
                ir::Intrinsic::InBackground => runtime_fn!((Value::Function(scope, label)) => async {
                    let io = self.lock().io.clone();

                    let interpreter = self.clone();
                    let context = context.deep_clone();

                    let fut = Box::pin(async move {
                        interpreter
                            .call_function(label, scope, &context, Value::Tuple(Vec::new()))
                            .await?;

                        Ok(())
                    });

                    let start = {
                        let (completion_tx, completion_rx) = tokio::sync::oneshot::channel();
                        io(IoRequest::Schedule(self.clone(), fut, Box::new(|start| {
                            completion_tx
                                .send(start)
                                .unwrap_or_else(|_| panic!("failed to signal task completion"));
                        }))).await?;

                        completion_rx.await.map_err(|_| Error::from("program exited"))?
                    };

                    start();

                    Ok(Value::Tuple(Vec::new()))
                }),
                ir::Intrinsic::Delay => runtime_fn!((Value::Natural(ms)) => async {
                    let duration = std::time::Duration::from_millis(ms);

                    let io = self.lock().io.clone();

                    let (completion_tx, completion_rx) = tokio::sync::oneshot::channel();
                    io(IoRequest::Sleep(self.clone(), duration, Box::new(|| {
                        completion_tx.send(()).unwrap()
                    }))).await?;

                    completion_rx.await.map_err(|_| Error::from("program exited"))?;

                    Ok(Value::Tuple(Vec::new()))
                }),
                ir::Intrinsic::NumberToText => {
                    runtime_text_fn!((Value::Number(n)) => async {
                        match n {
                            Number::Undefined => String::from("undefined"),
                            Number::Decimal(n) => n.normalize().to_string(),
                        }
                    })
                }
                ir::Intrinsic::IntegerToText => runtime_text_fn!(Value::Integer),
                ir::Intrinsic::NaturalToText => runtime_text_fn!(Value::Natural),
                ir::Intrinsic::ByteToText => runtime_text_fn!(Value::Byte),
                ir::Intrinsic::SignedToText => runtime_text_fn!(Value::Signed),
                ir::Intrinsic::UnsignedToText => runtime_text_fn!(Value::Unsigned),
                ir::Intrinsic::FloatToText => runtime_text_fn!(Value::Float),
                ir::Intrinsic::DoubleToText => runtime_text_fn!(Value::Double),
                ir::Intrinsic::TextToNumber => runtime_fn!((Value::Text(text)) => async {
                    Ok(maybe(text.parse().ok().map(Number::Decimal).map(Value::Number)))
                }),
                ir::Intrinsic::TextToInteger => runtime_parse_fn!(Value::Integer),
                ir::Intrinsic::TextToNatural => runtime_parse_fn!(Value::Natural),
                ir::Intrinsic::TextToByte => runtime_parse_fn!(Value::Byte),
                ir::Intrinsic::TextToSigned => runtime_parse_fn!(Value::Signed),
                ir::Intrinsic::TextToUnsigned => runtime_parse_fn!(Value::Unsigned),
                ir::Intrinsic::TextToFloat => runtime_parse_fn!(Value::Float),
                ir::Intrinsic::TextToDouble => runtime_parse_fn!(Value::Double),
                ir::Intrinsic::NaturalToNumber => runtime_fn!((Value::Natural(n)) => async {
                    Ok(Value::Number(Decimal::from_u64(n).map_or(Number::Undefined, Number::Decimal)))
                }),
                ir::Intrinsic::NumberToNatural => runtime_fn!((Value::Number(n)) => async {
                    match n {
                        Number::Decimal(n) if n >= Decimal::ZERO && n.is_integer() => {
                            Ok(some(Value::Natural(n.to_u64().unwrap())))
                        }
                        _ => Ok(none()),
                    }
                }),
                ir::Intrinsic::NaturalToInteger => runtime_fn!((Value::Natural(n)) => async {
                    Ok(Value::Integer(i64::from_u64(n).expect("overflow")))
                }),
                ir::Intrinsic::IntegerToNatural => runtime_fn!((Value::Integer(n)) => async {
                    if n >= 0 {
                        Ok(some(Value::Natural(n.to_u64().unwrap())))
                    } else {
                        Ok(none())
                    }
                }),
                ir::Intrinsic::AddNumber => {
                    runtime_math_fn!(Value::Number, (lhs, rhs) => async {
                        Ok(match (lhs, rhs) {
                            (Number::Decimal(lhs), Number::Decimal(rhs)) => {
                                lhs.checked_add(rhs).map_or(Number::Undefined, Number::Decimal)
                            }
                            _ => Number::Undefined,
                        })
                    })
                }
                ir::Intrinsic::SubtractNumber => {
                    runtime_math_fn!(Value::Number, (lhs, rhs) => async {
                        Ok(match (lhs, rhs) {
                            (Number::Decimal(lhs), Number::Decimal(rhs)) => {
                                lhs.checked_sub(rhs).map_or(Number::Undefined, Number::Decimal)
                            }
                            _ => Number::Undefined,
                        })
                    })
                }
                ir::Intrinsic::MultiplyNumber => {
                    runtime_math_fn!(Value::Number, (lhs, rhs) => async {
                        Ok(match (lhs, rhs) {
                            (Number::Decimal(lhs), Number::Decimal(rhs)) => {
                                lhs.checked_mul(rhs).map_or(Number::Undefined, Number::Decimal)
                            }
                            _ => Number::Undefined,
                        })
                    })
                }
                ir::Intrinsic::DivideNumber => {
                    runtime_math_fn!(Value::Number, (lhs, rhs) => async {
                        Ok(match (lhs, rhs) {
                            (Number::Decimal(lhs), Number::Decimal(rhs)) => {
                                lhs.checked_div(rhs).map_or(Number::Undefined, Number::Decimal)
                            }
                            _ => Number::Undefined,
                        })
                    })
                }
                ir::Intrinsic::ModuloNumber => {
                    runtime_math_fn!(Value::Number, (lhs, rhs) => async {
                        Ok(match (lhs, rhs) {
                            (Number::Decimal(lhs), Number::Decimal(rhs)) => {
                                lhs.checked_rem(rhs).map_or(Number::Undefined, Number::Decimal)
                            }
                            _ => Number::Undefined,
                        })
                    })
                }
                ir::Intrinsic::PowerNumber => {
                    runtime_math_fn!(Value::Number, (lhs, rhs) => async {
                        Ok(match (lhs, rhs) {
                            (Number::Decimal(lhs), Number::Decimal(rhs)) => {
                                lhs.checked_powd(rhs).map_or(Number::Undefined, Number::Decimal)
                            }
                            _ => Number::Undefined,
                        })
                    })
                }
                ir::Intrinsic::FloorNumber => {
                    runtime_math_fn!(Value::Number, (n) => async {
                        Ok(match n {
                            Number::Decimal(n) => Number::Decimal(n.floor()),
                            Number::Undefined => Number::Undefined,
                        })
                    })
                }
                ir::Intrinsic::CeilNumber => {
                    runtime_math_fn!(Value::Number, (n) => async {
                        Ok(match n {
                            Number::Decimal(n) => Number::Decimal(n.ceil()),
                            Number::Undefined => Number::Undefined,
                        })
                    })
                }
                ir::Intrinsic::SqrtNumber => {
                    runtime_math_fn!(Value::Number, (n) => async {
                        Ok(match n {
                            Number::Decimal(n) => n.sqrt().map_or(Number::Undefined, Number::Decimal),
                            Number::Undefined => Number::Undefined,
                        })
                    })
                }
                ir::Intrinsic::NegateNumber => {
                    runtime_math_fn!(Value::Number, (n) => async {
                        Ok(match n {
                            Number::Decimal(n) => Number::Decimal(-n),
                            Number::Undefined => Number::Undefined,
                        })
                    })
                }
                ir::Intrinsic::AddInteger => {
                    runtime_math_fn!(Value::Integer, (lhs, rhs) => async {
                        Ok(lhs + rhs)
                    })
                }
                ir::Intrinsic::SubtractInteger => {
                    runtime_math_fn!(Value::Integer, (lhs, rhs) => async {
                        Ok(lhs - rhs)
                    })
                }
                ir::Intrinsic::MultiplyInteger => {
                    runtime_math_fn!(Value::Integer, (lhs, rhs) => async {
                        Ok(lhs * rhs)
                    })
                }
                ir::Intrinsic::DivideInteger => runtime_div_fn!(Value::Integer, 0),
                ir::Intrinsic::ModuloInteger => runtime_mod_fn!(Value::Integer, 0),
                ir::Intrinsic::PowerInteger => {
                    runtime_math_fn!(Value::Integer, (lhs, rhs) => async {
                        if lhs == 0 && rhs == 0 {
                            Err(Error::from("cannot raise zero to the power of zero"))
                        } else {
                            Ok(lhs.pow(rhs as u32))
                        }
                    })
                }
                ir::Intrinsic::NegateInteger => runtime_negate_fn!(Value::Integer),
                ir::Intrinsic::AddNatural => {
                    runtime_math_fn!(Value::Natural, (lhs, rhs) => async {
                        Ok(lhs + rhs)
                    })
                }
                ir::Intrinsic::SubtractNatural => {
                    runtime_math_fn!(Value::Natural, (lhs, rhs) => async {
                        Ok(lhs - rhs)
                    })
                }
                ir::Intrinsic::MultiplyNatural => {
                    runtime_math_fn!(Value::Natural, (lhs, rhs) => async {
                        Ok(lhs * rhs)
                    })
                }
                ir::Intrinsic::DivideNatural => runtime_div_fn!(Value::Natural, 0),
                ir::Intrinsic::ModuloNatural => runtime_mod_fn!(Value::Natural, 0),
                ir::Intrinsic::PowerNatural => {
                    runtime_math_fn!(Value::Natural, (lhs, rhs) => async {
                        if lhs == 0 && rhs == 0 {
                            Err(Error::from("cannot raise zero to the power of zero"))
                        } else {
                            Ok(lhs.pow(rhs as u32))
                        }
                    })
                }
                ir::Intrinsic::AddByte => {
                    runtime_math_fn!(Value::Byte, (lhs, rhs) => async {
                        Ok(lhs + rhs)
                    })
                }
                ir::Intrinsic::SubtractByte => {
                    runtime_math_fn!(Value::Byte, (lhs, rhs) => async {
                        Ok(lhs - rhs)
                    })
                }
                ir::Intrinsic::MultiplyByte => {
                    runtime_math_fn!(Value::Byte, (lhs, rhs) => async {
                        Ok(lhs * rhs)
                    })
                }
                ir::Intrinsic::DivideByte => runtime_div_fn!(Value::Byte, 0),
                ir::Intrinsic::ModuloByte => runtime_mod_fn!(Value::Byte, 0),
                ir::Intrinsic::PowerByte => {
                    runtime_math_fn!(Value::Byte, (lhs, rhs) => async {
                        if lhs == 0 && rhs == 0 {
                            Err(Error::from("cannot raise zero to the power of zero"))
                        } else {
                            Ok(lhs.pow(rhs as u32))
                        }
                    })
                }
                ir::Intrinsic::AddSigned => {
                    runtime_math_fn!(Value::Signed, (lhs, rhs) => async {
                        Ok(lhs + rhs)
                    })
                }
                ir::Intrinsic::SubtractSigned => {
                    runtime_math_fn!(Value::Signed, (lhs, rhs) => async {
                        Ok(lhs - rhs)
                    })
                }
                ir::Intrinsic::MultiplySigned => {
                    runtime_math_fn!(Value::Signed, (lhs, rhs) => async {
                        Ok(lhs * rhs)
                    })
                }
                ir::Intrinsic::DivideSigned => runtime_div_fn!(Value::Signed, 0),
                ir::Intrinsic::ModuloSigned => runtime_mod_fn!(Value::Signed, 0),
                ir::Intrinsic::PowerSigned => runtime_math_fn!(Value::Signed, (lhs, rhs) => async {
                    if lhs == 0 && rhs == 0 {
                        Err(Error::from("cannot raise zero to the power of zero"))
                    } else {
                        Ok(lhs.pow(rhs as u32))
                    }
                }),
                ir::Intrinsic::NegateSigned => runtime_negate_fn!(Value::Signed),
                ir::Intrinsic::AddUnsigned => {
                    runtime_math_fn!(Value::Unsigned, (lhs, rhs) => async {
                        Ok(lhs + rhs)
                    })
                }
                ir::Intrinsic::SubtractUnsigned => {
                    runtime_math_fn!(Value::Unsigned, (lhs, rhs) => async {
                        Ok(lhs - rhs)
                    })
                }
                ir::Intrinsic::MultiplyUnsigned => {
                    runtime_math_fn!(Value::Unsigned, (lhs, rhs) => async {
                        Ok(lhs * rhs)
                    })
                }
                ir::Intrinsic::DivideUnsigned => runtime_div_fn!(Value::Unsigned, 0),
                ir::Intrinsic::ModuloUnsigned => runtime_mod_fn!(Value::Unsigned, 0),
                ir::Intrinsic::PowerUnsigned => {
                    runtime_math_fn!(Value::Unsigned, (lhs, rhs) => async {
                        if lhs == 0 && rhs == 0 {
                            Err(Error::from("cannot raise zero to the power of zero"))
                        } else {
                            Ok(lhs.pow(rhs))
                        }
                    })
                }
                ir::Intrinsic::AddFloat => {
                    runtime_math_fn!(Value::Float, (lhs, rhs) => async {
                        Ok(lhs + rhs)
                    })
                }
                ir::Intrinsic::SubtractFloat => {
                    runtime_math_fn!(Value::Float, (lhs, rhs) => async {
                        Ok(lhs - rhs)
                    })
                }
                ir::Intrinsic::MultiplyFloat => {
                    runtime_math_fn!(Value::Float, (lhs, rhs) => async {
                        Ok(lhs * rhs)
                    })
                }
                ir::Intrinsic::DivideFloat => {
                    runtime_math_fn!(Value::Float, (lhs, rhs) => async {
                        Ok(lhs / rhs)
                    })
                }
                ir::Intrinsic::ModuloFloat => {
                    runtime_math_fn!(Value::Float, (lhs, rhs) => async {
                        Ok(lhs % rhs)
                    })
                }
                ir::Intrinsic::PowerFloat => {
                    runtime_math_fn!(Value::Float, (lhs, rhs) => async {
                        Ok(lhs.pow(rhs))
                    })
                }
                ir::Intrinsic::FloorFloat => {
                    runtime_math_fn!(Value::Double, (n) => async {
                        Ok(n.floor())
                    })
                }
                ir::Intrinsic::CeilFloat => {
                    runtime_math_fn!(Value::Double, (n) => async {
                        Ok(n.ceil())
                    })
                }
                ir::Intrinsic::SqrtFloat => {
                    runtime_math_fn!(Value::Double, (n) => async {
                        Ok(n.sqrt())
                    })
                }
                ir::Intrinsic::NegateFloat => runtime_negate_fn!(Value::Float),
                ir::Intrinsic::AddDouble => {
                    runtime_math_fn!(Value::Double, (lhs, rhs) => async {
                        Ok(lhs + rhs)
                    })
                }
                ir::Intrinsic::SubtractDouble => {
                    runtime_math_fn!(Value::Double, (lhs, rhs) => async {
                        Ok(lhs - rhs)
                    })
                }
                ir::Intrinsic::MultiplyDouble => {
                    runtime_math_fn!(Value::Double, (lhs, rhs) => async {
                        Ok(lhs * rhs)
                    })
                }
                ir::Intrinsic::DivideDouble => {
                    runtime_math_fn!(Value::Double, (lhs, rhs) => async {
                        Ok(lhs / rhs)
                    })
                }
                ir::Intrinsic::ModuloDouble => {
                    runtime_math_fn!(Value::Double, (lhs, rhs) => async {
                        Ok(lhs % rhs)
                    })
                }
                ir::Intrinsic::PowerDouble => {
                    runtime_math_fn!(Value::Double, (lhs, rhs) => async {
                        Ok(lhs.pow(rhs))
                    })
                }
                ir::Intrinsic::FloorDouble => {
                    runtime_math_fn!(Value::Double, (n) => async {
                        Ok(n.floor())
                    })
                }
                ir::Intrinsic::CeilDouble => {
                    runtime_math_fn!(Value::Double, (n) => async {
                        Ok(n.ceil())
                    })
                }
                ir::Intrinsic::SqrtDouble => {
                    runtime_math_fn!(Value::Double, (n) => async {
                        Ok(n.sqrt())
                    })
                }
                ir::Intrinsic::NegateDouble => runtime_negate_fn!(Value::Double),
                ir::Intrinsic::TextEquality => runtime_eq_fn!(Value::Text),
                ir::Intrinsic::NumberEquality => runtime_eq_fn!(Value::Number),
                ir::Intrinsic::IntegerEquality => runtime_eq_fn!(Value::Integer),
                ir::Intrinsic::NaturalEquality => runtime_eq_fn!(Value::Natural),
                ir::Intrinsic::ByteEquality => runtime_eq_fn!(Value::Byte),
                ir::Intrinsic::SignedEquality => runtime_eq_fn!(Value::Signed),
                ir::Intrinsic::UnsignedEquality => runtime_eq_fn!(Value::Unsigned),
                ir::Intrinsic::FloatEquality => runtime_eq_fn!(Value::Float),
                ir::Intrinsic::DoubleEquality => runtime_eq_fn!(Value::Double),
                ir::Intrinsic::TextOrdering => runtime_cmp_fn!(Value::Text),
                ir::Intrinsic::NumberOrdering => {
                    runtime_cmp_fn!((Value::Number(lhs), Value::Number(rhs)) => async {
                        lhs.cmp(&rhs)
                    })
                },
                ir::Intrinsic::IntegerOrdering => runtime_cmp_fn!(Value::Integer),
                ir::Intrinsic::NaturalOrdering => runtime_cmp_fn!(Value::Natural),
                ir::Intrinsic::ByteOrdering => runtime_cmp_fn!(Value::Byte),
                ir::Intrinsic::SignedOrdering => runtime_cmp_fn!(Value::Signed),
                ir::Intrinsic::UnsignedOrdering => runtime_cmp_fn!(Value::Unsigned),
                ir::Intrinsic::FloatOrdering => {
                    runtime_cmp_fn!((Value::Float(lhs), Value::Float(rhs)) => async {
                        lhs.partial_cmp(&rhs).unwrap_or(std::cmp::Ordering::Equal)
                    })
                }
                ir::Intrinsic::DoubleOrdering => {
                    runtime_cmp_fn!((Value::Double(lhs), Value::Double(rhs)) => async {
                        lhs.partial_cmp(&rhs).unwrap_or(std::cmp::Ordering::Equal)
                    })
                }
                ir::Intrinsic::MakeMutable => runtime_fn!((value) => async {
                    Ok(Value::Mutable(Shared::new(value)))
                }),
                ir::Intrinsic::GetMutable => {
                    runtime_fn!((Value::Mutable(value)) => async {
                        Ok(value.lock().clone())
                    })
                }
                ir::Intrinsic::SetMutable => {
                    runtime_fn!((Value::Mutable(value), new_value) => async {
                        *value.lock() = new_value;
                        Ok(Value::Tuple(Vec::new()))
                    })
                }
                ir::Intrinsic::MakeEmptyList => {
                    assert!(inputs.is_empty());
                    Ok(Value::List(Default::default()))
                }
                ir::Intrinsic::ListFirst => runtime_fn!((Value::List(list)) => async {
                    Ok(match list.front() {
                        Some(first) => some(first.clone()),
                        None => none(),
                    })
                }),
                ir::Intrinsic::ListLast => runtime_fn!((Value::List(list)) => async {
                    Ok(match list.back() {
                        Some(last) => some(last.clone()),
                        None => none(),
                    })
                }),
                ir::Intrinsic::ListInitial => {
                    runtime_fn!((Value::List(mut list)) => async {
                        Ok(if list.is_empty() {
                            none()
                        } else {
                            some(Value::List(list.slice(0..(list.len() - 1))))
                        })
                    })
                }
                ir::Intrinsic::ListTail => {
                    runtime_fn!((Value::List(mut list)) => async {
                        Ok(if list.is_empty() {
                            none()
                        } else {
                            some(Value::List(list.slice(1..list.len())))
                        })
                    })
                }
                ir::Intrinsic::ListNth => {
                    runtime_fn!((Value::List(list), Value::Natural(index)) => async {
                        Ok(match list.get(index as usize) {
                            Some(value) => some(value.clone()),
                            None => none(),
                        })
                    })
                }
                ir::Intrinsic::ListAppend => {
                    runtime_fn!((Value::List(mut list), value) => async {
                        list.push_back(value);
                        Ok(Value::List(list))
                    })
                }
                ir::Intrinsic::ListPrepend => {
                    runtime_fn!((Value::List(mut list), value) => async {
                        list.push_front(value);
                        Ok(Value::List(list))
                    })
                }
                ir::Intrinsic::ListInsertAt => {
                    runtime_fn!((Value::List(mut list), Value::Natural(index), value) => async {
                        let index = index as usize;
                        let index = if (0..list.len()).contains(&index) {
                            index
                        } else {
                            return Ok(none());
                        };

                        list.insert(index, value);

                        Ok(some(Value::List(list)))
                    })
                }
                ir::Intrinsic::ListRemoveAt => {
                    runtime_fn!((Value::List(mut list), Value::Natural(index)) => async {
                        let index = index as usize;
                        let index = if (0..list.len()).contains(&index) {
                            index
                        } else {
                            return Ok(none());
                        };

                        Ok(some(list.remove(index)))
                    })
                }
                ir::Intrinsic::ListCount => {
                    runtime_fn!((Value::List(list)) => async {
                        Ok(Value::Natural(list.len() as u64))
                    })
                }
                ir::Intrinsic::ListSlice => {
                    runtime_fn!((Value::List(mut list), Value::Natural(start), Value::Natural(end)) => async {
                        Ok(Value::List(list.slice((start as usize)..(end as usize))))
                    })
                }
                ir::Intrinsic::TextCharacters => {
                    runtime_fn!((Value::Text(text)) => async {
                        Ok(Value::List(
                            text.graphemes(true)
                                .map(|s| Value::Text(Arc::from(s)))
                                .collect(),
                        ))
                    })
                }
                ir::Intrinsic::RandomNumber => {
                    runtime_fn!((Value::Number(min), Value::Number(max)) => async {
                        let (min, max) = match (min, max) {
                            (Number::Decimal(min), Number::Decimal(max)) => (min, max),
                            _ => return Ok(Value::Number(Number::Undefined)),
                        };

                        if min == max {
                            return Ok(Value::Number(Number::Decimal(min)));
                        }

                        let mut seed = [0; 32];
                        getrandom::getrandom(&mut seed).expect("failed to seed random number generator");
                        let mut rng = rand::rngs::StdRng::from_seed(seed);

                        Ok(Value::Number(Number::Decimal(rng.gen_range(min..max))))
                    })
                }
                ir::Intrinsic::RandomInteger => runtime_rand_fn!(Value::Integer),
                ir::Intrinsic::RandomNatural => runtime_rand_fn!(Value::Natural),
                ir::Intrinsic::RandomByte => runtime_rand_fn!(Value::Byte),
                ir::Intrinsic::RandomSigned => runtime_rand_fn!(Value::Signed),
                ir::Intrinsic::RandomUnsigned => runtime_rand_fn!(Value::Unsigned),
                ir::Intrinsic::RandomFloat => runtime_rand_fn!(Value::Float),
                ir::Intrinsic::RandomDouble => runtime_rand_fn!(Value::Double),
                ir::Intrinsic::UndefinedNumber => runtime_fn!(() => async {
                    Ok(Value::Number(Number::Undefined))
                }),
            }
        })().await?;

        stack.push(value);

        Ok(())
    }
}
