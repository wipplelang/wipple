use crate::{Context, Error, Interpreter, IoRequest, Stack, TaskGroup, UiHandle, Value};
use futures::channel::oneshot;
use itertools::Itertools;
use num_traits::{pow::Pow, FromPrimitive, ToPrimitive};
use parking_lot::Mutex;
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

fn ok(value: Value) -> Value {
    Value::Variant(VariantIndex::new(0), vec![value])
}

fn error(value: Value) -> Value {
    Value::Variant(VariantIndex::new(1), vec![value])
}

impl Interpreter {
    pub(crate) async fn call_runtime(
        &self,
        func: ir::RuntimeFunction,
        inputs: Vec<Value>,
        stack: &mut Stack,
        context: &Context,
    ) -> Result<(), Error> {
        #![allow(unreachable_patterns)]

        macro_rules! runtime_fn {
            (($($input:pat),*) => $result:expr) => {
                match inputs
                    .into_iter()
                    .collect_tuple()
                    .expect("wrong number of inputs to builtin function")
                {
                    ($($input,)*) => $result.await,
                    _ => unreachable!(),
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

        let value = (|| async {
            match func {
                ir::RuntimeFunction::Crash => {
                    runtime_fn!((Value::Text(text)) => async {
                        Err(Error::from(text.as_ref()))
                    })
                }
                ir::RuntimeFunction::Display => runtime_fn!((Value::Text(text)) => async {
                    let io = self.lock().io.clone();

                    let (completion_tx, completion_rx) = tokio::sync::oneshot::channel();
                    io(IoRequest::Display(self.clone(), &text, Box::new(|| {
                        completion_tx.send(()).unwrap()
                    }))).await?;

                    completion_rx.await.map_err(|_| Error::from("program exited"))?;

                    Ok(Value::Tuple(Vec::new()))
                }),
                ir::RuntimeFunction::Prompt => {
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
                ir::RuntimeFunction::Choice => runtime_fn!((Value::Text(prompt), Value::List(list)) => async {
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
                ir::RuntimeFunction::WithUi => runtime_fn!((Value::Text(url), Value::Function(scope, label)) => async {
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
                ir::RuntimeFunction::MessageUi => runtime_fn!((Value::UiHandle(handle), Value::Text(message), value) => async {
                    let (completion_tx, completion_rx) = tokio::sync::oneshot::channel();

                    handle.on_message.lock()(message.to_string(), value, context.deep_clone(), Box::new(move |result| {
                        completion_tx.send(result).map_err(|_| Error::from("program exited"))
                    }));

                    completion_rx.await.map_err(|_| Error::from("program exited"))?
                }),
                ir::RuntimeFunction::WithContinuation => runtime_fn!((Value::Function(scope, label)) => async {
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

                    self.call_function(label, scope, &context, function).await?;

                    match completion_rx.await {
                        Ok(result) => Ok(result),
                        Err(_) => {
                            // The program is in an infinite loop, wait for it to be cancelled
                            std::future::pending().await
                        },
                    }
                }),
                ir::RuntimeFunction::WithTaskGroup => runtime_fn!((Value::Function(scope, label)) => async {
                    let group = TaskGroup::default();

                    self.call_function(label, scope, &context, Value::TaskGroup(group.clone())).await?;

                    let tasks = match group.0.try_lock() {
                        Some(mut group) => mem::take(&mut *group).into_iter().map(|f| f()).collect::<Vec<_>>(),
                        None => return Err(Error::from("task group used outside of `with-task-group`")),
                    };

                    for task in tasks {
                        task.await?;
                    }

                    Ok(Value::Tuple(Vec::new()))
                }),
                ir::RuntimeFunction::Task => runtime_fn!((Value::TaskGroup(group), Value::Function(scope, label)) => async {
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
                ir::RuntimeFunction::InBackground => runtime_fn!((Value::Function(scope, label)) => async {
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
                ir::RuntimeFunction::Delay => runtime_fn!((Value::Natural(ms)) => async {
                    let duration = std::time::Duration::from_millis(ms);

                    let io = self.lock().io.clone();

                    let (completion_tx, completion_rx) = tokio::sync::oneshot::channel();
                    io(IoRequest::Sleep(self.clone(), duration, Box::new(|| {
                        completion_tx.send(()).unwrap()
                    }))).await?;

                    completion_rx.await.map_err(|_| Error::from("program exited"))?;

                    Ok(Value::Tuple(Vec::new()))
                }),
                ir::RuntimeFunction::NumberToText => {
                    runtime_text_fn!((Value::Number(n)) => async {
                        n.normalize().to_string()
                    })
                }
                ir::RuntimeFunction::IntegerToText => runtime_text_fn!(Value::Integer),
                ir::RuntimeFunction::NaturalToText => runtime_text_fn!(Value::Natural),
                ir::RuntimeFunction::ByteToText => runtime_text_fn!(Value::Byte),
                ir::RuntimeFunction::SignedToText => runtime_text_fn!(Value::Signed),
                ir::RuntimeFunction::UnsignedToText => runtime_text_fn!(Value::Unsigned),
                ir::RuntimeFunction::FloatToText => runtime_text_fn!(Value::Float),
                ir::RuntimeFunction::DoubleToText => runtime_text_fn!(Value::Double),
                ir::RuntimeFunction::TextToNumber => runtime_parse_fn!(Value::Number),
                ir::RuntimeFunction::TextToInteger => runtime_parse_fn!(Value::Integer),
                ir::RuntimeFunction::TextToNatural => runtime_parse_fn!(Value::Natural),
                ir::RuntimeFunction::TextToByte => runtime_parse_fn!(Value::Byte),
                ir::RuntimeFunction::TextToSigned => runtime_parse_fn!(Value::Signed),
                ir::RuntimeFunction::TextToUnsigned => runtime_parse_fn!(Value::Unsigned),
                ir::RuntimeFunction::TextToFloat => runtime_parse_fn!(Value::Float),
                ir::RuntimeFunction::TextToDouble => runtime_parse_fn!(Value::Double),
                ir::RuntimeFunction::NaturalToNumber => runtime_fn!((Value::Natural(n)) => async {
                    Ok(Value::Number(Decimal::from_u64(n).expect("overflow")))
                }),
                ir::RuntimeFunction::NumberToNatural => runtime_fn!((Value::Number(n)) => async {
                    if n == n.trunc() {
                        Ok(some(Value::Natural(n.to_u64().unwrap())))
                    } else {
                        Ok(none())
                    }
                }),
                ir::RuntimeFunction::AddNumber => {
                    runtime_math_fn!(Value::Number, (lhs, rhs) => async {
                        Ok(lhs + rhs)
                    })
                }
                ir::RuntimeFunction::SubtractNumber => {
                    runtime_math_fn!(Value::Number, (lhs, rhs) => async {
                        Ok(lhs - rhs)
                    })
                }
                ir::RuntimeFunction::MultiplyNumber => {
                    runtime_math_fn!(Value::Number, (lhs, rhs) => async {
                        Ok(lhs * rhs)
                    })
                }
                ir::RuntimeFunction::DivideNumber => runtime_div_fn!(Value::Number, Decimal::ZERO),
                ir::RuntimeFunction::ModuloNumber => runtime_mod_fn!(Value::Number, Decimal::ZERO),
                ir::RuntimeFunction::PowerNumber => {
                    runtime_math_fn!(Value::Number, (lhs, rhs) => async {
                        if lhs == Decimal::ZERO && rhs == Decimal::ZERO {
                            Err(Error::from("cannot raise zero to the power of zero"))
                        } else {
                            Ok(lhs.pow(rhs))
                        }
                    })
                }
                ir::RuntimeFunction::FloorNumber => {
                    runtime_math_fn!(Value::Number, (n) => async {
                        Ok(n.floor())
                    })
                }
                ir::RuntimeFunction::CeilNumber => {
                    runtime_math_fn!(Value::Number, (n) => async {
                        Ok(n.ceil())
                    })
                }
                ir::RuntimeFunction::SqrtNumber => {
                    runtime_math_fn!(Value::Number, (n) => async {
                        Ok(n.sqrt().unwrap())
                    })
                }
                ir::RuntimeFunction::AddInteger => {
                    runtime_math_fn!(Value::Integer, (lhs, rhs) => async {
                        Ok(lhs + rhs)
                    })
                }
                ir::RuntimeFunction::SubtractInteger => {
                    runtime_math_fn!(Value::Integer, (lhs, rhs) => async {
                        Ok(lhs - rhs)
                    })
                }
                ir::RuntimeFunction::MultiplyInteger => {
                    runtime_math_fn!(Value::Integer, (lhs, rhs) => async {
                        Ok(lhs * rhs)
                    })
                }
                ir::RuntimeFunction::DivideInteger => runtime_div_fn!(Value::Integer, 0),
                ir::RuntimeFunction::ModuloInteger => runtime_mod_fn!(Value::Integer, 0),
                ir::RuntimeFunction::PowerInteger => {
                    runtime_math_fn!(Value::Integer, (lhs, rhs) => async {
                        if lhs == 0 && rhs == 0 {
                            Err(Error::from("cannot raise zero to the power of zero"))
                        } else {
                            Ok(lhs.pow(rhs as u32))
                        }
                    })
                }
                ir::RuntimeFunction::AddNatural => {
                    runtime_math_fn!(Value::Natural, (lhs, rhs) => async {
                        Ok(lhs + rhs)
                    })
                }
                ir::RuntimeFunction::SubtractNatural => {
                    runtime_math_fn!(Value::Natural, (lhs, rhs) => async {
                        Ok(lhs - rhs)
                    })
                }
                ir::RuntimeFunction::MultiplyNatural => {
                    runtime_math_fn!(Value::Natural, (lhs, rhs) => async {
                        Ok(lhs * rhs)
                    })
                }
                ir::RuntimeFunction::DivideNatural => runtime_div_fn!(Value::Natural, 0),
                ir::RuntimeFunction::ModuloNatural => runtime_mod_fn!(Value::Natural, 0),
                ir::RuntimeFunction::PowerNatural => {
                    runtime_math_fn!(Value::Natural, (lhs, rhs) => async {
                        if lhs == 0 && rhs == 0 {
                            Err(Error::from("cannot raise zero to the power of zero"))
                        } else {
                            Ok(lhs.pow(rhs as u32))
                        }
                    })
                }
                ir::RuntimeFunction::AddByte => {
                    runtime_math_fn!(Value::Byte, (lhs, rhs) => async {
                        Ok(lhs + rhs)
                    })
                }
                ir::RuntimeFunction::SubtractByte => {
                    runtime_math_fn!(Value::Byte, (lhs, rhs) => async {
                        Ok(lhs - rhs)
                    })
                }
                ir::RuntimeFunction::MultiplyByte => {
                    runtime_math_fn!(Value::Byte, (lhs, rhs) => async {
                        Ok(lhs * rhs)
                    })
                }
                ir::RuntimeFunction::DivideByte => runtime_div_fn!(Value::Byte, 0),
                ir::RuntimeFunction::ModuloByte => runtime_mod_fn!(Value::Byte, 0),
                ir::RuntimeFunction::PowerByte => {
                    runtime_math_fn!(Value::Byte, (lhs, rhs) => async {
                        if lhs == 0 && rhs == 0 {
                            Err(Error::from("cannot raise zero to the power of zero"))
                        } else {
                            Ok(lhs.pow(rhs as u32))
                        }
                    })
                }
                ir::RuntimeFunction::AddSigned => {
                    runtime_math_fn!(Value::Signed, (lhs, rhs) => async {
                        Ok(lhs + rhs)
                    })
                }
                ir::RuntimeFunction::SubtractSigned => {
                    runtime_math_fn!(Value::Signed, (lhs, rhs) => async {
                        Ok(lhs - rhs)
                    })
                }
                ir::RuntimeFunction::MultiplySigned => {
                    runtime_math_fn!(Value::Signed, (lhs, rhs) => async {
                        Ok(lhs * rhs)
                    })
                }
                ir::RuntimeFunction::DivideSigned => runtime_div_fn!(Value::Signed, 0),
                ir::RuntimeFunction::ModuloSigned => runtime_mod_fn!(Value::Signed, 0),
                ir::RuntimeFunction::PowerSigned => runtime_math_fn!(Value::Signed, (lhs, rhs) => async {
                    if lhs == 0 && rhs == 0 {
                        Err(Error::from("cannot raise zero to the power of zero"))
                    } else {
                        Ok(lhs.pow(rhs as u32))
                    }
                }),
                ir::RuntimeFunction::AddUnsigned => {
                    runtime_math_fn!(Value::Unsigned, (lhs, rhs) => async {
                        Ok(lhs + rhs)
                    })
                }
                ir::RuntimeFunction::SubtractUnsigned => {
                    runtime_math_fn!(Value::Unsigned, (lhs, rhs) => async {
                        Ok(lhs - rhs)
                    })
                }
                ir::RuntimeFunction::MultiplyUnsigned => {
                    runtime_math_fn!(Value::Unsigned, (lhs, rhs) => async {
                        Ok(lhs * rhs)
                    })
                }
                ir::RuntimeFunction::DivideUnsigned => runtime_div_fn!(Value::Unsigned, 0),
                ir::RuntimeFunction::ModuloUnsigned => runtime_mod_fn!(Value::Unsigned, 0),
                ir::RuntimeFunction::PowerUnsigned => {
                    runtime_math_fn!(Value::Unsigned, (lhs, rhs) => async {
                        if lhs == 0 && rhs == 0 {
                            Err(Error::from("cannot raise zero to the power of zero"))
                        } else {
                            Ok(lhs.pow(rhs))
                        }
                    })
                }
                ir::RuntimeFunction::AddFloat => {
                    runtime_math_fn!(Value::Float, (lhs, rhs) => async {
                        Ok(lhs + rhs)
                    })
                }
                ir::RuntimeFunction::SubtractFloat => {
                    runtime_math_fn!(Value::Float, (lhs, rhs) => async {
                        Ok(lhs - rhs)
                    })
                }
                ir::RuntimeFunction::MultiplyFloat => {
                    runtime_math_fn!(Value::Float, (lhs, rhs) => async {
                        Ok(lhs * rhs)
                    })
                }
                ir::RuntimeFunction::DivideFloat => {
                    runtime_math_fn!(Value::Float, (lhs, rhs) => async {
                        Ok(lhs / rhs)
                    })
                }
                ir::RuntimeFunction::ModuloFloat => {
                    runtime_math_fn!(Value::Float, (lhs, rhs) => async {
                        Ok(lhs % rhs)
                    })
                }
                ir::RuntimeFunction::PowerFloat => {
                    runtime_math_fn!(Value::Float, (lhs, rhs) => async {
                        Ok(lhs.pow(rhs))
                    })
                }
                ir::RuntimeFunction::FloorFloat => {
                    runtime_math_fn!(Value::Double, (n) => async {
                        Ok(n.floor())
                    })
                }
                ir::RuntimeFunction::CeilFloat => {
                    runtime_math_fn!(Value::Double, (n) => async {
                        Ok(n.ceil())
                    })
                }
                ir::RuntimeFunction::SqrtFloat => {
                    runtime_math_fn!(Value::Double, (n) => async {
                        Ok(n.sqrt())
                    })
                }
                ir::RuntimeFunction::AddDouble => {
                    runtime_math_fn!(Value::Double, (lhs, rhs) => async {
                        Ok(lhs + rhs)
                    })
                }
                ir::RuntimeFunction::SubtractDouble => {
                    runtime_math_fn!(Value::Double, (lhs, rhs) => async {
                        Ok(lhs - rhs)
                    })
                }
                ir::RuntimeFunction::MultiplyDouble => {
                    runtime_math_fn!(Value::Double, (lhs, rhs) => async {
                        Ok(lhs * rhs)
                    })
                }
                ir::RuntimeFunction::DivideDouble => {
                    runtime_math_fn!(Value::Double, (lhs, rhs) => async {
                        Ok(lhs / rhs)
                    })
                }
                ir::RuntimeFunction::ModuloDouble => {
                    runtime_math_fn!(Value::Double, (lhs, rhs) => async {
                        Ok(lhs % rhs)
                    })
                }
                ir::RuntimeFunction::PowerDouble => {
                    runtime_math_fn!(Value::Double, (lhs, rhs) => async {
                        Ok(lhs.pow(rhs))
                    })
                }
                ir::RuntimeFunction::FloorDouble => {
                    runtime_math_fn!(Value::Double, (n) => async {
                        Ok(n.floor())
                    })
                }
                ir::RuntimeFunction::CeilDouble => {
                    runtime_math_fn!(Value::Double, (n) => async {
                        Ok(n.ceil())
                    })
                }
                ir::RuntimeFunction::SqrtDouble => {
                    runtime_math_fn!(Value::Double, (n) => async {
                        Ok(n.sqrt())
                    })
                }
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
                    runtime_cmp_fn!((Value::Float(lhs), Value::Float(rhs)) => async {
                        lhs.partial_cmp(&rhs).expect("unexpected NaN")
                    })
                }
                ir::RuntimeFunction::DoubleOrdering => {
                    runtime_cmp_fn!((Value::Double(lhs), Value::Double(rhs)) => async {
                        lhs.partial_cmp(&rhs).expect("unexpected NaN")
                    })
                }
                ir::RuntimeFunction::MakeMutable => runtime_fn!((value) => async {
                    Ok(Value::Mutable(Shared::new(value)))
                }),
                ir::RuntimeFunction::GetMutable => {
                    runtime_fn!((Value::Mutable(value)) => async {
                        Ok(value.lock().clone())
                    })
                }
                ir::RuntimeFunction::SetMutable => {
                    runtime_fn!((Value::Mutable(value), new_value) => async {
                        *value.lock() = new_value;
                        Ok(Value::Tuple(Vec::new()))
                    })
                }
                ir::RuntimeFunction::MakeEmptyList => {
                    assert!(inputs.is_empty());
                    Ok(Value::List(Default::default()))
                }
                ir::RuntimeFunction::ListFirst => runtime_fn!((Value::List(list)) => async {
                    Ok(match list.front() {
                        Some(first) => some(first.clone()),
                        None => none(),
                    })
                }),
                ir::RuntimeFunction::ListLast => runtime_fn!((Value::List(list)) => async {
                    Ok(match list.back() {
                        Some(last) => some(last.clone()),
                        None => none(),
                    })
                }),
                ir::RuntimeFunction::ListInitial => {
                    runtime_fn!((Value::List(mut list)) => async {
                        Ok(if list.is_empty() {
                            none()
                        } else {
                            some(Value::List(list.slice(0..(list.len() - 1))))
                        })
                    })
                }
                ir::RuntimeFunction::ListTail => {
                    runtime_fn!((Value::List(mut list)) => async {
                        Ok(if list.is_empty() {
                            none()
                        } else {
                            some(Value::List(list.slice(1..list.len())))
                        })
                    })
                }
                ir::RuntimeFunction::ListNth => {
                    runtime_fn!((Value::List(list), Value::Natural(index)) => async {
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
                    })
                }
                ir::RuntimeFunction::ListAppend => {
                    runtime_fn!((Value::List(mut list), value) => async {
                        list.push_back(value);
                        Ok(Value::List(list))
                    })
                }
                ir::RuntimeFunction::ListPrepend => {
                    runtime_fn!((Value::List(mut list), value) => async {
                        list.push_front(value);
                        Ok(Value::List(list))
                    })
                }
                ir::RuntimeFunction::ListInsert => {
                    runtime_fn!((Value::List(mut list), Value::Natural(index), value) => async {
                        let index = index as usize;
                        let index = if (0..list.len()).contains(&index) {
                            index
                        } else {
                            return Ok(error(Value::Marker));
                        };

                        list.insert(index, value);

                        Ok(ok(Value::List(list)))
                    })
                }
                ir::RuntimeFunction::ListRemove => {
                    runtime_fn!((Value::List(mut list), Value::Natural(index)) => async {
                        let index = index as usize;
                        let index = if (0..list.len()).contains(&index) {
                            index
                        } else {
                            return Ok(error(Value::Marker));
                        };

                        Ok(ok(list.remove(index)))
                    })
                }
                ir::RuntimeFunction::TextHeadTail => {
                    runtime_fn!((Value::Text(text)) => async {
                        match text.grapheme_indices(true).next() {
                            Some((_, head)) => {
                                let tail = &text[head.len()..];

                                Ok(some(Value::Tuple(vec![
                                    Value::Text(Arc::from(head)),
                                    Value::Text(Arc::from(tail)),
                                ])))
                            }
                            None => Ok(none()),
                        }
                    })
                }
            }
        })().await?;

        stack.push(value);

        Ok(())
    }
}
