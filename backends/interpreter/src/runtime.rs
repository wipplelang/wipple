#![allow(clippy::redundant_closure_call)]

use crate::{Context, Error, Interpreter, IoRequest, Number, Stack, TaskGroup, UiHandle, Value};
use futures::channel::oneshot;
use itertools::Itertools;
use num_traits::{FromPrimitive, ToPrimitive};
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

        macro_rules! runtime_trig_fn {
            (Value::Number, $f:ident) => {
                runtime_fn!((Value::Variant(index, values)) => async {
                    let n = match values.into_iter().next().unwrap() {
                        Value::Number(Number::Decimal(n)) => n,
                        Value::Number(Number::Undefined) => return Ok(Value::Number(Number::Undefined)),
                        _ => unreachable!(),
                    };

                    let radians = match index.0 {
                        0 => n,
                        1 => n * Decimal::PI / Decimal::from_u8(180).unwrap(),
                        _ => unreachable!(),
                    };

                    Ok(Value::Number(radians.$f().map_or(Number::Undefined, Number::Decimal)))
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

        macro_rules! runtime_hash_fn {
            (Value::$ty:ident) => {
                runtime_fn!((Value::$ty(x)) => async {
                    use std::hash::{Hash, Hasher};

                    let mut hasher = std::collections::hash_map::DefaultHasher::default();
                    x.hash(&mut hasher);
                    let value = hasher.finish();

                    Ok(Value::Number(Number::Decimal(Decimal::from_u64(value).unwrap())))
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

                    Ok(Value::Number(Number::Decimal(Decimal::from_usize(index).unwrap())))
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
                ir::Intrinsic::Delay => runtime_fn!((Value::Number(ms)) => async {
                    let ms = match ms {
                        Number::Decimal(n) => n.to_u64().ok_or_else(|| Error::from("expected a whole number"))?,
                        Number::Undefined => return Err(Error::from("unexpected `undefined`")),
                    };

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
                ir::Intrinsic::TextToNumber => runtime_fn!((Value::Text(text)) => async {
                    Ok(maybe(text.parse().ok().map(Number::Decimal).map(Value::Number)))
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
                ir::Intrinsic::SinNumber => runtime_trig_fn!(Value::Number, checked_sin),
                ir::Intrinsic::CosNumber => runtime_trig_fn!(Value::Number, checked_cos),
                ir::Intrinsic::TanNumber => runtime_trig_fn!(Value::Number, checked_tan),
                ir::Intrinsic::NegateNumber => {
                    runtime_math_fn!(Value::Number, (n) => async {
                        Ok(match n {
                            Number::Decimal(n) => Number::Decimal(-n),
                            Number::Undefined => Number::Undefined,
                        })
                    })
                }
                ir::Intrinsic::TextEquality => runtime_eq_fn!(Value::Text),
                ir::Intrinsic::NumberEquality => runtime_eq_fn!(Value::Number),
                ir::Intrinsic::TextOrdering => runtime_cmp_fn!(Value::Text),
                ir::Intrinsic::NumberOrdering => {
                    runtime_cmp_fn!((Value::Number(lhs), Value::Number(rhs)) => async {
                        lhs.cmp(&rhs)
                    })
                },
                ir::Intrinsic::MakeReference => runtime_fn!((value) => async {
                    Ok(Value::Reference(Shared::new(value)))
                }),
                ir::Intrinsic::GetReference => {
                    runtime_fn!((Value::Reference(value)) => async {
                        Ok(value.lock().clone())
                    })
                }
                ir::Intrinsic::SetReference => {
                    runtime_fn!((Value::Reference(value), new_value) => async {
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
                    runtime_fn!((Value::List(list), Value::Number(index)) => async {
                        let index = match index {
                            Number::Decimal(n) => n.to_usize().ok_or_else(|| Error::from("expected a whole number"))?,
                            Number::Undefined => return Err(Error::from("unexpected `undefined`")),
                        };

                        Ok(match list.get(index) {
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
                    runtime_fn!((Value::List(mut list), Value::Number(index), value) => async {
                        let index = match index {
                            Number::Decimal(n) => n.to_usize().ok_or_else(|| Error::from("expected a whole number"))?,
                            Number::Undefined => return Err(Error::from("unexpected `undefined`")),
                        };

                        let index = if (0..=list.len()).contains(&index) {
                            index
                        } else {
                            return Ok(none());
                        };

                        list.insert(index, value);

                        Ok(some(Value::List(list)))
                    })
                }
                ir::Intrinsic::ListRemoveAt => {
                    runtime_fn!((Value::List(mut list), Value::Number(index)) => async {
                        let index = match index {
                            Number::Decimal(n) => n.to_usize().ok_or_else(|| Error::from("expected a whole number"))?,
                            Number::Undefined => return Err(Error::from("unexpected `undefined`")),
                        };

                        let index = if (0..list.len()).contains(&index) {
                            index
                        } else {
                            return Ok(none());
                        };

                        list.remove(index);

                        Ok(some(Value::List(list)))
                    })
                }
                ir::Intrinsic::ListCount => {
                    runtime_fn!((Value::List(list)) => async {
                        Ok(Value::Number(Number::Decimal(Decimal::from_usize(list.len()).unwrap())))
                    })
                }
                ir::Intrinsic::ListSlice => {
                    runtime_fn!((Value::List(mut list), Value::Number(start), Value::Number(end)) => async {
                        let start = match start {
                            Number::Decimal(n) => n.to_usize().ok_or_else(|| Error::from("expected a whole number"))?,
                            Number::Undefined => return Err(Error::from("unexpected `undefined`")),
                        };

                        let end = match end {
                            Number::Decimal(n) => n.to_usize().ok_or_else(|| Error::from("expected a whole number"))?,
                            Number::Undefined => return Err(Error::from("unexpected `undefined`")),
                        };

                        Ok(Value::List(list.slice(start..end)))
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
                ir::Intrinsic::UndefinedNumber => runtime_fn!(() => async {
                    Ok(Value::Number(Number::Undefined))
                }),
                ir::Intrinsic::MakeHasher => runtime_fn!(() => async {
                    Ok(Value::Hasher(Default::default()))
                }),
                ir::Intrinsic::HashIntoHasher => runtime_fn!((Value::Hasher(mut hasher), Value::Number(new)) => async {
                    std::hash::Hash::hash(&new, &mut hasher.0);
                    Ok(Value::Hasher(hasher))
                }),
                ir::Intrinsic::ValueOfHasher => runtime_fn!((Value::Hasher(hasher)) => async {
                    let hash = std::hash::Hasher::finish(&hasher.0);
                    Ok(Value::Number(Number::Decimal(Decimal::from_u64(hash).unwrap())))
                }),
                ir::Intrinsic::HashText => runtime_hash_fn!(Value::Text),
            }
        })().await?;

        stack.push(value);

        Ok(())
    }
}
