use super::*;

pub type Intrinsic<R> = Box<
    dyn Fn(Vec<Value<R>>, Context<R>, TaskLocals<R>) -> BoxFuture<'static, Result<Value<R>>>
        + Send
        + Sync,
>;

pub fn intrinsics<R: Runtime>() -> HashMap<&'static str, Intrinsic<R>> {
    let mut intrinsics = HashMap::<&'static str, Intrinsic<R>>::new();

    macro_rules! intrinsic {
        ($name:literal, async |$($args:tt),*| $body:block) => {
            intrinsics.insert(
                $name,
                Box::new(|$($args),*| {
                    (async move $body).boxed()
                }),
            );
        };
    }

    intrinsic!("debug", async |inputs, context, _| {
        let value = inputs.into_iter().next().unwrap();
        context.debug(|| format!("{:#?}", value)).await;
        Ok(value)
    });

    intrinsic!("crash", async |inputs, _, _| {
        let message = inputs.into_iter().next().unwrap().to_text();
        return Err(Error(message));
    });

    intrinsic!("display", async |inputs, context, _| {
        let message = inputs.into_iter().next().unwrap().to_text();
        (context.io.display)(message).await?;

        Ok(Value::unit())
    });

    intrinsic!("prompt", async |inputs, context, task| {
        let mut inputs = inputs.into_iter();
        let message = inputs.next().unwrap().to_text();
        let validate = inputs.next().unwrap();

        let (tx, rx) = async_channel::unbounded();

        let tx = Arc::new(Mutex::new(Some(tx)));
        let validate = Arc::new({
            let tx = tx.clone();
            let task = task.clone();
            let context = context.clone();
            let validate = validate.clone();

            move |input| {
                let tx = tx.clone();
                let task = task.clone();
                let context = context.clone();
                let validate = validate.clone();

                async move {
                    let value = context
                        .call(validate, vec![Value::from_text(input)], task)
                        .await?
                        .to_maybe();

                    if let Some(value) = value {
                        if let Some(tx) = tx.lock_arc().await.take() {
                            tx.send(value).await.unwrap();
                        }

                        Ok(true)
                    } else {
                        Ok(false)
                    }
                }
                .boxed()
            }
        });

        (context.io.prompt)(message, validate).await?;

        let input = rx.recv().await.unwrap();

        Ok(input)
    });

    intrinsic!("choice", async |inputs, context, _| {
        let mut inputs = inputs.into_iter();

        let message = inputs.next().unwrap().to_text();

        let choices = inputs
            .next()
            .unwrap()
            .to_list()
            .into_iter()
            .map(|value| value.to_text())
            .collect::<Vec<_>>();

        let index = (context.io.choice)(message, choices).await?;

        Ok(Value::from_number(Decimal::from_usize(index)))
    });

    intrinsic!("runtime-message", async |inputs, context, task| {
        let mut inputs = inputs.into_iter();

        let message = inputs.next().unwrap().to_text();
        let value = inputs.next().unwrap();

        let result = (context.io.ui)(message, R::from_value(value, &task, &context).await).await?;

        Ok(R::to_value(result).await)
    });

    intrinsic!("with-continuation", async |inputs, context, _| {
        let callback = inputs.into_iter().next().unwrap();

        let (tx, rx) = async_channel::unbounded();

        let tx = Arc::new(Mutex::new(Some(tx)));
        R::run(async move {
            let continuation = Value::from_function(move |inputs, _, _| {
                let tx = tx.clone();

                async move {
                    let result = inputs.into_iter().next().unwrap();

                    if let Some(tx) = tx.lock().await.take() {
                        tx.send(result).await.unwrap();
                    } else {
                        // Ignore multiple calls to the continuation
                    }

                    Ok(Value::unit())
                }
                .boxed()
            });

            if let Err(_error) = context
                .call(callback, vec![continuation], TaskLocals::default())
                .await
            {
                // TODO: Propagate errors to the parent task
            }
        })
        .await;

        let result = rx.recv().await.unwrap();

        Ok(result)
    });

    intrinsic!("with-task-group", async |inputs, context, task| {
        let callback = inputs.into_iter().next().unwrap();

        let task_group = TaskGroup::default();

        context
            .call(
                callback,
                vec![Value::from_task_group(task_group.clone())],
                task,
            )
            .await?;

        task_group.await?;

        Ok(Value::unit())
    });

    intrinsic!("begin-task-group", async |_, _, _| {
        Ok(Value::from_task_group(TaskGroup::default()))
    });

    intrinsic!("end-task-group", async |inputs, _, _| {
        let task_group = inputs.into_iter().next().unwrap().to_task_group();
        task_group.await?;

        Ok(Value::unit())
    });

    intrinsic!("task", async |inputs, context, _| {
        let mut inputs = inputs.into_iter();
        let task_group = inputs.next().unwrap().to_task_group();
        let callback = inputs.next().unwrap();

        task_group
            .add(R::run(async move {
                if let Err(_error) = context.call(callback, vec![], TaskLocals::default()).await {
                    // TODO: Handle errors
                }
            }))
            .await;

        Ok(Value::unit())
    });

    intrinsic!("task-local-key", async |_, _, _| {
        Ok(Value::from_task_local_key(TaskLocalKey::new()))
    });

    intrinsic!("set-task-local", async |inputs, _, task| {
        let mut inputs = inputs.into_iter();
        let key = inputs.next().unwrap().to_task_local_key();
        let value = inputs.next().unwrap();

        task.lock_arc().await.insert(key, value);

        Ok(Value::unit())
    });

    intrinsic!("task-local", async |inputs, _, task| {
        let key = inputs.into_iter().next().unwrap().to_task_local_key();

        let value = task.lock_arc().await.get(&key).cloned();

        Ok(Value::from_maybe(value))
    });

    intrinsic!("in-background", async |inputs, context, _| {
        let callback = inputs.into_iter().next().unwrap();

        let task = {
            let context = context.clone();
            async move {
                if let Err(_error) = context.call(callback, vec![], TaskLocals::default()).await {
                    // TODO: Handle errors
                }
            }
        };

        context.background_tasks.add(task).await;

        Ok(Value::unit())
    });

    intrinsic!("delay", async |inputs, context, _| {
        let duration = inputs
            .into_iter()
            .next()
            .unwrap()
            .to_number()
            .ok_or_else(|| Error(String::from("delay duration may not be `undefined`")))?;

        let ms = (duration * dec!(1000)).trunc().to_u64().ok_or_else(|| {
            Error(String::from(
                "delay duration not representable as milliseconds",
            ))
        })?;

        (context.io.sleep)(ms).await?;

        Ok(Value::unit())
    });

    intrinsic!("number-to-text", async |inputs, _, _| {
        let number = inputs.into_iter().next().unwrap().to_number();

        let text = number
            .as_ref()
            .map_or_else(|| String::from("undefined"), ToString::to_string);

        Ok(Value::from_text(text))
    });

    intrinsic!("text-to-number", async |inputs, _, _| {
        let text = inputs.into_iter().next().unwrap().to_text();

        let number = if text == "undefined" {
            Some(Value::from_number(None))
        } else {
            text.parse::<Decimal>()
                .ok()
                .map(|n| Value::from_number(Some(n)))
        };

        Ok(Value::from_maybe(number))
    });

    intrinsic!("add-number", async |inputs, _, _| {
        let mut inputs = inputs.into_iter();

        let a = inputs.next().unwrap().to_number();
        let b = inputs.next().unwrap().to_number();

        Ok(match (a, b) {
            (Some(a), Some(b)) => Value::from_number(a.checked_add(b)),
            _ => Value::from_number(None),
        })
    });

    intrinsic!("subtract-number", async |inputs, _, _| {
        let mut inputs = inputs.into_iter();

        let a = inputs.next().unwrap().to_number();
        let b = inputs.next().unwrap().to_number();

        Ok(match (a, b) {
            (Some(a), Some(b)) => Value::from_number(a.checked_sub(b)),
            _ => Value::from_number(None),
        })
    });

    intrinsic!("multiply-number", async |inputs, _, _| {
        let mut inputs = inputs.into_iter();

        let a = inputs.next().unwrap().to_number();
        let b = inputs.next().unwrap().to_number();

        Ok(match (a, b) {
            (Some(a), Some(b)) => Value::from_number(a.checked_mul(b)),
            _ => Value::from_number(None),
        })
    });

    intrinsic!("divide-number", async |inputs, _, _| {
        let mut inputs = inputs.into_iter();

        let a = inputs.next().unwrap().to_number();
        let b = inputs.next().unwrap().to_number();

        Ok(match (a, b) {
            (Some(a), Some(b)) => Value::from_number(a.checked_div(b)),
            _ => Value::from_number(None),
        })
    });

    intrinsic!("remainder-number", async |inputs, _, _| {
        let mut inputs = inputs.into_iter();

        let a = inputs.next().unwrap().to_number();
        let b = inputs.next().unwrap().to_number();

        Ok(match (a, b) {
            (Some(a), Some(b)) => Value::from_number(a.checked_rem(b)),
            _ => Value::from_number(None),
        })
    });

    intrinsic!("power-number", async |inputs, _, _| {
        let mut inputs = inputs.into_iter();

        let base = inputs.next().unwrap().to_number();

        let exponent = inputs.next().unwrap().to_number();
        Ok(match (base, exponent) {
            (Some(base), Some(exponent)) => Value::from_number(base.checked_powd(exponent)),
            _ => Value::from_number(None),
        })
    });

    intrinsic!("floor-number", async |inputs, _, _| {
        let number = inputs.into_iter().next().unwrap().to_number();

        Ok(match number {
            Some(n) => Value::from_number(Some(n.floor())),
            None => Value::from_number(None),
        })
    });

    intrinsic!("ceiling-number", async |inputs, _, _| {
        let number = inputs.into_iter().next().unwrap().to_number();

        Ok(match number {
            Some(n) => Value::from_number(Some(n.ceil())),
            None => Value::from_number(None),
        })
    });

    intrinsic!("sqrt-number", async |inputs, _, _| {
        let number = inputs.into_iter().next().unwrap().to_number();

        Ok(match number {
            Some(n) => Value::from_number(n.sqrt()),
            None => Value::from_number(None),
        })
    });

    intrinsic!("sin", async |inputs, _, _| {
        let number = inputs.into_iter().next().unwrap().to_number();

        Ok(match number {
            Some(n) => Value::from_number(n.checked_sin()),
            None => Value::from_number(None),
        })
    });

    intrinsic!("cos", async |inputs, _, _| {
        let number = inputs.into_iter().next().unwrap().to_number();

        Ok(match number {
            Some(n) => Value::from_number(n.checked_cos()),
            None => Value::from_number(None),
        })
    });

    intrinsic!("tan", async |inputs, _, _| {
        let number = inputs.into_iter().next().unwrap().to_number();

        Ok(match number {
            Some(n) => Value::from_number(n.checked_tan()),
            None => Value::from_number(None),
        })
    });

    intrinsic!("negate-number", async |inputs, _, _| {
        let number = inputs.into_iter().next().unwrap().to_number();

        Ok(match number {
            Some(mut n) => {
                n.set_sign_negative(!n.is_sign_negative());
                Value::from_number(Some(n))
            }
            None => Value::from_number(None),
        })
    });

    intrinsic!("text-equality", async |inputs, _, _| {
        let mut inputs = inputs.into_iter();
        let a = inputs.next().unwrap().to_text();
        let b = inputs.next().unwrap().to_text();

        Ok(Value::from_bool(a == b))
    });

    intrinsic!("number-equality", async |inputs, _, _| {
        let mut inputs = inputs.into_iter();
        let a = inputs.next().unwrap().to_number();
        let b = inputs.next().unwrap().to_number();

        Ok(Value::from_bool(a == b))
    });

    intrinsic!("text-ordering", async |inputs, _, _| {
        let mut inputs = inputs.into_iter();
        let a = inputs.next().unwrap().to_text();
        let b = inputs.next().unwrap().to_text();

        Ok(Value::from_ordering(a.cmp(&b)))
    });

    intrinsic!("number-ordering", async |inputs, _, _| {
        let mut inputs = inputs.into_iter();
        let a = inputs.next().unwrap().to_number();
        let b = inputs.next().unwrap().to_number();

        Ok(Value::from_ordering(a.cmp(&b)))
    });

    intrinsic!("make-empty-list", async |_, _, _| {
        Ok(Value::from_list(Vec::new()))
    });

    intrinsic!("list-first", async |inputs, _, _| {
        let list = inputs.into_iter().next().unwrap().to_list();
        let first = list.first().cloned();
        Ok(Value::from_maybe(first))
    });

    intrinsic!("list-last", async |inputs, _, _| {
        let list = inputs.into_iter().next().unwrap().to_list();
        let last = list.last().cloned();
        Ok(Value::from_maybe(last))
    });

    intrinsic!("list-initial", async |inputs, _, _| {
        let list = inputs.into_iter().next().unwrap().to_list();

        Ok(if list.is_empty() {
            Value::none()
        } else {
            Value::from_some(Value::from_list(
                list.into_iter().rev().skip(1).rev().collect(),
            ))
        })
    });

    intrinsic!("list-tail", async |inputs, _, _| {
        let list = inputs.into_iter().next().unwrap().to_list();

        Ok(if list.is_empty() {
            Value::none()
        } else {
            Value::from_some(Value::from_list(list.into_iter().skip(1).collect()))
        })
    });

    intrinsic!("list-nth", async |inputs, _, _| {
        let mut inputs = inputs.into_iter();
        let list = inputs.next().unwrap().to_list();
        let index = inputs.next().unwrap().to_number();

        Ok(Value::from_maybe(
            index
                .and_then(|index| index.to_usize())
                .and_then(|index| list.get(index))
                .cloned(),
        ))
    });

    intrinsic!("list-append", async |inputs, _, _| {
        let mut inputs = inputs.into_iter();
        let mut list = inputs.next().unwrap().to_list();
        let value = inputs.next().unwrap();

        list.push(value);

        Ok(Value::from_list(list))
    });

    intrinsic!("list-prepend", async |inputs, _, _| {
        let mut inputs = inputs.into_iter();
        let mut list = inputs.next().unwrap().to_list();
        let value = inputs.next().unwrap();

        list.insert(0, value);

        Ok(Value::from_list(list))
    });

    intrinsic!("list-insert-at", async |inputs, _, _| {
        let mut inputs = inputs.into_iter();
        let mut list = inputs.next().unwrap().to_list();
        let index = inputs.next().unwrap().to_number();
        let value = inputs.next().unwrap();

        Ok(
            if let Some(index) = index
                .and_then(|index| index.to_usize())
                .filter(|index| *index <= list.len())
            {
                list.insert(index, value);
                Value::from_some(Value::from_list(list))
            } else {
                Value::none()
            },
        )
    });

    intrinsic!("list-remove-at", async |inputs, _, _| {
        let mut inputs = inputs.into_iter();
        let mut list = inputs.next().unwrap().to_list();
        let index = inputs.next().unwrap().to_number();

        Ok(
            if let Some(index) = index
                .and_then(|index| index.to_usize())
                .filter(|index| *index < list.len())
            {
                list.remove(index);
                Value::from_some(Value::from_list(list))
            } else {
                Value::none()
            },
        )
    });

    intrinsic!("list-count", async |inputs, _, _| {
        let list = inputs.into_iter().next().unwrap().to_list();

        let count = list.len();

        Ok(Value::from_number(Decimal::from_usize(count)))
    });

    intrinsic!("list-slice", async |inputs, _, _| {
        let mut inputs = inputs.into_iter();
        let list = inputs.next().unwrap().to_list();
        let start = inputs.next().unwrap().to_number();
        let end = inputs.next().unwrap().to_number();

        Ok(
            if let Some((start, end)) = start
                .and_then(|start| start.to_usize())
                .filter(|start| *start <= list.len())
                .zip(
                    end.and_then(|end| end.to_usize())
                        .filter(|end| *end <= list.len()),
                )
            {
                Value::from_some(Value::from_list(list[start..end].to_vec()))
            } else {
                Value::none()
            },
        )
    });

    intrinsic!("text-characters", async |inputs, _, _| {
        let text = inputs.into_iter().next().unwrap().to_text();

        Ok(Value::from_list(
            text.chars()
                .map(|c| Value::from_text(c.to_string()))
                .collect(),
        ))
    });

    intrinsic!("random-number", async |inputs, _, _| {
        let mut inputs = inputs.into_iter();
        let min = inputs.next().unwrap().to_number();
        let max = inputs.next().unwrap().to_number();

        Ok(
            if let Some((min, max)) = min.zip(max).filter(|(min, max)| min <= max) {
                if min == max {
                    Value::from_number(Some(min))
                } else {
                    Value::from_number(Some(rand::thread_rng().gen_range(min..max)))
                }
            } else {
                Value::from_number(None)
            },
        )
    });

    intrinsic!("undefined-number", async |_, _, _| {
        Ok(Value::from_number(None))
    });

    intrinsic!("make-hasher", async |_, _, _| { Ok(Value::hasher()) });

    intrinsic!("hash-number", async |inputs, _, _| {
        let mut inputs = inputs.into_iter();
        let mut hasher = inputs.next().unwrap().to_hasher();
        let number = inputs.next().unwrap().to_number();

        std::hash::Hash::hash(&number, &mut hasher);
        let hash = std::hash::Hasher::finish(&hasher);

        Ok(Value::from_number(Some(Decimal::from_u64(hash).unwrap())))
    });

    intrinsic!("hash-text", async |inputs, _, _| {
        let mut inputs = inputs.into_iter();
        let mut hasher = inputs.next().unwrap().to_hasher();
        let text = inputs.next().unwrap().to_text();

        std::hash::Hash::hash(&text, &mut hasher);
        let hash = std::hash::Hasher::finish(&hasher);

        Ok(Value::from_number(Some(Decimal::from_u64(hash).unwrap())))
    });

    intrinsics
}
