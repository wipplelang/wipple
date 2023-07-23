use futures::channel::oneshot;
use parking_lot::Mutex;
use send_wrapper::SendWrapper;
use std::{
    future::Future,
    pin::Pin,
    sync::{atomic::AtomicBool, Arc},
};
use wasm_bindgen::{prelude::*, JsCast};

// SAFETY: This is safe because Wasm is single-threaded
pub struct SendSyncFuture<F: ?Sized>(pub Pin<Box<F>>);
unsafe impl<F: ?Sized> Send for SendSyncFuture<F> {}
unsafe impl<F: ?Sized> Sync for SendSyncFuture<F> {}

impl<F: Future + ?Sized> Future for SendSyncFuture<F> {
    type Output = F::Output;

    fn poll(
        mut self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        self.0.as_mut().poll(cx)
    }
}

pub struct CancellableFuture<Fut> {
    token: Arc<AtomicBool>,
    fut: Pin<Box<Fut>>,
}

impl<Fut> CancellableFuture<Fut> {
    pub fn new(token: Arc<AtomicBool>, fut: Fut) -> Self {
        CancellableFuture {
            token,
            fut: Box::pin(fut),
        }
    }
}

impl<Fut: Future> Future for CancellableFuture<Fut> {
    type Output = Option<Fut::Output>;

    fn poll(self: Pin<&mut Self>, cx: &mut std::task::Context) -> std::task::Poll<Self::Output> {
        let cancelled = self.token.load(std::sync::atomic::Ordering::SeqCst);

        if cancelled {
            std::task::Poll::Ready(None)
        } else {
            Pin::new(&mut self.get_mut().fut).poll(cx).map(Some)
        }
    }
}

#[wasm_bindgen(js_name = "runProgram")]
pub fn run_program(
    program: JsValue,
    handle_io: js_sys::Function,
    callback: js_sys::Function,
) -> JsValue {
    let program = match serde_wasm_bindgen::from_value(program) {
        Ok(program) => program,
        Err(_) => {
            callback
                .call1(&JsValue::NULL, &String::from("bad program format").into())
                .unwrap();

            return JsValue::NULL;
        }
    };

    run(program, handle_io, callback)
}

pub fn run(
    program: wipple_interpreter_backend::Program,
    handle_io: js_sys::Function,
    callback: js_sys::Function,
) -> JsValue {
    web_sys::console::log_1(&program.to_string().into());

    let handle_io = Arc::new(Mutex::new(SendWrapper::new(handle_io)));

    let tasks: Arc<Mutex<Vec<oneshot::Receiver<()>>>> = Default::default();
    let cancel_token = Arc::new(AtomicBool::new(false));

    let send_display = {
        let handle_io = handle_io.clone();

        move |text: String, callback: Box<dyn FnOnce() + Send>| {
            #[wasm_bindgen(getter_with_clone)]
            pub struct DisplayRequest {
                pub kind: String,
                pub text: String,
                pub callback: JsValue,
            }

            let request = DisplayRequest {
                kind: String::from("display"),
                text,
                callback: Closure::once(callback).into_js_value(),
            };

            handle_io
                .lock()
                .call1(&JsValue::NULL, &request.into())
                .unwrap();
        }
    };

    let interpreter = wipple_interpreter_backend::Interpreter::new({
        let send_display = send_display.clone();
        let tasks = tasks.clone();
        let cancel_token = cancel_token.clone();

        move |request| {
            let handle_io = handle_io.clone();
            let send_display = send_display.clone();
            let tasks = tasks.clone();
            let cancel_token = cancel_token.clone();

            Box::pin(async move {
                match request {
                    wipple_interpreter_backend::IoRequest::Display(_, text, callback) => {
                        send_display(text.to_string(), callback);
                    }
                    wipple_interpreter_backend::IoRequest::Prompt(
                        _,
                        prompt,
                        input_tx,
                        valid_rx,
                        callback,
                    ) => {
                        let prompt = prompt.to_string();
                        let input_tx = Arc::new(Mutex::new(input_tx));
                        let valid_rx = Arc::new(Mutex::new(valid_rx));

                        #[wasm_bindgen(getter_with_clone)]
                        pub struct PromptRequest {
                            pub kind: String,
                            pub prompt: String,
                            pub send_input: JsValue,
                            pub recv_valid: JsValue,
                            pub callback: JsValue,
                        }

                        let send_input =
                            Closure::<dyn Fn(String) -> js_sys::Promise>::new(move |input| {
                                let input_tx = input_tx.clone();

                                wasm_bindgen_futures::future_to_promise(async move {
                                    input_tx.lock().send(input).await.unwrap();
                                    Ok(JsValue::UNDEFINED)
                                })
                            });

                        let recv_valid = Closure::<dyn Fn() -> js_sys::Promise>::new(move || {
                            let valid_rx = valid_rx.clone();

                            wasm_bindgen_futures::future_to_promise(async move {
                                let valid = valid_rx.lock().recv().await.unwrap();
                                Ok(valid.into())
                            })
                        });

                        let request = PromptRequest {
                            kind: String::from("prompt"),
                            prompt,
                            send_input: send_input.into_js_value(),
                            recv_valid: recv_valid.into_js_value(),
                            callback: Closure::once(callback).into_js_value(),
                        };

                        handle_io
                            .lock()
                            .call1(&JsValue::NULL, &request.into())
                            .unwrap();
                    }
                    wipple_interpreter_backend::IoRequest::Choice(_, prompt, choices, callback) => {
                        #[wasm_bindgen(getter_with_clone)]
                        pub struct ChoiceRequest {
                            pub kind: String,
                            pub prompt: String,
                            pub choices: Vec<JsValue>,
                            pub callback: JsValue,
                        }

                        let request = ChoiceRequest {
                            kind: String::from("choice"),
                            prompt: prompt.to_string(),
                            choices: choices.into_iter().map(From::from).collect(),
                            callback: Closure::once(callback).into_js_value(),
                        };

                        handle_io
                            .lock()
                            .call1(&JsValue::NULL, &request.into())
                            .unwrap();
                    }
                    wipple_interpreter_backend::IoRequest::Ui(interpreter, url, completion) => {
                        #[wasm_bindgen(getter_with_clone)]
                        pub struct LoadUiRequest {
                            pub kind: String,
                            pub url: String,
                            pub callback: JsValue,
                        }

                        let request = LoadUiRequest {
                            kind: String::from("loadUi"),
                            url: url.to_string(),
                            callback: {
                                let handle_io = handle_io.clone();

                                Closure::once_into_js(move |id: String, handle_message: JsValue| {
                                    let handle_message =
                                        match handle_message.dyn_into::<js_sys::Function>() {
                                            Ok(handle) => {
                                                Arc::new(Mutex::new(SendWrapper::new(handle)))
                                            }
                                            Err(_) => panic!("message handler must be a function"),
                                        };

                                    wasm_bindgen_futures::future_to_promise(async move {
                                        // We can ignore the result of calling `completion` here
                                        // because if the runner fails, the whole program will be
                                        // reset
                                        let _ = completion(
                                            Box::new(move |message, value, context, callback| {
                                                let callback = Closure::once({
                                                    let interpreter = interpreter.clone();
                                                    let context = context.deep_clone();

                                                    move |value| {
                                                        callback(Ok(js_to_wipple(
                                                            &interpreter,
                                                            &context,
                                                            value,
                                                        )))
                                                        .unwrap();
                                                    }
                                                })
                                                .into_js_value();

                                                handle_message
                                                    .lock()
                                                    .call3(
                                                        &JsValue::NULL,
                                                        &JsValue::from_str(&message),
                                                        &wipple_to_js(
                                                            &interpreter,
                                                            &context,
                                                            value,
                                                        ),
                                                        &callback,
                                                    )
                                                    .unwrap();
                                            }),
                                            Box::new(move || {
                                                #[wasm_bindgen(getter_with_clone)]
                                                pub struct FinishUiRequest {
                                                    pub kind: String,
                                                    pub id: String,
                                                }

                                                let request = FinishUiRequest {
                                                    kind: String::from("finishUi"),
                                                    id,
                                                };

                                                handle_io
                                                    .lock()
                                                    .call1(&JsValue::NULL, &request.into())
                                                    .unwrap();
                                            }),
                                        )
                                        .await;

                                        Ok(JsValue::UNDEFINED)
                                    })
                                })
                            },
                        };

                        handle_io
                            .lock()
                            .call1(&JsValue::NULL, &request.into())
                            .unwrap();
                    }
                    wipple_interpreter_backend::IoRequest::Sleep(_, duration, callback) => {
                        let global = js_sys::global()
                            .dyn_into::<web_sys::WorkerGlobalScope>()
                            .unwrap();

                        let callback = Closure::once(callback);

                        global
                            .set_timeout_with_callback_and_timeout_and_arguments_0(
                                callback.as_ref().unchecked_ref(),
                                duration.as_millis() as i32,
                            )
                            .unwrap();

                        callback.forget();
                    }
                    wipple_interpreter_backend::IoRequest::Schedule(_, fut, callback) => {
                        let (completion_tx, completion_rx) = oneshot::channel();

                        tasks.lock().push(completion_rx);

                        let fut = CancellableFuture::new(cancel_token, async move {
                            if let Err(error) = fut.await {
                                let (completion_tx, completion_rx) = oneshot::channel();
                                send_display(
                                    format!("fatal error: {error}"),
                                    Box::new(|| completion_tx.send(()).unwrap()),
                                );

                                completion_rx.await.unwrap();
                            }

                            completion_tx
                                .send(())
                                .expect("failed to signal task completion");
                        });

                        callback(Box::new(move || {
                            wasm_bindgen_futures::spawn_local(async move {
                                fut.await;
                            })
                        }))
                    }
                }

                Ok(())
            })
        }
    });

    let future = CancellableFuture::new(cancel_token.clone(), {
        let callback = callback.clone();

        async move {
            let result = interpreter.run(&program).await;

            if let Err(error) = result {
                let (completion_tx, completion_rx) = oneshot::channel();
                send_display(
                    format!("fatal error: {error}"),
                    Box::new(|| completion_tx.send(()).unwrap()),
                );

                completion_rx.await.unwrap();
            }

            loop {
                let completion_rx = match tasks.lock().pop() {
                    Some(rx) => rx,
                    None => break,
                };

                let _ = completion_rx.await;
            }

            callback.call1(&JsValue::NULL, &JsValue::UNDEFINED).unwrap();
        }
    });

    wasm_bindgen_futures::spawn_local(async move {
        let panic_hook = std::panic::take_hook();

        let callback = SendWrapper::new(callback);

        std::panic::set_hook(Box::new(move |info| {
            callback
                .call1(&JsValue::NULL, &info.to_string().into())
                .unwrap();

            panic_hook(info);
        }));

        future.await;
    });

    Closure::once_into_js(move || {
        cancel_token.store(true, std::sync::atomic::Ordering::SeqCst);
    })
}

fn wipple_to_js(
    interpreter: &wipple_interpreter_backend::Interpreter,
    context: &wipple_interpreter_backend::Context,
    value: wipple_interpreter_backend::Value,
) -> JsValue {
    match value {
        wipple_interpreter_backend::Value::Marker => {
            panic!("marker values may not be sent to JavaScript")
        }
        wipple_interpreter_backend::Value::Number(n) => {
            JsValue::from_f64(n.try_into().expect("number out of bounds"))
        }
        wipple_interpreter_backend::Value::Integer(_)
        | wipple_interpreter_backend::Value::Natural(_)
        | wipple_interpreter_backend::Value::Byte(_)
        | wipple_interpreter_backend::Value::Signed(_)
        | wipple_interpreter_backend::Value::Unsigned(_)
        | wipple_interpreter_backend::Value::Float(_)
        | wipple_interpreter_backend::Value::Double(_) => {
            panic!("only numbers of type `Number` may be sent to JavaScript")
        }
        wipple_interpreter_backend::Value::Text(s) => JsValue::from_str(&s),
        wipple_interpreter_backend::Value::Function(scope, label) => {
            let interpreter = interpreter.clone();
            let context = context.deep_clone();

            Closure::<dyn Fn(JsValue) -> JsValue>::new(move |input| {
                let interpreter = interpreter.clone();
                let scope = scope.clone();
                let context = context.deep_clone();

                let input = js_to_wipple(&interpreter, &context, input);

                wasm_bindgen_futures::future_to_promise(async move {
                    let output = interpreter
                        .call_function(label, scope, &context, input)
                        .await?;

                    Ok(wipple_to_js(&interpreter, &context, output))
                })
                .into()
            })
            .into_js_value()
        }
        wipple_interpreter_backend::Value::NativeFunction(f) => {
            let f = f.clone();
            let interpreter = interpreter.clone();
            let context = context.deep_clone();

            Closure::<dyn Fn(JsValue) -> JsValue>::new(move |input| {
                let f = f.clone();
                let interpreter = interpreter.clone();
                let context = context.deep_clone();

                let input = js_to_wipple(&interpreter, &context, input);

                wasm_bindgen_futures::future_to_promise(async move {
                    let output = f(input).await?;
                    Ok(wipple_to_js(&interpreter, &context, output))
                })
                .into()
            })
            .into_js_value()
        }
        wipple_interpreter_backend::Value::Variant(_, _) => {
            panic!("enumeration values may not be sent to JavaScript")
        }
        wipple_interpreter_backend::Value::Mutable(_) => {
            panic!("`Mutable` values may not be sent to JavaScript")
        }
        wipple_interpreter_backend::Value::List(_) => {
            panic!("`List` values may not be sent to JavaScript")
        }
        wipple_interpreter_backend::Value::Structure(_) => {
            panic!("structure values may not be sent to JavaScript")
        }
        wipple_interpreter_backend::Value::Tuple(elements) => {
            if elements.is_empty() {
                JsValue::NULL
            } else {
                panic!("tuple values may not be sent to JavaScript");
            }
        }
        wipple_interpreter_backend::Value::UiHandle(_) => {
            panic!("UI handles may not be sent to JavaScript")
        }
        wipple_interpreter_backend::Value::TaskGroup(_) => {
            panic!("task groups may not be sent to JavaScript")
        }
    }
}

fn js_to_wipple(
    interpreter: &wipple_interpreter_backend::Interpreter,
    context: &wipple_interpreter_backend::Context,
    value: JsValue,
) -> wipple_interpreter_backend::Value {
    if value.is_null() | value.is_undefined() {
        wipple_interpreter_backend::Value::Tuple(Vec::new())
    } else if let Some(n) = value.as_f64() {
        wipple_interpreter_backend::Value::Number(n.try_into().expect("number out of bounds"))
    } else if let Some(s) = value.as_string() {
        wipple_interpreter_backend::Value::Text(Arc::from(s))
    } else if let Some(f) = value.dyn_ref::<js_sys::Function>() {
        let f = Arc::new(Mutex::new(SendWrapper::new(f.clone())));
        let interpreter = Arc::new(Mutex::new(SendWrapper::new(interpreter.clone())));
        let context = context.clone();

        wipple_interpreter_backend::Value::NativeFunction(Arc::new(move |input| {
            let f = f.clone();
            let interpreter = interpreter.clone();
            let context = context.deep_clone();

            Box::pin(SendSyncFuture(Box::pin(async move {
                let input = wipple_to_js(&interpreter.lock(), &context, input);

                let mut output = f
                    .lock()
                    .call1(&JsValue::NULL, &input)
                    .expect("uncaught exception");

                let output = loop {
                    let promise = match output.dyn_into::<js_sys::Promise>() {
                        Ok(promise) => promise,
                        Err(value) => break value,
                    };

                    output = wasm_bindgen_futures::JsFuture::from(promise)
                        .await
                        .expect("uncaught exception");
                };

                Ok(js_to_wipple(&interpreter.lock(), &context, output))
            })))
        }))
    } else {
        panic!("JavaScript value cannot be sent to Wipple")
    }
}
