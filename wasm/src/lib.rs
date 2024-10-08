//! JavaScript bridge for Wipple.

#![cfg(target_arch = "wasm32")]
#![allow(missing_docs)]

mod util;

use async_lock::Mutex;
use futures::{
    channel::oneshot::{self, Canceled},
    future::{self, BoxFuture},
    Future, FutureExt,
};
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    mem,
    sync::{Arc, Once},
};
use util::{is_dev, OrThrowExt, SendWrapper, VariadicFunction};
use wasm_bindgen::prelude::*;
use web_sys::js_sys;
use wipple_driver::util::lazy_static::lazy_static;
use wipple_interpreter::rust_decimal::prelude::*;

fn to_value<T: Serialize>(value: &T) -> Result<JsValue, serde_wasm_bindgen::Error> {
    let serializer = serde_wasm_bindgen::Serializer::json_compatible();
    value.serialize(&serializer)
}

fn from_value<T: for<'de> Deserialize<'de>>(
    value: JsValue,
) -> Result<T, serde_wasm_bindgen::Error> {
    serde_wasm_bindgen::from_value(value)
}

lazy_static! {
    static ref RENDERS: Mutex<HashMap<String, wipple_render::Render>> = Default::default();
}

async fn render_for(id: String) -> wipple_render::Render {
    let render = RENDERS
        .lock()
        .await
        .entry(id)
        .or_insert_with(wipple_render::Render::new)
        .clone();

    render
}

lazy_static! {
    static ref CANCEL_TOKENS: Mutex<HashMap<String, oneshot::Sender<()>>> = Default::default();
}

async fn run_on_thread<T: Send + 'static, Fut: Future<Output = T>>(
    id: impl ToString,
    f: impl FnOnce() -> Fut + Send + 'static,
) -> Result<T, Canceled> {
    use std::collections::hash_map::Entry;

    let id = id.to_string();

    let (cancel_tx, mut cancel_rx) = oneshot::channel();

    // Cancel any previous thread with the same ID
    match CANCEL_TOKENS.lock().await.entry(id.clone()) {
        Entry::Occupied(mut entry) => {
            let prev_cancel = entry.insert(cancel_tx);
            prev_cancel.send(()).or_warn("failed to cancel thread");
        }
        Entry::Vacant(entry) => {
            entry.insert(cancel_tx);
        }
    };

    let (tx, rx) = async_channel::unbounded();

    // In production, use the entrypoint copied over to the `public` folder
    // during build (see `worker_entrypoint.js`)
    let mut thread = wasm_thread::Builder::new();
    if !is_dev() {
        let origin = js_sys::eval("window.location.origin")
            .or_throw("failed to get origin")
            .as_string()
            .expect_throw("origin is not a string");

        thread = thread
            .wasm_bindgen_shim_url(format!("{origin}/playground/assets/worker_entrypoint.js"));
    }

    thread
        .spawn_async({
            let id = id.clone();
            move || async move {
                let result = futures::select! {
                    result = f().fuse() => {
                        CANCEL_TOKENS.lock().await.remove(&id);
                        Ok(result)
                    },
                    _ = cancel_rx => Err(Canceled),
                };

                tx.send(result).await.or_warn("tx failed");
            }
        })
        .or_throw("failed to spawn thread");

    rx.recv().await.or_throw("rx failed")
}

async fn stop_thread(id: &str) -> bool {
    if let Some(cancel) = CANCEL_TOKENS.lock().await.remove(id) {
        cancel.send(()).or_throw("failed to cancel thread");
        true
    } else {
        false
    }
}

#[wasm_bindgen(start)]
pub fn init() {
    static HOOK: Once = Once::new();
    HOOK.call_once(|| {
        std::panic::set_hook(Box::new(|info| {
            let _handle = wasm_main_executor::spawn(async move {
                // If a thread panics, cancel everything to prevent hangs
                for cancel in mem::take(&mut *CANCEL_TOKENS.lock().await).into_values() {
                    cancel.send(()).or_throw("failed to cancel thread");
                }
            });

            console_error_panic_hook::hook(info);
        }));
    });

    wasm_main_executor::initialize().unwrap();
}

#[derive(Clone, Default)]
struct Dependencies {
    interfaces: Vec<wipple_driver::Interface>,
    libraries: Vec<wipple_driver::Library>,
}

lazy_static! {
    static ref DEPENDENCIES: Mutex<HashMap<String, Dependencies>> = Default::default();
    static ref SOURCES: Mutex<HashMap<String, String>> = Default::default();
    static ref EXECUTABLES: Mutex<HashMap<String, wipple_driver::Executable>> = Default::default();
}

#[derive(Deserialize)]
pub struct InitializeOptions {
    pub id: String,
    pub interfaces: Vec<wipple_driver::Interface>,
    pub libraries: Vec<wipple_driver::Library>,
}

#[derive(Serialize)]
pub struct InitializeResult {}

#[wasm_bindgen]
pub async fn initialize(options: JsValue) -> JsValue {
    let options =
        from_value::<InitializeOptions>(options).or_throw("failed to deserialize options");

    DEPENDENCIES.lock().await.insert(
        options.id,
        Dependencies {
            interfaces: options.interfaces,
            libraries: options.libraries,
        },
    );

    let result = InitializeResult {};

    to_value(&result).or_throw("failed to serialize result")
}

#[derive(Deserialize)]
pub struct CompileOptions {
    pub id: String,
    pub path: String,
    pub code: String,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CompileResult {
    pub success: bool,
    pub driver_diagnostics:
        Vec<wipple_driver::util::WithInfo<wipple_driver::Info, wipple_driver::Diagnostic>>,
    pub diagnostics: Vec<wipple_render::RenderedDiagnostic>,
}

#[wasm_bindgen]
pub async fn compile(options: JsValue) -> JsValue {
    let options = from_value::<CompileOptions>(options).or_throw("failed to deserialize options");

    let result = run_on_thread(format!("compile-{}", options.id), move || async move {
        SOURCES
            .lock()
            .await
            .insert(options.id.clone(), options.code.clone());

        let sources = vec![wipple_driver::File {
            path: options.path.clone(),
            visible_path: options.path,
            code: options.code,
        }];

        let dependencies = DEPENDENCIES
            .lock()
            .await
            .entry(options.id.clone())
            .or_default()
            .clone();

        let result = wipple_driver::compile(sources, dependencies.interfaces.clone());

        render_for(options.id.clone()).await.update(
            dependencies
                .interfaces
                .into_iter()
                .chain([result.interface])
                .collect(),
            Some(result.library.clone()),
            Some(result.ide),
        );

        let compiled = Mutex::new(true);
        let rendered_diagnostics = future::join_all(result.diagnostics.iter().map(|diagnostic| {
            let compiled = &compiled;
            let id = &options.id;
            async move {
                let rendered_diagnostic =
                    render_for(id.clone()).await.render_diagnostic(diagnostic)?;

                if let wipple_render::RenderedDiagnosticSeverity::Error =
                    rendered_diagnostic.severity
                {
                    *compiled.lock().await = false;
                }

                Some(rendered_diagnostic)
            }
            .boxed()
        }))
        .await
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();

        let executable = if compiled.into_inner() {
            wipple_driver::link(
                [result.library]
                    .into_iter()
                    .chain(dependencies.libraries)
                    .collect(),
            )
        } else {
            None
        };

        let success = executable.is_some();

        let mut exectutables = EXECUTABLES.lock().await;
        if let Some(executable) = executable {
            exectutables.insert(options.id, executable);
        } else {
            exectutables.remove(&options.id);
        }

        CompileResult {
            success,
            driver_diagnostics: result.diagnostics,
            diagnostics: rendered_diagnostics,
        }
    })
    .await;

    match result {
        Ok(result) => to_value(&result).or_throw("failed to serialize result"),
        Err(Canceled) => JsValue::UNDEFINED,
    }
}

#[derive(Deserialize)]
pub struct FormatOptions {
    pub code: String,
}

#[derive(Serialize)]
pub struct FormatResult {
    pub code: String,
}

#[wasm_bindgen]
pub async fn format(options: JsValue) -> JsValue {
    let options = from_value::<FormatOptions>(options).or_throw("failed to deserialize options");

    let formatted = wipple_driver::format(&options.code);
    let result = FormatResult { code: formatted };

    to_value(&result).or_throw("failed to serialize result")
}

#[derive(Deserialize)]
pub struct HighlightsOptions {
    pub id: String,
}

#[derive(Serialize)]
pub struct HighlightsResult {
    pub highlights: HashMap<String, wipple_render::RenderedHighlight>,
}

#[wasm_bindgen]
pub async fn highlights(options: JsValue) -> JsValue {
    let options =
        from_value::<HighlightsOptions>(options).or_throw("failed to deserialize options");

    let result = run_on_thread(format!("highlights-{}", options.id), move || async move {
        let interfaces = DEPENDENCIES
            .lock()
            .await
            .entry(options.id.clone())
            .or_default()
            .interfaces
            .clone();

        let mut highlights = HashMap::new();
        for interface in interfaces {
            let render = wipple_render::Render::new();
            render.update(vec![interface], None, None);

            for (name, paths) in render.get_interface().top_level {
                if paths.len() > 1 {
                    continue;
                }

                for path in paths {
                    if let Some(declaration) = render.get_declaration_from_path(&path.item) {
                        if let Some(highlight) = render.render_highlight(&declaration) {
                            highlights.insert(name, highlight);
                            break; // needed to move `name` into `highlights` above
                        }
                    }
                }
            }
        }

        HighlightsResult { highlights }
    })
    .await;

    match result {
        Ok(result) => to_value(&result).or_throw("failed to serialize result"),
        Err(Canceled) => JsValue::UNDEFINED,
    }
}

#[derive(Deserialize)]
pub struct HelpOptions {
    pub id: String,
    pub path: String,
    pub position: u32,
}

#[derive(Serialize)]
pub struct HelpResult {
    help: Option<Help>,
}

#[derive(Serialize)]
pub struct Help {
    pub name: Option<String>,
    pub declaration: Option<String>,
    pub docs: String,
    pub example: Option<String>,
}

#[wasm_bindgen]
pub async fn help(options: JsValue) -> JsValue {
    let options = from_value::<HelpOptions>(options).or_throw("failed to deserialize options");

    let result = run_on_thread(format!("help-{}", options.id), move || async move {
        let render = render_for(options.id.clone()).await;

        let help = async {
            let declaration = async {
                if let Some(declaration) = render
                    .get_path_at_cursor(&options.path, options.position)
                    .and_then(|path| render.get_declaration_from_path(&path.item))
                {
                    return Some(declaration);
                }

                let code = SOURCES.lock().await.get(&options.id).cloned()?;

                let symbol_range =
                    wipple_driver::util::find_word_boundary(&code, options.position as usize);

                let symbol = &code[symbol_range];

                render.get_declaration_for_syntax(symbol)
            }
            .await?;

            let documentation = render.render_documentation(&declaration)?;

            let declaration_string = render.render_declaration(&declaration);

            Some(Help {
                name: declaration.item.name,
                declaration: declaration_string,
                docs: documentation.docs,
                example: documentation.example,
            })
        }
        .await;

        HelpResult { help }
    })
    .await;

    match result {
        Ok(result) => to_value(&result).or_throw("failed to serialize result"),
        Err(Canceled) => JsValue::UNDEFINED,
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GetIntelligentFixOptions {
    pub id: String,
    pub path: String,
    pub code: String,
    pub diagnostic: wipple_driver::util::WithInfo<wipple_driver::Info, wipple_driver::Diagnostic>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct GetIntelligentFixResult {
    fix: Option<IntelligentFix>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct IntelligentFix {
    pub message: String,
    pub fixed_code: String,
}

#[wasm_bindgen(js_name = "getIntelligentFix")]
pub async fn get_intelligent_fix(options: JsValue) -> JsValue {
    let options =
        from_value::<GetIntelligentFixOptions>(options).or_throw("failed to deserialize options");

    let result = run_on_thread(
        format!("get_intelligent_fix-{}", options.id),
        move || async move {
            let interfaces = DEPENDENCIES
                .lock()
                .await
                .entry(options.id.clone())
                .or_default()
                .interfaces
                .clone();

            let sources = vec![wipple_driver::File {
                path: options.path.clone(),
                visible_path: options.path,
                code: options.code,
            }];

            let fix_result = wipple_driver::fix_file(options.diagnostic, sources, interfaces);

            let (fix, fixed_code) = match fix_result {
                Some((fix, code)) => (fix, code),
                None => return GetIntelligentFixResult { fix: None },
            };

            let render = render_for(options.id).await;

            let message = render.render_fix(&fix);

            GetIntelligentFixResult {
                fix: Some(IntelligentFix {
                    message,
                    fixed_code,
                }),
            }
        },
    )
    .await;

    match result {
        Ok(result) => to_value(&result).or_throw("failed to serialize result"),
        Err(Canceled) => JsValue::UNDEFINED,
    }
}

#[derive(Deserialize)]
pub struct RunOptions {
    pub id: String,
    pub handlers: Arc<SendWrapper<RunHandlers>>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RunHandlers {
    #[serde(with = "serde_wasm_bindgen::preserve")]
    pub display: js_sys::Function,

    #[serde(with = "serde_wasm_bindgen::preserve")]
    pub prompt: js_sys::Function,

    #[serde(with = "serde_wasm_bindgen::preserve")]
    pub choice: js_sys::Function,

    #[serde(with = "serde_wasm_bindgen::preserve")]
    pub ui: js_sys::Function,
}

#[derive(Serialize)]
pub struct RunResult {
    pub error: Option<String>,
}

fn run_thread_id(id: &str) -> String {
    format!("run-{}", id)
}

#[wasm_bindgen]
pub async fn run(options: JsValue) -> JsValue {
    let options = from_value::<RunOptions>(options).or_throw("failed to deserialize options");

    #[derive(Clone)]
    struct Value(SendWrapper<JsValue>);

    impl From<JsValue> for Value {
        fn from(value: JsValue) -> Self {
            Value(SendWrapper::new(value))
        }
    }

    impl From<Value> for JsValue {
        fn from(value: Value) -> Self {
            value.0.into_inner()
        }
    }

    lazy_static! {
        static ref FUNCTIONS: Mutex<Vec<wipple_interpreter::StoredFunction<Runtime>>> =
            Default::default();
    }

    struct Runtime;

    impl wipple_interpreter::Runtime for Runtime {
        type Value = Value;
        type JoinHandle = BoxFuture<'static, ()>;

        fn run(future: impl future::Future<Output = ()> + Send + 'static) -> Self::JoinHandle {
            let (tx, rx) = async_channel::unbounded();
            wasm_bindgen_futures::spawn_local(async move {
                future.await;
                tx.send(()).await.or_warn("tx failed");
            });

            async move { rx.recv().await.or_throw("rx failed") }.boxed()
        }

        fn from_value(
            value: wipple_interpreter::Value<Self>,
            task: &wipple_interpreter::TaskLocals<Self>,
            context: &wipple_interpreter::Context<Self>,
        ) -> impl Future<Output = Self::Value> + Send {
            let task = task.clone();
            let context = context.clone();

            wasm_main_executor::spawn(async move {
                Value(SendWrapper::new(match value {
                    wipple_interpreter::Value::Marker => JsValue::NULL,
                    wipple_interpreter::Value::Number(number) => {
                        JsValue::from_f64(number.and_then(|n| n.to_f64()).unwrap_or(f64::NAN))
                    }
                    wipple_interpreter::Value::Text(text) => JsValue::from_str(&text),
                    wipple_interpreter::Value::Function { .. } => {
                        let mut functions = FUNCTIONS.lock().await;

                        let index = functions.len();
                        functions.push(wipple_interpreter::StoredFunction::new(
                            task.clone(),
                            context.clone(),
                            move |inputs, task, context| {
                                let value = value.clone();
                                async move { Ok(context.call(value, inputs, task).await?.unwrap()) }
                                    .boxed()
                            },
                        ));

                        let handle = wipple_interpreter::FunctionHandle::<Self>::new(index);

                        // FIXME: Until https://github.com/rustwasm/wasm-bindgen/issues/3715 is fixed,
                        // clients must pass all arguments as a single array
                        Closure::<dyn Fn(Vec<JsValue>) -> JsValue>::new(
                            move |inputs: Vec<JsValue>| {
                                wasm_bindgen_futures::future_to_promise(async move {
                                    let inputs =
                                        inputs.into_iter().map(Value::from).collect::<Vec<_>>();

                                    let output = wipple_interpreter::call_function(handle, inputs)
                                        .await
                                        .or_throw("failed to evaluate function");

                                    Ok(JsValue::from(output))
                                })
                                .into()
                            },
                        )
                        .into_js_value()
                    }
                    wipple_interpreter::Value::NativeFunction(func) => {
                        let mut functions = FUNCTIONS.lock().await;

                        let index = functions.len();
                        functions.push(wipple_interpreter::StoredFunction::new(
                            task.clone(),
                            context.clone(),
                            move |inputs, task, context| {
                                let func = func.clone();
                                async move { func(inputs, task, context).await }.boxed()
                            },
                        ));
                        let handle = wipple_interpreter::FunctionHandle::<Self>::new(index);

                        // FIXME: Until https://github.com/rustwasm/wasm-bindgen/issues/3715 is fixed,
                        // clients must pass all arguments as a single array
                        Closure::<dyn Fn(Vec<JsValue>) -> JsValue>::new(
                            move |inputs: Vec<JsValue>| {
                                wasm_bindgen_futures::future_to_promise(async move {
                                    let inputs =
                                        inputs.into_iter().map(Value::from).collect::<Vec<_>>();

                                    let output = wipple_interpreter::call_function(handle, inputs)
                                        .await
                                        .or_throw("failed to evaluate function");

                                    Ok(JsValue::from(output))
                                })
                                .into()
                            },
                        )
                        .into_js_value()
                    }
                    wipple_interpreter::Value::List(elements)
                    | wipple_interpreter::Value::Tuple(elements) => {
                        future::join_all(elements.into_iter().map(move |element| {
                            let task = task.clone();
                            let context = context.clone();

                            async move { Self::from_value(element, &task, &context).await }
                        }))
                        .await
                        .into_iter()
                        .map(JsValue::from)
                        .collect::<js_sys::Array>()
                        .into()
                    }
                    wipple_interpreter::Value::Variant { .. }
                    | wipple_interpreter::Value::Wrapper(_)
                    | wipple_interpreter::Value::Structure(_)
                    | wipple_interpreter::Value::TaskGroup(_)
                    | wipple_interpreter::Value::TaskLocalKey(_)
                    | wipple_interpreter::Value::Hasher(_) => {
                        wasm_bindgen::throw_str("cannot convert Wipple value to JavaScript value")
                    }
                }))
            })
        }

        fn to_value(
            value: Self::Value,
        ) -> impl Future<Output = wipple_interpreter::Value<Self>> + Send {
            wasm_main_executor::spawn(async move {
                if value.0.is_null() || value.0.is_undefined() {
                    wipple_interpreter::Value::Marker
                } else if let Some(number) = value.0.as_f64() {
                    wipple_interpreter::Value::from_number(Decimal::from_f64(number))
                } else if let Some(text) = value.0.as_string() {
                    wipple_interpreter::Value::from_text(text)
                } else if let Some(bool) = value.0.as_bool() {
                    wipple_interpreter::Value::from_bool(bool)
                } else if let Some(function) = value.0.dyn_ref::<VariadicFunction>() {
                    let function = SendWrapper::new(function.clone());
                    wipple_interpreter::Value::from_function(move |inputs, task, context| {
                        let function = function.clone();
                        async move {
                            let inputs = future::join_all(inputs.into_iter().map(move |element| {
                                let task = task.clone();
                                let context = context.clone();

                                async move { Self::from_value(element, &task, &context).await }
                            }))
                            .await;

                            let result = function
                                .into_inner()
                                .call_variadic(
                                    &JsValue::UNDEFINED,
                                    &inputs
                                        .into_iter()
                                        .map(JsValue::from)
                                        .collect::<js_sys::Array>(),
                                )
                                .or_throw("failed to call function");

                            Ok(Self::to_value(result.into()).await)
                        }
                        .boxed()
                    })
                } else if let Some(array) = value.0.dyn_ref::<js_sys::Array>() {
                    let elements = future::join_all(array.iter().map(|element| {
                        let element = SendWrapper::new(element);
                        async move { Self::to_value(element.into_inner().into()).await }.boxed()
                    }))
                    .await;

                    wipple_interpreter::Value::from_tuple(elements)
                } else {
                    wasm_bindgen::throw_str(&format!(
                        "cannot convert JavaScript value to Wipple value: {:#?}",
                        ()
                    ))
                }
            })
            .boxed()
        }

        async fn with_functions<T>(
            f: impl FnOnce(&mut Vec<wipple_interpreter::StoredFunction<Self>>) -> T + Send,
        ) -> T {
            f(&mut *FUNCTIONS.lock().await)
        }
    }

    let handlers = options.handlers.clone();

    let thread_id = run_thread_id(&options.id);

    let result = run_on_thread(thread_id, move || async move {
        let executable = match EXECUTABLES.lock().await.get(&options.id) {
            Some(executable) => executable.clone(),
            None => {
                return RunResult {
                    error: Some(String::from("program not compiled")),
                };
            }
        };

        let runtime_options =
            wipple_interpreter::Options::<Runtime>::with_io(wipple_interpreter::Io {
                display: Arc::new({
                    let handlers = handlers.clone();
                    move |message| {
                        let handlers = handlers.clone();

                        async move {
                            let (tx, rx) = async_channel::unbounded();

                            let _handle = wasm_main_executor::spawn(async move {
                                let promise = handlers
                                    .display
                                    .call1(&JsValue::UNDEFINED, &JsValue::from_str(&message))
                                    .or_throw("failed to call `display`")
                                    .dyn_into::<js_sys::Promise>()
                                    .or_throw("`display` did not return a promise");

                                wasm_bindgen_futures::spawn_local(async move {
                                    wasm_bindgen_futures::JsFuture::from(promise)
                                        .await
                                        .or_throw("`display` promise failed");

                                    tx.send(()).await.or_warn("tx failed");
                                });
                            });

                            rx.recv().await.or_throw("rx failed");

                            Ok(())
                        }
                        .boxed()
                    }
                }),
                prompt: Arc::new({
                    let handlers = handlers.clone();
                    move |message, validate| {
                        let handlers = handlers.clone();
                        async move {
                            let (tx, rx) = async_channel::unbounded();

                            let _handle = wasm_main_executor::spawn(async move {
                                let promise = handlers
                                    .prompt
                                    .call2(
                                        &JsValue::UNDEFINED,
                                        &JsValue::from_str(&message),
                                        &Closure::<dyn Fn(String) -> JsValue>::new(move |input| {
                                            let validate = validate.clone();
                                            wasm_bindgen_futures::future_to_promise(
                                                async move {
                                                    let result = validate(input)
                                                        .await
                                                        .or_throw("`validate` failed");

                                                    Ok(result.into())
                                                }
                                                .boxed(),
                                            )
                                            .into()
                                        })
                                        .into_js_value(),
                                    )
                                    .or_throw("failed to call `prompt`")
                                    .dyn_into::<js_sys::Promise>()
                                    .or_throw("`prompt` did not return a promise");

                                wasm_bindgen_futures::spawn_local(async move {
                                    wasm_bindgen_futures::JsFuture::from(promise)
                                        .await
                                        .or_throw("`prompt` promise failed");

                                    tx.send(()).await.or_warn("tx failed");
                                });
                            });

                            rx.recv().await.or_throw("rx failed");

                            Ok(())
                        }
                        .boxed()
                    }
                }),
                choice: Arc::new({
                    let handlers = handlers.clone();
                    move |message, choices| {
                        let handlers = handlers.clone();
                        async move {
                            let (tx, rx) = async_channel::unbounded();

                            let _handle = wasm_main_executor::spawn(async move {
                                let promise = handlers
                                    .choice
                                    .call2(
                                        &JsValue::UNDEFINED,
                                        &JsValue::from_str(&message),
                                        &choices
                                            .into_iter()
                                            .map(|choice| JsValue::from_str(&choice))
                                            .collect::<js_sys::Array>(),
                                    )
                                    .or_throw("failed to call `choice`")
                                    .dyn_into::<js_sys::Promise>()
                                    .or_throw("`choice` did not return a promise");

                                wasm_bindgen_futures::spawn_local(async move {
                                    let future = wasm_bindgen_futures::JsFuture::from(promise);

                                    let index = future
                                        .await
                                        .or_throw("`choice` promise failed")
                                        .as_f64()
                                        .expect_throw("`choice` did not return a number")
                                        .to_usize()
                                        .expect_throw("invalid index");

                                    tx.send(index).await.or_warn("tx failed");
                                });
                            });

                            let index = rx.recv().await.or_throw("rx failed");

                            Ok(index)
                        }
                        .boxed()
                    }
                }),
                ui: Arc::new({
                    let handlers = handlers.clone();
                    move |message, value| {
                        let handlers = handlers.clone();
                        async move {
                            let (tx, rx) = async_channel::unbounded();

                            let _handle = wasm_main_executor::spawn(async move {
                                let promise = handlers
                                    .ui
                                    .call2(
                                        &JsValue::UNDEFINED,
                                        &JsValue::from_str(&message),
                                        &value.0,
                                    )
                                    .or_throw("failed to call `ui`")
                                    .dyn_into::<js_sys::Promise>()
                                    .or_throw("`ui` did not return a promise");

                                wasm_bindgen_futures::spawn_local(async move {
                                    let result = wasm_bindgen_futures::JsFuture::from(promise)
                                        .await
                                        .or_throw("`ui` promise failed")
                                        .into();

                                    tx.send(result).await.or_warn("tx failed");
                                });
                            });

                            let result = rx.recv().await.or_throw("rx failed");

                            Ok(result)
                        }
                        .boxed()
                    }
                }),
                sleep: Arc::new(|ms| {
                    async move {
                        let (tx, rx) = async_channel::unbounded();
                        wasm_bindgen_futures::spawn_local(async move {
                            async_std::task::sleep(std::time::Duration::from_millis(ms)).await;
                            tx.send(()).await.or_warn("tx failed");
                        });

                        rx.recv().await.or_throw("rx failed");

                        Ok(())
                    }
                    .boxed()
                }),
            });

        let result = wipple_interpreter::evaluate(executable, runtime_options).await;
        let error = result.err().map(|error| error.0);

        RunResult { error }
    })
    .await;

    drop(options.handlers); // `SendHandler` must be dropped on original thread

    match result {
        Ok(result) => to_value(&result).or_throw("failed to serialize result"),
        Err(Canceled) => JsValue::UNDEFINED,
    }
}

#[derive(Deserialize)]
pub struct StopOptions {
    pub id: String,
}

#[derive(Serialize)]
pub struct StopResult {}

#[wasm_bindgen]
pub async fn stop(options: JsValue) -> JsValue {
    let options = from_value::<StopOptions>(options).or_throw("failed to deserialize options");

    stop_thread(&run_thread_id(&options.id)).await;

    let result = StopResult {};

    to_value(&result).or_throw("failed to serialize result")
}

#[derive(Deserialize)]
pub struct CleanupOptions {
    pub id: String,
}

#[derive(Serialize)]
pub struct CleanupResult {}

#[wasm_bindgen]
pub async fn cleanup(options: JsValue) -> JsValue {
    let options = from_value::<CleanupOptions>(options).or_throw("failed to deserialize options");

    DEPENDENCIES.lock().await.remove(&options.id);
    EXECUTABLES.lock().await.remove(&options.id);

    let result = CleanupResult {};

    to_value(&result).or_throw("failed to serialize result")
}
