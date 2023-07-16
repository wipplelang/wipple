use futures::channel::oneshot;
use itertools::Itertools;
use lazy_static::lazy_static;
use loader::Fetcher;
use nonoverlapping_interval_tree::NonOverlappingIntervalTree;
use parking_lot::{MappedMutexGuard, Mutex, MutexGuard};
use send_wrapper::SendWrapper;
use serde::Serialize;
use std::{
    collections::HashMap,
    future::Future,
    mem,
    pin::Pin,
    sync::{atomic::AtomicBool, Arc},
};
use url::Url;
use wasm_bindgen::{prelude::*, JsCast};
use wasm_bindgen_futures::JsFuture;
use wipple_default_loader as loader;
use wipple_frontend::Loader;

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct AnalysisOutput {
    diagnostics: Vec<AnalysisOutputDiagnostic>,
    syntax_highlighting: Vec<AnalysisOutputSyntaxHighlightingItem>,
    completions: AnalysisOutputCompletions,
}

#[derive(PartialEq, Eq, Serialize)]
#[serde(rename_all = "camelCase")]
struct AnalysisOutputDiagnostic {
    level: AnalysisOutputDiagnosticLevel,
    message: String,
    notes: Vec<AnalysisOutputDiagnosticNote>,
    fix: Option<AnalysisOutputDiagnosticFix>,
}

#[derive(PartialEq, Eq, Serialize)]
#[serde(rename_all = "camelCase")]
enum AnalysisOutputDiagnosticLevel {
    Warning,
    Error,
}

#[derive(PartialEq, Eq, Serialize)]
#[serde(rename_all = "camelCase")]
struct AnalysisOutputDiagnosticNote {
    code: String,
    span: AnalysisOutputDiagnosticSpan,
    messages: Vec<String>,
}

#[derive(PartialEq, Eq, Serialize)]
pub struct AnalysisOutputDiagnosticFix {
    pub description: String,
    pub start: usize,
    pub end: usize,
    pub replacement: String,
}

#[derive(PartialEq, Eq, Serialize)]
#[serde(rename_all = "camelCase")]
struct AnalysisOutputDiagnosticSpan {
    pub file: String,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct AnalysisOutputSyntaxHighlightingItem {
    start: usize,
    end: usize,
    kind: &'static str,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct HoverOutput {
    code: String,
    help: String,
    url: Option<String>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct AnalysisOutputCompletions {
    language: Vec<Completion>,
    grouped_constants: Vec<(String, Vec<Completion>)>,
    ungrouped_constants: Vec<Completion>,
    variables: Vec<Completion>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "camelCase")]
struct Completion {
    kind: Option<&'static str>,
    name: String,
    help: String,
    template: String,
}

// SAFETY: This is safe because Wasm is single-threaded
struct SendSyncFuture<F: ?Sized>(Pin<Box<F>>);
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

struct CancellableFuture<Fut> {
    token: Arc<AtomicBool>,
    fut: Pin<Box<Fut>>,
}

impl<Fut> CancellableFuture<Fut> {
    fn new(token: Arc<AtomicBool>, fut: Fut) -> Self {
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

#[cfg(feature = "debug_playground")]
const FILES_PATH: &str = "http://localhost:8080/playground/files/";

#[cfg(not(feature = "debug_playground"))]
const FILES_PATH: &str = "https://wipple.dev/playground/files/";

lazy_static! {
    static ref PLAYGROUND_PATH: wipple_frontend::helpers::InternedString = wipple_frontend::helpers::InternedString::new("playground");


    static ref LOADER: loader::Loader = {
        loader::Loader::new(
            Some(wipple_frontend::FilePath::Url(wipple_frontend::helpers::InternedString::new(FILES_PATH))),
            Some(wipple_frontend::FilePath::Path(
                #[cfg(feature = "debug_playground")]
                wipple_frontend::helpers::InternedString::new(format!(
                    "{}std/std.wpl",
                    env!("CARGO_WORKSPACE_DIR")
                )),
                #[cfg(not(feature = "debug_playground"))]
                wipple_frontend::helpers::InternedString::new(loader::STD_URL),
            )),
        )
        .with_fetcher(
            Fetcher::new()
                .with_path_handler(|path| {
                    Box::pin(async move {
                        let error = || anyhow::Error::msg("loading files from paths is not supported in the playground; try loading from a URL instead");

                        #[cfg(feature = "debug_playground")]
                        {
                            #[derive(rust_embed::RustEmbed)]
                            #[folder = "$CARGO_WORKSPACE_DIR/std"]
                            struct EmbeddedStd;

                            let path = std::path::PathBuf::from(path);
                            let path = path.file_name().unwrap().to_str().unwrap();

                            let file = EmbeddedStd::get(path)
                                .map(|file| String::from_utf8(file.data.to_vec()).unwrap())
                                .ok_or_else(error)?;

                            Ok(file)
                        }

                        #[cfg(not(feature = "debug_playground"))]
                        {
                            let _ = path;
                            Err(error())
                        }
                    })
                })
                .with_url_handler(move |url| {
                    Box::pin(SendSyncFuture(Box::pin(async move {
                        let error_to_string = |msg: &str, value: JsValue| {
                            let error = value
                                .as_string()
                                .or_else(|| {
                                    value
                                        .dyn_into::<js_sys::Error>()
                                        .ok()
                                        .map(|e| e.to_string().into())
                                });

                            match error {
                                Some(error) => format!("{msg}: {error}"),
                                None => msg.to_string(),
                            }
                        };

                        let global = js_sys::global()
                            .dyn_into::<web_sys::WorkerGlobalScope>()
                            .unwrap();

                        let response = JsFuture::from(global.fetch_with_request(
                            &web_sys::Request::new_with_str(url.as_str()).unwrap(),
                        ))
                        .await
                        .map_err(|e| anyhow::Error::msg(error_to_string("request failed", e)))?;

                        let response = response
                            .dyn_into::<web_sys::Response>()
                            .map_err(|e| anyhow::Error::msg(error_to_string("invalid response", e)))?;

                        if !response.ok() {
                            return Err(anyhow::Error::msg(format!("request failed: {}", response.status_text())));
                        }

                        let text = JsFuture::from(
                            response.text().map_err(|e| anyhow::Error::msg(error_to_string("invalid response", e)))?,
                        )
                        .await
                        .map_err(|e| anyhow::Error::msg(error_to_string("invalid response", e)))?;

                        let text = text
                            .as_string()
                            .ok_or_else(|| anyhow::Error::msg("invalid response"))?;

                        Ok(text)
                    })))
                }),
        )
    };

    #[allow(clippy::let_and_return)]
    static ref COMPILER: wipple_frontend::Compiler = {
        let compiler = wipple_frontend::Compiler::new(LOADER.clone());

        #[cfg(debug_assertions)]
        let compiler = compiler.set_backtrace_enabled(false);

        compiler
    };


    static ref ANALYSIS: Arc<Mutex<Option<Analysis>>> = Default::default();
}

#[derive(Debug)]
struct Analysis {
    program: wipple_frontend::analysis::Program,
    success: bool,
}

fn save_analysis(analysis: Analysis) {
    *ANALYSIS.lock() = Some(analysis);
}

fn get_analysis<'a>() -> Option<MappedMutexGuard<'a, Analysis>> {
    MutexGuard::try_map(ANALYSIS.lock(), |analysis| analysis.as_mut()).ok()
}

#[wasm_bindgen]
pub fn analyze(
    code: String,
    lint: bool,
    handle_plugin: js_sys::Function,
    callback: js_sys::Function,
) {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    wasm_bindgen_futures::spawn_local(async move {
        let handle_plugin = SendWrapper::new(handle_plugin);

        let prev_plugin_hander = Mutex::new(Some(LOADER.set_plugin_handler(
            loader::PluginHandler::new().with_url_handler(move |path, name, input, _api| {
                let handle_plugin = handle_plugin.clone();
                let path = path.to_string();
                let name = name.to_string();

                Box::pin(SendSyncFuture(Box::pin(async move {
                    #[wasm_bindgen(getter_with_clone)]
                    struct Api {
                        // TODO
                    }

                    let api = Api {
                        // TODO
                    };

                    let mut output = handle_plugin
                        .apply(
                            &JsValue::NULL,
                            &js_sys::Array::from_iter([
                                path.into(),
                                name.into(),
                                serde_wasm_bindgen::to_value(&input).unwrap(),
                                api.into(),
                            ]),
                        )
                        .map_err(|e| {
                            anyhow::Error::msg(
                                js_sys::Error::try_from(e)
                                    .expect("expected error")
                                    .to_string()
                                    .as_string()
                                    .unwrap(),
                            )
                        })?;

                    let output = loop {
                        let promise = match output.dyn_into::<js_sys::Promise>() {
                            Ok(promise) => promise,
                            Err(value) => break value,
                        };

                        output = wasm_bindgen_futures::JsFuture::from(promise)
                            .await
                            .map_err(|e| {
                                anyhow::Error::msg(
                                    js_sys::Error::try_from(e)
                                        .expect("expected error")
                                        .to_string()
                                        .as_string()
                                        .unwrap(),
                                )
                            })?;
                    };

                    Ok(serde_wasm_bindgen::from_value(output).unwrap())
                })))
            }),
        )));

        let panic_hook = std::panic::take_hook();

        let callback = SendWrapper::new(callback);

        std::panic::set_hook({
            let callback = callback.clone();

            Box::new(move |info| {
                callback
                    .call2(&JsValue::NULL, &JsValue::FALSE, &info.to_string().into())
                    .unwrap();

                if let Some(prev_plugin_hander) = prev_plugin_hander.lock().take() {
                    LOADER.set_plugin_handler(prev_plugin_hander);
                }

                panic_hook(info);
            })
        });

        LOADER
            .virtual_paths()
            .lock()
            .insert(*PLAYGROUND_PATH, Arc::from(code));

        let (program, diagnostics) = COMPILER
            .analyze_with(
                wipple_frontend::FilePath::Virtual(*PLAYGROUND_PATH),
                &wipple_frontend::analysis::Options::new().lint(lint),
            )
            .await;

        let success = !diagnostics.contains_errors();

        let diagnostics = diagnostics
            .diagnostics
            .into_iter()
            .map(|diagnostic| {
                let mut notes = Vec::<(
                    wipple_frontend::analysis::Span,
                    AnalysisOutputDiagnosticNote,
                )>::new();

                for note in diagnostic.notes {
                    let (first, rest) = note.span.split_iter();
                    let convert_span = |span: wipple_frontend::analysis::Span, use_caller: bool| {
                        let range = use_caller
                            .then(|| span.caller_range())
                            .flatten()
                            .unwrap_or_else(|| span.primary_range());

                        AnalysisOutputDiagnosticSpan {
                            file: span.path.to_string(),
                            start: range.start,
                            end: range.end,
                        }
                    };

                    let get_code = |path: wipple_frontend::FilePath| {
                        LOADER.source_map().lock().get(&path).unwrap().to_string()
                    };

                    for (span, message) in
                        std::iter::once((first, note.message)).chain(rest.map(|span| {
                            (
                                span,
                                format!(
                                    "actual {} occurred here",
                                    match diagnostic.level {
                                        wipple_frontend::diagnostics::DiagnosticLevel::Error =>
                                            "error",
                                        wipple_frontend::diagnostics::DiagnosticLevel::Warning =>
                                            "warning",
                                    }
                                ),
                            )
                        }))
                    {
                        if let Some(existing) = notes
                            .iter_mut()
                            .find(|(existing_span, _)| *existing_span == span)
                        {
                            existing.1.messages.push(message);
                        } else {
                            notes.push((
                                span,
                                AnalysisOutputDiagnosticNote {
                                    code: get_code(span.path),
                                    span: convert_span(span, note.use_caller_if_available),
                                    messages: vec![message],
                                },
                            ));
                        }
                    }
                }

                AnalysisOutputDiagnostic {
                    level: match diagnostic.level {
                        wipple_frontend::diagnostics::DiagnosticLevel::Warning => {
                            AnalysisOutputDiagnosticLevel::Warning
                        }
                        wipple_frontend::diagnostics::DiagnosticLevel::Error => {
                            AnalysisOutputDiagnosticLevel::Error
                        }
                    },
                    message: diagnostic.message,
                    notes: notes.into_iter().map(|(_, note)| note).collect(),
                    fix: diagnostic.fix.map(|fix| AnalysisOutputDiagnosticFix {
                        description: fix.description,
                        start: fix.range.range().start,
                        end: fix.range.range().end,
                        replacement: fix.replacement,
                    }),
                }
            })
            .dedup()
            .collect();

        let syntax_highlighting = get_syntax_highlighting(&program);
        let completions = get_completions(&program);

        save_analysis(Analysis { program, success });

        let output = AnalysisOutput {
            diagnostics,
            syntax_highlighting,
            completions,
        };

        let output = serde_wasm_bindgen::to_value(&output).unwrap();

        callback
            .call2(&JsValue::NULL, &JsValue::TRUE, &output)
            .unwrap();
    });
}

fn get_syntax_highlighting(
    program: &wipple_frontend::analysis::Program,
) -> Vec<AnalysisOutputSyntaxHighlightingItem> {
    let playground_path = wipple_frontend::FilePath::Virtual(*PLAYGROUND_PATH);

    let mut items = Vec::new();

    const ORDER: [&str; 9] = [
        "type", "trait", "variable", "function", "number", "text", "keyword", "operator", "syntax",
    ];

    macro_rules! insert_semantic_tokens {
        ($kind:ident, $token:expr) => {
            for (id, decl) in &program.declarations.$kind {
                if decl.span.first().path == playground_path {
                    items.push(AnalysisOutputSyntaxHighlightingItem {
                        start: decl
                            .span
                            .first()
                            .caller_start()
                            .unwrap_or_else(|| decl.span.first().primary_start()),
                        end: decl
                            .span
                            .first()
                            .caller_end()
                            .unwrap_or_else(|| decl.span.first().primary_end()),
                        kind: $token(id, decl),
                    });
                }

                for &span in &decl.uses {
                    if span.first().path == playground_path {
                        items.push(AnalysisOutputSyntaxHighlightingItem {
                            start: span
                                .first()
                                .caller_start()
                                .unwrap_or_else(|| span.first().primary_start()),
                            end: span
                                .first()
                                .caller_end()
                                .unwrap_or_else(|| span.first().primary_end()),
                            kind: $token(id, decl),
                        });
                    }
                }
            }
        };
    }

    insert_semantic_tokens!(types, |_, _| "type");
    insert_semantic_tokens!(traits, |_, _| "trait");
    insert_semantic_tokens!(constants, |_, _| "variable");
    insert_semantic_tokens!(
        syntaxes,
        |_, syntax: &wipple_frontend::analysis::typecheck::SyntaxDecl| {
            if syntax.keyword {
                "keyword"
            } else if syntax.operator {
                "operator"
            } else {
                "syntax"
            }
        }
    );
    insert_semantic_tokens!(
        builtin_syntaxes,
        |_, syntax: &wipple_frontend::analysis::typecheck::BuiltinSyntaxDecl| {
            if syntax.definition.operator {
                "operator"
            } else {
                "keyword"
            }
        }
    );
    insert_semantic_tokens!(builtin_types, |_, _| "type");
    insert_semantic_tokens!(type_parameters, |_, _| "type");
    insert_semantic_tokens!(variables, |_, _| "variable");

    let mut traverse_semantic_tokens = |expr: &wipple_frontend::analysis::Expression| {
        if expr.span.first().path != playground_path {
            return;
        }

        if matches!(
            expr.kind,
            wipple_frontend::analysis::ExpressionKind::Variable(_)
                | wipple_frontend::analysis::ExpressionKind::Constant(_)
        ) && matches!(expr.ty, wipple_frontend::analysis::Type::Function(_, _))
        {
            items.push(AnalysisOutputSyntaxHighlightingItem {
                start: expr.span.first().primary_start(),
                end: expr.span.first().primary_end(),
                kind: "function",
            });
        } else if let Some(literal_kind) = expr.literal_kind() {
            items.push(AnalysisOutputSyntaxHighlightingItem {
                start: expr.span.first().primary_start(),
                end: expr.span.first().primary_end(),
                kind: match literal_kind {
                    wipple_frontend::analysis::LiteralKind::Number => "number",
                    wipple_frontend::analysis::LiteralKind::Text => "text",
                },
            });
        }
    };

    for decl in program.declarations.constants.values() {
        if let Some(expr) = &decl.body {
            expr.traverse(&mut traverse_semantic_tokens);
        }
    }

    for instances in program.declarations.instances.values() {
        for decl in instances.values() {
            if let Some(expr) = &decl.body {
                expr.traverse(&mut traverse_semantic_tokens);
            }
        }
    }

    for item in program.items.values() {
        let item = item.read();
        let (constant, expr) = &*item;

        if constant.is_some() {
            // Skip monomorphized constant types
            continue;
        }

        expr.traverse(&mut traverse_semantic_tokens);
    }

    let items = items
        .into_iter()
        .sorted_by_key(|item| (item.start, ORDER.iter().position(|&s| s == item.kind)));

    let mut tree = NonOverlappingIntervalTree::new();
    for item in items {
        tree.insert_replace(item.start..item.end, Some(item));
    }

    tree.range_mut(0..usize::MAX)
        .map(|(_, value)| value.value_mut().take().unwrap())
        .collect()
}

fn get_completions(program: &wipple_frontend::analysis::Program) -> AnalysisOutputCompletions {
    let mut language = Vec::new();
    for decl in wipple_syntax::ast::builtin_syntax_definitions() {
        let completion = Completion {
            kind: Some(if decl.operator { "operator" } else { "keyword" }),
            name: decl.name.to_string(),
            help: decl.help.to_string(),
            template: decl.template.to_string(),
        };

        language.push(completion);
    }

    let mut grouped_constants = HashMap::<String, Vec<Completion>>::new();
    let mut ungrouped_constants = Vec::new();

    macro_rules! add {
        ($group:expr, $completion:ident) => {{
            if let Some(group) = $group {
                grouped_constants
                    .entry(group.to_string())
                    .or_default()
                    .push($completion);
            } else {
                ungrouped_constants.push($completion);
            }
        }};
    }

    for decl in program.declarations.constants.values() {
        let completion = Completion {
            kind: matches!(decl.ty, wipple_frontend::analysis::Type::Function(_, _))
                .then_some("function"),
            name: decl.name.to_string(),
            help: decl.attributes.decl_attributes.help.iter().join("\n"),
            template: decl
                .attributes
                .decl_attributes
                .help_template
                .map(|template| template.to_string())
                .unwrap_or_else(|| decl.name.to_string()),
        };

        add!(decl.attributes.decl_attributes.help_group, completion);
    }

    for decl in program.declarations.syntaxes.values() {
        let completion = Completion {
            kind: Some("syntax"),
            name: decl.name.to_string(),
            help: decl.attributes.decl_attributes.help.iter().join("\n"),
            template: decl
                .attributes
                .decl_attributes
                .help_template
                .map(|template| template.to_string())
                .unwrap_or_else(|| decl.name.to_string()),
        };

        add!(decl.attributes.decl_attributes.help_group, completion);
    }

    let mut variables = Vec::new();
    for decl in program.declarations.variables.values() {
        let name = match decl.name {
            Some(name) => name.to_string(),
            None => continue,
        };

        variables.push(Completion {
            kind: matches!(decl.ty, wipple_frontend::analysis::Type::Function(_, _))
                .then_some("function"),
            name: name.clone(),
            help: String::new(),
            template: name,
        });
    }

    for completions in std::iter::once(&mut language)
        .chain(grouped_constants.values_mut())
        .chain(std::iter::once(&mut ungrouped_constants))
        .chain(std::iter::once(&mut variables))
    {
        *completions = mem::take(completions).into_iter().unique().collect();
        completions.sort_by(|a, b| a.name.cmp(&b.name));
    }

    let grouped_constants = grouped_constants
        .into_iter()
        .sorted_by(|(a, _), (b, _)| a.cmp(b))
        .collect::<Vec<_>>();

    AnalysisOutputCompletions {
        language,
        grouped_constants,
        ungrouped_constants,
        variables,
    }
}

#[wasm_bindgen]
pub fn run(handle_io: js_sys::Function, callback: js_sys::Function) -> JsValue {
    let handle_io = Arc::new(Mutex::new(SendWrapper::new(handle_io)));

    let tasks: Arc<Mutex<Vec<oneshot::Receiver<()>>>> = Default::default();
    let cancel_token = Arc::new(AtomicBool::new(false));

    let analysis = match get_analysis() {
        Some(analysis) => analysis,
        None => {
            callback
                .call1(&JsValue::NULL, &String::from("program not compiled").into())
                .unwrap();

            return JsValue::NULL;
        }
    };

    let Analysis { program, success } = &*analysis;

    let program = success.then(|| COMPILER.ir_from(program));

    let program = match program {
        Some(program) => program,
        None => {
            callback
                .call1(&JsValue::NULL, &String::from("compilation failed").into())
                .unwrap();

            return JsValue::NULL;
        }
    };

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

#[wasm_bindgen]
pub fn hover(start: usize, end: usize) -> JsValue {
    use wipple_frontend::{
        analysis::Span,
        analysis::{
            typecheck::{
                format::{format_type, Format, TypeFunctionFormat},
                TraitDecl, TypeDecl,
            },
            ExpressionKind, Type,
        },
        helpers::InternedString,
        FilePath,
    };

    let analysis = match get_analysis() {
        Some(analysis) => analysis,
        None => return JsValue::NULL,
    };

    let within_hover = |span: Span| {
        span.path == FilePath::Virtual(*PLAYGROUND_PATH)
            && start >= span.primary_start()
            && end <= span.primary_end()
    };

    macro_rules! getter {
        ($kind:ident, $f:expr) => {
            |id| $f(analysis.program.declarations.$kind.get(&id).unwrap().name)
        };
    }

    let type_names = getter!(types, |name: InternedString| name.to_string());
    let trait_names = getter!(traits, |name: InternedString| name.to_string());
    let param_names = getter!(type_parameters, |name: Option<_>| {
        name.as_ref().map(ToString::to_string)
    });

    let format_type =
        |ty: Type, format: Format| format_type(ty, type_names, trait_names, param_names, format);

    let mut hovers = Vec::new();

    for item in analysis.program.items.values() {
        let item = item.read();
        let (constant, expr) = &*item;

        if constant.is_some() {
            // Skip monomorphized constant types
            continue;
        }

        expr.traverse(|expr| {
            // Don't show type of entire file
            if let Some(entrypoint) = &analysis.program.entrypoint {
                if let Some(item) = analysis.program.items.get(entrypoint) {
                    let item = item.read();
                    let (_, item) = &*item;

                    if expr.span == item.span {
                        return;
                    }
                }
            }

            if matches!(
                expr.kind,
                ExpressionKind::Variable(_) | ExpressionKind::Constant(_)
            ) {
                return;
            }

            if !within_hover(expr.span.first()) {
                return;
            }

            hovers.push((
                expr.span.first(),
                HoverOutput {
                    code: format_type(expr.ty.clone(), Format::default()),
                    help: String::new(),
                    url: None,
                },
            ));
        })
    }

    fn find_nearest_help_url(
        file: wipple_frontend::FilePath,
        analysis: &Analysis,
    ) -> Option<String> {
        let attributes = analysis.program.file_attributes.get(&file).unwrap();

        attributes
            .help_url
            .as_ref()
            .map(ToString::to_string)
            .or_else(|| {
                let imported_by = attributes.imported_by.lock().clone();

                imported_by
                    .into_iter()
                    .find_map(|file| find_nearest_help_url(file, analysis))
            })
    }

    macro_rules! help_url {
        ($id:expr, $anchor:expr) => {{
            $id.file
                .and_then(|file| find_nearest_help_url(file, &analysis))
                .and_then(|url| url.parse::<Url>().ok())
                .and_then(|mut url| {
                    url.set_fragment(Some($anchor));
                    Some(url.to_string())
                })
        }};
    }

    macro_rules! type_decls {
        ($kind:ident $(($opt:tt))?, $str:literal $(, $help:expr)?) => {
            for (&id, decl) in &analysis.program.declarations.$kind {
                for span in std::iter::once(decl.span).chain(decl.uses.iter().copied()) {
                    if !within_hover(span.first()) {
                        continue;
                    }

                    hovers.push((
                        span.first(),
                        HoverOutput {
                            code: format!("{} : {}", decl.name, $str),
                            help: type_decls!(@help decl, $($help)?),
                            url: help_url!(id, &format!("{}.{}", $str, decl.name)),
                        },
                    ));
                }
            }
        };
        (@help $decl:ident, $help:expr) => {
            $help($decl)
                .into_iter()
                .map(|line| line.to_string())
                .collect::<Vec<_>>()
                .join("\n")
        };
        (@help $decl:ident) => {
            String::new()
        }
    }

    type_decls!(types, "type", |decl: &TypeDecl| {
        decl.attributes.decl_attributes.help.clone()
    });

    type_decls!(traits, "trait", |decl: &TraitDecl| {
        decl.attributes.decl_attributes.help.clone()
    });

    for (&id, decl) in &analysis.program.declarations.constants {
        for span in std::iter::once(decl.span).chain(decl.uses.iter().copied()) {
            if !within_hover(span.first()) {
                continue;
            }

            let format = Format {
                type_function: TypeFunctionFormat::Arrow(&decl.bounds),
                ..Default::default()
            };

            let is_function = matches!(decl.ty, wipple_frontend::analysis::Type::Function(_, _));

            hovers.push((
                span.first(),
                HoverOutput {
                    code: format!(
                        "{} :: {}",
                        decl.name,
                        format_type(
                            decl.reduced_ty.clone().unwrap_or_else(|| decl.ty.clone()),
                            format
                        )
                    ),
                    help: decl
                        .attributes
                        .decl_attributes
                        .help
                        .iter()
                        .map(|line| line.to_string())
                        .collect::<Vec<_>>()
                        .join("\n"),
                    url: help_url!(
                        id,
                        &format!(
                            "{}.{}",
                            if is_function { "function" } else { "constant" },
                            decl.name
                        )
                    ),
                },
            ));
        }
    }

    for decl in analysis.program.declarations.variables.values() {
        for span in std::iter::once(decl.span).chain(decl.uses.iter().copied()) {
            if !within_hover(span.first()) {
                continue;
            }

            let name = match decl.name {
                Some(name) => name,
                None => continue,
            };

            hovers.push((
                span.first(),
                HoverOutput {
                    code: format!(
                        "{} :: {}",
                        name,
                        format_type(decl.ty.clone(), Format::default())
                    ),
                    help: String::new(),
                    url: None,
                },
            ));
        }
    }

    for decl in analysis.program.declarations.builtin_syntaxes.values() {
        for span in decl.uses.iter().copied() {
            if !within_hover(span.first()) {
                continue;
            }

            hovers.push((
                span.first(),
                HoverOutput {
                    code: format!("{} : syntax", decl.definition.name),
                    help: decl.definition.help.to_string(),
                    url: Some(format!(
                        "https://wipple.dev/doc/std.html#{}.{}",
                        if decl.definition.operator {
                            "operator"
                        } else {
                            "keyword"
                        },
                        decl.name
                    )),
                },
            ));
        }
    }

    for (&id, decl) in &analysis.program.declarations.syntaxes {
        for span in std::iter::once(decl.span).chain(decl.uses.iter().copied()) {
            if !within_hover(span.first()) {
                continue;
            }

            hovers.push((
                span.first(),
                HoverOutput {
                    code: format!("{} : syntax", decl.name),
                    help: decl
                        .attributes
                        .decl_attributes
                        .help
                        .iter()
                        .map(|line| line.to_string())
                        .collect::<Vec<_>>()
                        .join("\n"),
                    url: help_url!(id, &format!("syntax.{}", decl.name)),
                },
            ));
        }
    }

    let hover = hovers
        .into_iter()
        .min_by_key(|(span, _)| span.primary_end() - span.primary_start())
        .map(|(_, hover)| hover);

    serde_wasm_bindgen::to_value(&hover).unwrap()
}

#[wasm_bindgen]
pub fn format(code: &str) -> Option<String> {
    wipple_syntax::parse::format(code)
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
