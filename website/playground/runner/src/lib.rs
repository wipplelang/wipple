use futures::channel::oneshot::channel;
use itertools::Itertools;
use lazy_static::lazy_static;
use loader::Fetcher;
use parking_lot::{MappedMutexGuard, Mutex, MutexGuard};
use serde::Serialize;
use std::{
    future::Future,
    pin::Pin,
    sync::{atomic::AtomicBool, Arc},
};
use wasm_bindgen::{prelude::*, JsCast};
use wasm_bindgen_futures::JsFuture;
use wipple_default_loader as loader;
use wipple_frontend::Loader;

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct AnalysisOutput {
    diagnostics: Vec<AnalysisOutputDiagnostic>,
    syntax_highlighting: Vec<AnalysisOutputSyntaxHighlightingItem>,
    completions: Vec<Completion>,
}

#[derive(PartialEq, Eq, Serialize)]
#[serde(rename_all = "camelCase")]
struct AnalysisOutputDiagnostic {
    level: AnalysisOutputDiagnosticLevel,
    message: String,
    notes: Vec<AnalysisOutputDiagnosticNote>,
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
#[serde(rename_all = "camelCase")]
struct AnalysisOutputDiagnosticSpan {
    pub file: String,
    pub start: usize,
    pub end: usize,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct AnalysisOutputSyntaxHighlightingItem {
    start: usize,
    end: usize,
    kind: &'static str,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct HoverOutput {
    code: String,
    help: String,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct Completion {
    kind: Option<&'static str>,
    name: String,
    help: String,
}

// SAFETY: This is safe because Wasm is single-threaded
struct SendSyncFuture<F>(Pin<Box<F>>);
unsafe impl<F> Send for SendSyncFuture<F> {}
unsafe impl<F> Sync for SendSyncFuture<F> {}

impl<F: Future> Future for SendSyncFuture<F> {
    type Output = F::Output;

    fn poll(
        mut self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        self.0.as_mut().poll(cx)
    }
}

lazy_static! {
    static ref PLAYGROUND_PATH: wipple_frontend::helpers::InternedString = wipple_frontend::helpers::InternedString::new("playground");

    static ref LOADER: loader::Loader = {
        loader::Loader::new_with_fetcher(
            None,
            Some(wipple_frontend::FilePath::Path(
                #[cfg(feature = "debug_playground")]
                wipple_frontend::helpers::InternedString::new(format!(
                    "{}pkg/std/std.wpl",
                    env!("CARGO_WORKSPACE_DIR")
                )),
                #[cfg(not(feature = "debug_playground"))]
                wipple_frontend::helpers::InternedString::new(loader::STD_URL),
            )),
            Fetcher::new()
                .with_path_handler(|path| {
                    Box::pin(async move {
                        #[cfg(feature = "debug_playground")]
                        {
                            #[derive(rust_embed::RustEmbed)]
                            #[folder = "$CARGO_WORKSPACE_DIR/pkg/std"]
                            struct EmbeddedStd;

                            let path = std::path::PathBuf::from(path);
                            let path = path.file_name().unwrap().to_str().unwrap();

                            let file = EmbeddedStd::get(path)
                                .map(|file| String::from_utf8(file.data.to_vec()).unwrap())
                                .ok_or_else(|| {
                                    anyhow::Error::msg("file does not exist in standard library")
                                })?;

                            Ok(file)
                        }

                        #[cfg(not(feature = "debug_playground"))]
                        {
                            let _ = path;
                            Err(anyhow::Error::msg("loading files from paths is not supported in the playground; try loading from a URL instead"))
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
pub async fn analyze(code: String, lint: bool) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

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
            let mut notes =
                Vec::<(wipple_frontend::parse::Span, AnalysisOutputDiagnosticNote)>::new();

            for note in diagnostic.notes {
                let (first, rest) = note.span.split_iter();
                let convert_span =
                    |span: wipple_frontend::parse::Span| AnalysisOutputDiagnosticSpan {
                        file: span.path.to_string(),
                        start: span.start,
                        end: span.end,
                    };

                let get_code = |path: wipple_frontend::FilePath| {
                    LOADER.source_map().lock().get(&path).unwrap().to_string()
                };

                for (span, message) in std::iter::once((first, note.message))
                    .chain(rest.map(|span| (span, String::from(""))))
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
                                span: convert_span(span),
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

    JsValue::from_serde(&output).unwrap()
}

fn get_syntax_highlighting(
    program: &wipple_frontend::analysis::Program,
) -> Vec<AnalysisOutputSyntaxHighlightingItem> {
    let playground_path = wipple_frontend::FilePath::Virtual(*PLAYGROUND_PATH);

    let mut items = Vec::new();

    macro_rules! insert_semantic_tokens {
        ($kind:ident, $token:expr) => {
            for (id, decl) in &program.declarations.$kind {
                if decl.span.first().path == playground_path {
                    items.push(AnalysisOutputSyntaxHighlightingItem {
                        start: decl.span.first().start,
                        end: decl.span.first().end,
                        kind: $token(id, decl),
                    });
                }

                for &span in &decl.uses {
                    if span.first().path == playground_path {
                        items.push(AnalysisOutputSyntaxHighlightingItem {
                            start: span.first().start,
                            end: span.first().end,
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
                start: expr.span.first().start,
                end: expr.span.first().end,
                kind: "function",
            });
        }
    };

    for decl in program.declarations.constants.values() {
        if let Some(expr) = &decl.body {
            expr.traverse(&mut traverse_semantic_tokens);
        }
    }

    for (constant, expr) in program.items.values() {
        if constant.is_some() {
            // Skip monomorphized constant types
            continue;
        }

        expr.traverse(&mut traverse_semantic_tokens);
    }

    items.reverse();
    items.sort_by_key(|item| item.start);
    items.dedup_by_key(|item| (item.start, item.end));

    items
}

fn get_completions(program: &wipple_frontend::analysis::Program) -> Vec<Completion> {
    let mut items = Vec::new();

    for decl in program.declarations.constants.values() {
        items.push(Completion {
            kind: matches!(decl.ty, wipple_frontend::analysis::Type::Function(_, _))
                .then_some("function"),
            name: decl.name.to_string(),
            help: decl.attributes.decl_attributes.help.iter().join("\n"),
        });
    }

    for decl in program.declarations.variables.values() {
        items.push(Completion {
            kind: matches!(decl.ty, wipple_frontend::analysis::Type::Function(_, _))
                .then_some("function"),
            name: match decl.name {
                Some(name) => name.to_string(),
                None => continue,
            },
            help: String::new(),
        });
    }

    items.sort_by(|a, b| a.name.cmp(&b.name));
    items.dedup_by(|a, b| a.name == b.name);

    items
}

#[wasm_bindgen]
pub fn run(handle_console: js_sys::Function, callback: js_sys::Function) -> JsValue {
    let analysis = match get_analysis() {
        Some(analysis) => analysis,
        None => {
            callback.call1(&JsValue::NULL, &JsValue::FALSE).unwrap();
            return JsValue::NULL;
        }
    };

    let Analysis { program, success } = &*analysis;

    let program = success.then(|| COMPILER.ir_from(program));

    let program = match program {
        Some(program) => program,
        None => {
            callback.call1(&JsValue::NULL, &JsValue::FALSE).unwrap();
            return JsValue::NULL;
        }
    };

    let send_display = {
        let handle_console = handle_console.clone();

        move |text: String, callback: Box<dyn FnOnce()>| {
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

            handle_console
                .call1(&JsValue::NULL, &request.into())
                .unwrap();
        }
    };

    let mut interpreter = wipple_interpreter_backend::Interpreter::new({
        let send_display = send_display.clone();

        move |request| {
            match request {
                wipple_interpreter_backend::ConsoleRequest::Display(text, callback) => {
                    send_display(text.to_string(), callback);
                }
                wipple_interpreter_backend::ConsoleRequest::Prompt(
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

                    handle_console
                        .call1(&JsValue::NULL, &request.into())
                        .unwrap();
                }
                wipple_interpreter_backend::ConsoleRequest::Choice(prompt, choices, callback) => {
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

                    handle_console
                        .call1(&JsValue::NULL, &request.into())
                        .unwrap();
                }
            }

            Ok(())
        }
    });

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

        fn poll(
            self: Pin<&mut Self>,
            cx: &mut std::task::Context,
        ) -> std::task::Poll<Self::Output> {
            let cancelled = self.token.load(std::sync::atomic::Ordering::SeqCst);

            if cancelled {
                std::task::Poll::Ready(None)
            } else {
                Pin::new(&mut self.get_mut().fut).poll(cx).map(Some)
            }
        }
    }

    let token = Arc::new(AtomicBool::new(false));
    let future = CancellableFuture::new(token.clone(), async move {
        let result = interpreter.run(&program).await;

        if let Err(error) = result {
            let (completion_tx, completion_rx) = channel();
            send_display(
                format!("fatal error: {error}"),
                Box::new(|| completion_tx.send(()).unwrap()),
            );

            completion_rx.await.unwrap();
        }

        callback.call1(&JsValue::NULL, &JsValue::TRUE).unwrap();
    });

    wasm_bindgen_futures::spawn_local(async move {
        future.await;
    });

    Closure::once_into_js(move || {
        token.store(true, std::sync::atomic::Ordering::SeqCst);
    })
}

#[wasm_bindgen]
pub fn hover(start: usize, end: usize) -> JsValue {
    use wipple_frontend::{
        analysis::{
            typecheck::{
                format::{format_type, Format, TypeFunctionFormat},
                TraitDecl, TypeDecl,
            },
            ExpressionKind, Type,
        },
        helpers::InternedString,
        parse::Span,
        FilePath,
    };

    let analysis = match get_analysis() {
        Some(analysis) => analysis,
        None => return JsValue::NULL,
    };

    let within_hover = |span: Span| {
        span.path == FilePath::Virtual(*PLAYGROUND_PATH) && start >= span.start && end <= span.end
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

    for (constant, expr) in analysis.program.items.values() {
        if constant.is_some() {
            // Skip monomorphized constant types
            continue;
        }

        expr.traverse(|expr| {
            // Don't show type of entire file
            if let Some(entrypoint) = &analysis.program.entrypoint {
                if let Some((_, item)) = analysis.program.items.get(entrypoint) {
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
                },
            ));
        })
    }

    macro_rules! type_decls {
        ($kind:ident $(($opt:tt))?, $str:literal $(, $help:expr)?) => {
            for decl in analysis.program.declarations.$kind.values() {
                for span in std::iter::once(decl.span).chain(decl.uses.iter().copied()) {
                    if !within_hover(span.first()) {
                        continue;
                    }

                    hovers.push((
                        span.first(),
                        HoverOutput {
                            code: format!("{} : {}", decl.name, $str),
                            help: type_decls!(@help decl, $($help)?),
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

    for decl in analysis.program.declarations.constants.values() {
        for span in std::iter::once(decl.span).chain(decl.uses.iter().copied()) {
            if !within_hover(span.first()) {
                continue;
            }

            let format = Format {
                type_function: TypeFunctionFormat::Arrow(&decl.bounds),
                ..Default::default()
            };

            hovers.push((
                span.first(),
                HoverOutput {
                    code: format!("{} :: {}", decl.name, format_type(decl.ty.clone(), format)),
                    help: decl
                        .attributes
                        .decl_attributes
                        .help
                        .iter()
                        .map(|line| line.to_string())
                        .collect::<Vec<_>>()
                        .join("\n"),
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
                },
            ));
        }
    }

    let hover = hovers
        .into_iter()
        .min_by_key(|(span, _)| span.end - span.start)
        .map(|(_, hover)| hover);

    JsValue::from_serde(&hover).unwrap()
}
