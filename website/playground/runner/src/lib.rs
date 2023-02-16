use lazy_static::lazy_static;
use loader::Fetcher;
use serde::Serialize;
use std::{
    collections::HashMap,
    future::Future,
    pin::Pin,
    sync::{Arc, Mutex},
};
use wasm_bindgen::{prelude::*, JsCast};
use wasm_bindgen_futures::JsFuture;
use wipple_default_loader as loader;
use wipple_frontend::Loader;

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct AnalysisOutput {
    diagnostics: AnalysisOutputDiagnostics,
    syntax_highlighting: Vec<AnalysisOutputSyntaxHighlightingItem>,
}

#[derive(Serialize)]
#[serde(tag = "type", content = "diagnostics", rename_all = "camelCase")]
enum AnalysisOutputDiagnostics {
    Success,
    Warning(String),
    Error(String),
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


    static ref ANALYSIS: Mutex<HashMap<String, Arc<Analysis>>> = Default::default();
}

#[derive(Debug)]
struct Analysis {
    program: wipple_frontend::analysis::Program,
    success: bool,
}

fn save_analysis(id: impl ToString, analysis: Analysis) {
    ANALYSIS
        .lock()
        .unwrap()
        .insert(id.to_string(), Arc::new(analysis));
}

fn get_analysis(id: &str) -> Option<Arc<Analysis>> {
    ANALYSIS.lock().unwrap().get(id).cloned()
}

#[wasm_bindgen]
pub async fn analyze(id: String, code: String, lint: bool) -> JsValue {
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
    let highest_level = diagnostics.highest_level();

    let (files, diagnostics) = diagnostics.into_console_friendly(
        #[cfg(debug_assertions)]
        false,
    );

    let diagnostics = if diagnostics.is_empty() {
        AnalysisOutputDiagnostics::Success
    } else {
        let mut output = Vec::new();
        let mut writer = codespan_reporting::term::termcolor::NoColor::new(&mut output);
        let config = codespan_reporting::term::Config::default();

        for diagnostic in diagnostics {
            codespan_reporting::term::emit(&mut writer, &config, &files, &diagnostic).unwrap();
        }

        let output = String::from_utf8(output).unwrap().trim().to_string();

        match highest_level {
            Some(wipple_frontend::diagnostics::DiagnosticLevel::Warning) => {
                AnalysisOutputDiagnostics::Warning(output)
            }
            Some(wipple_frontend::diagnostics::DiagnosticLevel::Error) => {
                AnalysisOutputDiagnostics::Error(output)
            }
            None => AnalysisOutputDiagnostics::Success,
        }
    };

    let syntax_highlighting = get_syntax_highlighting(&program);

    save_analysis(id, Analysis { program, success });

    let output = AnalysisOutput {
        diagnostics,
        syntax_highlighting,
    };

    JsValue::from_serde(&output).unwrap()
}

fn get_syntax_highlighting(
    program: &wipple_frontend::analysis::Program,
) -> Vec<AnalysisOutputSyntaxHighlightingItem> {
    let playground_path = wipple_frontend::FilePath::Virtual(*PLAYGROUND_PATH);

    let mut items = Vec::new();

    macro_rules! insert_semantic_tokens {
        ($kind:ident, $condition:expr, $token:expr) => {
            for (id, decl) in &program.declarations.$kind {
                if $condition(id, decl) {
                    if decl.span.path == playground_path {
                        items.push(AnalysisOutputSyntaxHighlightingItem {
                            start: decl.span.start,
                            end: decl.span.end,
                            kind: $token(id, decl),
                        });
                    }

                    for &span in &decl.uses {
                        if span.path == playground_path {
                            items.push(AnalysisOutputSyntaxHighlightingItem {
                                start: span.start,
                                end: span.end,
                                kind: $token(id, decl),
                            });
                        }
                    }
                }
            }
        };
        ($kind:ident, $token:expr) => {
            insert_semantic_tokens!($kind, |_, _| true, $token)
        };
    }

    insert_semantic_tokens!(types, |_, _| "type");
    insert_semantic_tokens!(traits, |_, _| "trait");
    insert_semantic_tokens!(constants, |_, _| "variable");
    // insert_semantic_tokens!(operators, |id, _| {
    //     if program
    //         .declarations
    //         .templates
    //         .get(id)
    //         .map_or(false, |decl| decl.attributes.keyword)
    //     {
    //         "keyword"
    //     } else {
    //         "operator"
    //     }
    // });
    // insert_semantic_tokens!(
    //     templates,
    //     |id, _| !program.declarations.operators.contains_key(id),
    //     |_, decl: &wipple_frontend::analysis::typecheck::TemplateDecl| {
    //         if decl.attributes.keyword {
    //             "keyword"
    //         } else {
    //             "template"
    //         }
    //     }
    // );
    insert_semantic_tokens!(builtin_types, |_, _| "type");
    insert_semantic_tokens!(type_parameters, |_, _| "type");
    insert_semantic_tokens!(variables, |_, _| "variable");

    let mut traverse_semantic_tokens = |expr: &wipple_frontend::analysis::Expression| {
        if expr.span.path != playground_path {
            return;
        }

        if matches!(
            expr.kind,
            wipple_frontend::analysis::ExpressionKind::Variable(_)
                | wipple_frontend::analysis::ExpressionKind::Constant(_)
        ) && matches!(expr.ty, wipple_frontend::analysis::Type::Function(_, _))
        {
            items.push(AnalysisOutputSyntaxHighlightingItem {
                start: expr.span.start,
                end: expr.span.end,
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

#[wasm_bindgen]
pub async fn run(id: String, input: js_sys::Function, output: js_sys::Function) -> bool {
    let analysis = match get_analysis(&id) {
        Some(analysis) => analysis,
        None => return false,
    };

    let Analysis { program, success } = &*analysis;

    let program = success.then(|| COMPILER.ir_from(program));

    let input = Arc::new(input);
    let output = Arc::new(output);

    if let Some(program) = program {
        if *success {
            let result = {
                let output = output.clone();

                let mut interpreter = wipple_interpreter_backend::Interpreter::new(
                    move |prompt| {
                        let input = input.clone();

                        Box::pin(async move {
                            wasm_bindgen_futures::JsFuture::from(js_sys::Promise::from(
                                input.call1(&JsValue::NULL, &prompt.into()).unwrap(),
                            ))
                            .await
                            .unwrap()
                            .as_string()
                            .unwrap()
                        })
                    },
                    move |text| {
                        let output = output.clone();

                        Box::pin(async move {
                            wasm_bindgen_futures::JsFuture::from(js_sys::Promise::from(
                                output.call1(&JsValue::NULL, &text.into()).unwrap(),
                            ))
                            .await
                            .unwrap();
                        })
                    },
                );

                interpreter.run(&program).await
            };

            if let Err(error) = result {
                output
                    .call1(&JsValue::NULL, &format!("fatal error: {error}").into())
                    .unwrap();
            }
        }
    }

    true
}

#[wasm_bindgen]
pub fn hover(id: String, start: usize, end: usize) -> JsValue {
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

    let analysis = match get_analysis(&id) {
        Some(analysis) => analysis,
        None => return JsValue::NULL,
    };

    let within_hover = |span: Span| {
        span.path == FilePath::Virtual(*PLAYGROUND_PATH) && start >= span.start && end <= span.end
    };

    let format_type = |ty: Type, format: Format| {
        macro_rules! getter {
            ($kind:ident, $f:expr) => {
                |id| $f(analysis.program.declarations.$kind.get(&id).unwrap().name)
            };
        }

        format_type(
            ty,
            getter!(types, |name: InternedString| name.to_string()),
            getter!(traits, |name: InternedString| name.to_string()),
            getter!(type_parameters, |name: Option<_>| {
                name.as_ref().map(ToString::to_string)
            }),
            format,
        )
    };

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

            if !within_hover(expr.span) {
                return;
            }

            hovers.push((
                expr.span,
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
                    if !within_hover(span) {
                        continue;
                    }

                    hovers.push((
                        span,
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
            if !within_hover(span) {
                continue;
            }

            let format = Format {
                type_function: TypeFunctionFormat::Arrow,
                ..Default::default()
            };
            hovers.push((
                span,
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
            if !within_hover(span) {
                continue;
            }

            let name = match decl.name {
                Some(name) => name,
                None => continue,
            };

            hovers.push((
                span,
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

#[wasm_bindgen]
pub fn remove(id: String) {
    ANALYSIS.lock().unwrap().remove(&id);
}
