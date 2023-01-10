use lazy_static::lazy_static;
use loader::Fetcher;
use serde::Serialize;
use std::{future::Future, io::Write, pin::Pin, sync::Arc};
use wasm_bindgen::{prelude::*, JsCast};
use wasm_bindgen_futures::JsFuture;
use wipple_default_loader as loader;

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct Output {
    status: Status,
    value: String,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
enum Status {
    Success,
    Warning,
    Error,
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
                                Some(error) => format!("{}: {}", msg, error),
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
}

#[wasm_bindgen]
pub async fn run(code: String, lint: bool) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    #[cfg(debug_assertions)]
    wipple_frontend::diagnostics::set_backtrace_enabled(false);

    let loader = LOADER.clone();

    let playground_path = wipple_frontend::helpers::InternedString::new("playground");

    loader
        .virtual_paths
        .lock()
        .insert(playground_path, Arc::from(code));

    let compiler = wipple_frontend::Compiler::new(&loader);

    let (program, diagnostics) = compiler
        .analyze_with(
            wipple_frontend::FilePath::Virtual(playground_path),
            &wipple_frontend::analysis::Options::new().lint(lint),
        )
        .await;

    let success = !diagnostics.contains_errors();

    let program = success.then(|| compiler.ir_from(&program));

    let status = match diagnostics.highest_level() {
        None => Status::Success,
        Some(wipple_frontend::diagnostics::DiagnosticLevel::Warning) => Status::Warning,
        Some(wipple_frontend::diagnostics::DiagnosticLevel::Error) => Status::Error,
    };

    let (files, diagnostics) = diagnostics.into_console_friendly(
        #[cfg(debug_assertions)]
        false,
    );

    let mut output = Vec::new();

    if !diagnostics.is_empty() {
        let mut writer = codespan_reporting::term::termcolor::NoColor::new(&mut output);

        let config = codespan_reporting::term::Config::default();

        for diagnostic in diagnostics {
            codespan_reporting::term::emit(&mut writer, &config, &files, &diagnostic).unwrap();
        }
    }

    if let Some(program) = program {
        if matches!(status, Status::Success | Status::Warning) {
            let result = {
                let mut interpreter =
                    wipple_interpreter_backend::Interpreter::handling_output(|text| {
                        write!(output, "{}", text).unwrap();
                    });

                interpreter.run(&program)
            };

            if let Err(error) = result {
                write!(output, "fatal error: {error}").unwrap();
            }
        }
    }

    let output = Output {
        status,
        value: String::from_utf8(output).unwrap().trim().to_string(),
    };

    JsValue::from_serde(&output).unwrap()
}
