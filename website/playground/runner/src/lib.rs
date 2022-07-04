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
    success: bool,
    value: String,
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
            Some(wipple_compiler::FilePath::Path(
                #[cfg(feature = "debug_playground")]
                wipple_compiler::helpers::InternedString::new(format!(
                    "{}pkg/std/std.wpl",
                    env!("CARGO_WORKSPACE_DIR")
                )),
                #[cfg(not(feature = "debug_playground"))]
                wipple_compiler::helpers::InternedString::new(loader::STD_URL),
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
pub async fn run(code: String) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    let loader = LOADER.clone();

    let playground_path = wipple_compiler::helpers::InternedString::new("playground");

    loader
        .virtual_paths
        .lock()
        .insert(playground_path, Arc::from(code));

    let mut compiler = wipple_compiler::Compiler::new(loader);

    let program = compiler
        .build(wipple_compiler::FilePath::Virtual(playground_path))
        .await;

    let program = program.map(|program| {
        compiler.lint(&program);
        compiler.optimize(program)
    });

    let diagnostics = compiler.finish();
    let success = !diagnostics.contains_errors();

    let (_, codemap, diagnostics) = diagnostics.into_console_friendly(
        #[cfg(debug_assertions)]
        false,
    );

    let mut output = Vec::new();

    if !diagnostics.is_empty() {
        let mut emitter = codemap_diagnostic::Emitter::vec(&mut output, Some(&codemap));
        emitter.emit(&diagnostics);
    }

    if let Some(program) = program {
        if success {
            let result = {
                let interpreter =
                    wipple_interpreter_backend::Interpreter::handling_output_with_span(
                        |text, _| {
                            write!(output, "{}", text).unwrap();
                        },
                    );

                interpreter.eval(program)
            };

            if let Err((error, _)) = result {
                write!(output, "fatal error: {error}").unwrap();
            }
        }
    }

    let output = Output {
        success,
        value: String::from_utf8(output).unwrap().trim().to_string(),
    };

    JsValue::from_serde(&output).unwrap()
}
