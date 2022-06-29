use loader::Fetcher;
use serde::Serialize;
use std::{cell::RefCell, io::Write, rc::Rc, sync::Arc};
use wasm_bindgen::prelude::*;
use wipple_default_loader as loader;

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct Output {
    success: bool,
    value: String,
}

thread_local! {
    static LOADER: Rc<RefCell<Option<loader::Loader>>> = Default::default();
}

#[wasm_bindgen]
pub fn run(code: String, fetch: js_sys::Function) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    let playground_path = wipple_compiler::helpers::InternedString::new("playground");

    let loader = LOADER.with(Clone::clone);
    let mut loader = loader.borrow_mut();

    let loader = loader.get_or_insert_with(|| {
        loader::Loader::with_fetcher(
            String::new(),
            Some(wipple_compiler::FilePath::Path(
                #[cfg(feature = "debug_playground")]
                wipple_compiler::helpers::InternedString::new(format!(
                    "{}std/std.wpl",
                    env!("CARGO_WORKSPACE_DIR")
                )),
                #[cfg(not(feature = "debug_playground"))]
                wipple_compiler::helpers::InternedString::new(loader::STD_URL),
            )),
            Fetcher::new(
                |path| {
                    #[cfg(feature = "debug_playground")]
                    {
                        #[derive(rust_embed::RustEmbed)]
                        #[folder = "$CARGO_WORKSPACE_DIR/std"]
                        struct EmbeddedStd;

                        let path = std::path::PathBuf::from(path);
                        let path = path.file_name().unwrap().to_str().unwrap();

                        let file = EmbeddedStd::get(path)
                            .map(|file| String::from_utf8(file.data.to_vec()).unwrap())
                            .ok_or_else(|| anyhow::Error::msg("file does not exist"))?;

                        Ok(file)
                    }

                    #[cfg(not(feature = "debug_playground"))]
                    {
                        let _ = path;
                        Err(anyhow::Error::msg("loading files from paths is not supported in the playground; try loading from a URL instead"))
                    }
                },
                move |url| {
                    fetch
                        .call1(&JsValue::null(), &JsValue::from_str(url.as_str()))
                        .map_err(|e| anyhow::Error::msg(format!("request failed: {:?}", e)))
                        .map(|value| {
                            value
                                .as_string()
                                .expect("playground didn't return a string")
                        })
                },
            ),
        )
    });

    loader
        .virtual_paths
        .insert(playground_path, Arc::from(code));

    let mut compiler = wipple_compiler::Compiler::new(loader);

    let program = compiler.build(wipple_compiler::FilePath::Virtual(playground_path));

    let program = program.map(|program| {
        compiler.lint(&program);
        compiler.optimize(program)
    });

    let diagnostics = compiler.finish();
    let success = !diagnostics.contains_errors();

    let (codemap, diagnostics) = diagnostics.into_console_friendly(
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
