use lazy_static::lazy_static;
use serde::Serialize;
use std::{borrow::Cow, io::Write, sync::Mutex};
use wasm_bindgen::prelude::*;

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct Output {
    success: bool,
    value: String,
}

lazy_static! {
    static ref PROGRAM: Mutex<Option<wipple_compiler::compile::Program>> = Default::default();
}

#[wasm_bindgen]
pub fn run(code: &str) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    let mut compiler = wipple_compiler::Compiler::new(Loader { code });

    let path = wipple_compiler::FilePath::Virtual(wipple_compiler::helpers::InternedString::new(
        "playground",
    ));

    let program = compiler.build(path);

    let program = program.map(|program| {
        compiler.lint(&program);
        (program.clone(), compiler.optimize(program))
    });

    let (_, diagnostics) = compiler.finish();
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

    if let Some((program, optimized_program)) = program {
        *PROGRAM.lock().unwrap() = Some(program);

        if success {
            let result = {
                let interpreter =
                    wipple_interpreter_backend::Interpreter::handling_output_with_span(
                        |text, _| {
                            write!(output, "{}", text).unwrap();
                        },
                    );

                interpreter.eval(optimized_program)
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

struct Loader<'a> {
    code: &'a str,
}

impl<'a> wipple_compiler::Loader for Loader<'a> {
    type Error = &'static str;

    fn load(
        &mut self,
        path: wipple_compiler::FilePath,
        _current: Option<wipple_compiler::FilePath>,
    ) -> Result<(wipple_compiler::FilePath, Cow<'static, str>), Self::Error> {
        match path {
            wipple_compiler::FilePath::Path(_) => {
                Err("imports are not supported in the playground")
            }
            wipple_compiler::FilePath::Virtual(s) if s.as_str() == "playground" => Ok((
                wipple_compiler::FilePath::Virtual(s),
                Cow::Owned(self.code.to_string()),
            )),
            wipple_compiler::FilePath::Prelude => Ok((
                wipple_compiler::FilePath::Prelude,
                Cow::Borrowed(include_str!("../../../../support/prelude.wpl")),
            )),
            _ => unimplemented!(),
        }
    }
}
