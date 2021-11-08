use serde::Serialize;
use std::{cell::RefCell, sync::Arc};
use wasm_bindgen::prelude::*;
use wipple_diagnostics::*;
use wipple_frontend::{lower::Info, project::Project};
use wipple_interpreter_backend::{ExternalFunction, ExternalValues, Value};

#[derive(Serialize)]
struct Result {
    output: Vec<String>,
    diagnostics: Vec<Diagnostic>,
}

#[wasm_bindgen]
pub fn run(code: &str) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    wipple_frontend::id::reset();

    let mut diagnostics = Diagnostics::new();
    let project = Project::default();

    let files = {
        let mut info = Info::new(&mut diagnostics, &project);

        let success =
            wipple_frontend::project::load_string("playground", Arc::from(code), &mut info)
                .is_some();

        success
            .then(|| wipple_frontend::typecheck::typecheck(&info.files, info.diagnostics))
            .flatten()
    };

    let output = Arc::<RefCell<Vec<String>>>::default();

    {
        let external = external({
            let output = Arc::clone(&output);
            move |text| output.borrow_mut().push(text)
        });

        if let Some(files) = files {
            if let Err(error) = wipple_interpreter_backend::eval(&files, external) {
                output
                    .borrow_mut()
                    .push(format!("Fatal error: {:?}", error))
            }
        }
    }

    let output = Arc::try_unwrap(output).unwrap().into_inner();

    let result = Result {
        output,
        diagnostics: diagnostics.diagnostics,
    };

    JsValue::from_serde(&result).unwrap()
}

pub fn external(on_output: impl Fn(String) + 'static) -> ExternalValues {
    ExternalValues::new()
        .insert("math", "pi", Value::Number("3.14".parse().unwrap()))
        .insert(
            "internal",
            "show",
            Value::ExternalFunction(ExternalFunction::new(move |input| {
                let text = match input.as_ref() {
                    Value::Text(text) => text.clone(),
                    _ => format!("{:?}", input),
                };

                on_output(text);

                Ok(Arc::new(Value::Unit))
            })),
        )
        .insert(
            "math",
            "add",
            Value::ExternalFunction(ExternalFunction::new(|input| {
                let a = match input.as_ref() {
                    Value::Number(n) => *n,
                    _ => unreachable!(),
                };

                Ok(Arc::new(Value::ExternalFunction(ExternalFunction::new(
                    move |input| {
                        let b = match input.as_ref() {
                            Value::Number(n) => *n,
                            _ => unreachable!(),
                        };

                        Ok(Arc::new(Value::Number(a + b)))
                    },
                ))))
            })),
        )
}
