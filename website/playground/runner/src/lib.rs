use serde::Serialize;
use std::{cell::RefCell, rc::Rc, sync::Arc};
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
            wipple_frontend::project::load_string("playground", Arc::from(code), &mut info);

        success
            .then(|| {
                info.files
                    .iter()
                    .map(|file| wipple_frontend::typecheck::typecheck(file, info.diagnostics))
                    .collect::<Option<Vec<_>>>()
            })
            .flatten()
    };

    let output = Default::default();

    {
        let external = external(Rc::clone(&output));

        if let Some(files) = files {
            if let Err(error) = wipple_interpreter_backend::eval(&files, external) {
                output
                    .borrow_mut()
                    .push(format!("Fatal error: {:?}", error))
            }
        }
    }

    let output = Rc::try_unwrap(output).unwrap().into_inner();

    let result = Result {
        output,
        diagnostics: diagnostics.diagnostics,
    };

    JsValue::from_serde(&result).unwrap()
}

fn external(output: Rc<RefCell<Vec<String>>>) -> ExternalValues {
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

                output.borrow_mut().push(text);

                Ok(Rc::new(Value::Unit))
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

                Ok(Rc::new(Value::ExternalFunction(ExternalFunction::new(
                    move |input| {
                        let b = match input.as_ref() {
                            Value::Number(n) => *n,
                            _ => unreachable!(),
                        };

                        Ok(Rc::new(Value::Number(a + b)))
                    },
                ))))
            })),
        )
}
