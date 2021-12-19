use serde::Serialize;
use std::{cell::RefCell, sync::Arc};
use wasm_bindgen::prelude::*;
use wipple_diagnostics::*;

#[derive(Serialize)]
struct Result {
    annotations: Vec<Annotation>,
    output: Vec<String>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Serialize)]
struct Annotation {
    span: Span,
    value: String,
}

#[wasm_bindgen]
pub fn run(code: &str) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    wipple_frontend::reset_ids();

    let mut diagnostics = Diagnostics::new();
    let project = wipple_frontend::project::Project::default();

    let (annotations, output) = (|| {
        let mut info = wipple_frontend::compile::Info::new(&mut diagnostics, &project);
        wipple_frontend::project::load_string("playground", Arc::from(code), &mut info)?;

        let (well_typed, mut item) = wipple_frontend::typecheck::typecheck(info);
        let annotations = annotations(&mut item);

        let output = Arc::<RefCell<Vec<String>>>::default();
        if well_typed {
            // let external = external({
            //     let output = Arc::clone(&output);
            //     move |text| output.borrow_mut().push(text)
            // });

            if let Err(error) = wipple_interpreter_backend::eval(&item) {
                output
                    .borrow_mut()
                    .push(format!("Fatal error: {:?}", error))
            }
        }
        let output = Arc::try_unwrap(output).unwrap().into_inner();

        Some((annotations, output))
    })()
    .unwrap_or_else(|| (Vec::new(), Vec::new()));

    let result = Result {
        annotations,
        output,
        diagnostics: diagnostics.diagnostics,
    };

    JsValue::from_serde(&result).unwrap()
}

fn annotations(item: &mut wipple_frontend::typecheck::Item) -> Vec<Annotation> {
    let mut annotations = Vec::new();

    item.traverse(|item| {
        annotations.push(Annotation {
            span: item.compile_info.span,
            value: format!(
                "```wipple\n{}\n```",
                wipple_frontend::typecheck::format_type_schema(&item.ty)
            ),
        });
    });

    annotations
}
