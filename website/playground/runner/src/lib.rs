use serde::Serialize;
use std::sync::Arc;
use wasm_bindgen::prelude::*;
use wipple_diagnostics::*;
use wipple_frontend::{lower::Info, project::Project};
use wipple_interpreter_backend::ExternalFunctions;

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
    let external = ExternalFunctions::new();

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

    if let Some(files) = files {
        wipple_interpreter_backend::eval(&files, external);
    }

    // TODO: 'show' external function
    let output = vec![String::from(
        "I'm still working on playground support for 'show'. Check back later!",
    )];

    let result = Result {
        output,
        diagnostics: diagnostics.diagnostics,
    };

    JsValue::from_serde(&result).unwrap()
}
