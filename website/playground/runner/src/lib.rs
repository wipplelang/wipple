use serde::Serialize;
use std::sync::{Arc, RwLock};
use wasm_bindgen::prelude::*;
use wipple_diagnostics::*;

#[derive(Serialize)]
struct Result {
    annotations: Vec<Annotation>,
    output: Vec<String>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct Annotation {
    span: Span,
    declared_name: Option<String>,
    ty: String,
}

#[wasm_bindgen]
pub fn run(code: &str) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    wipple_frontend::reset_ids();

    let output = Arc::<RwLock<Vec<String>>>::default();
    wipple_interpreter_backend::set_output({
        let output = Arc::clone(&output);
        move |text| output.write().unwrap().push(text.to_string())
    });

    let mut diagnostics = Diagnostics::new();
    let project = wipple_frontend::project::Project::default();

    let (annotations, output) = (|| {
        let mut info = wipple_frontend::compile::Info::with_prelude(&mut diagnostics, &project);
        wipple_frontend::project::load_string("playground", Arc::from(code), &mut info)?;

        let (well_typed, mut item) = wipple_frontend::typecheck::typecheck(info);
        let annotations = annotations(&mut item);

        if well_typed {
            if let Err((error, callstack)) = wipple_interpreter_backend::eval(&item) {
                let mut output = output.write().unwrap();

                output.push(format!("Fatal error: {}", error));

                for (function, span) in callstack {
                    output.push(format!(
                        "  {} ({:?})",
                        function.as_deref().unwrap_or("<function>"),
                        span
                    ))
                }
            }
        }

        let output = output.read().unwrap().clone();

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
    use wipple_frontend::typecheck::*;

    let mut annotations = Vec::new();

    item.traverse(|info, ty| {
        if info.span.file.as_str() != "playground" {
            return;
        }

        annotations.push(Annotation {
            span: info.span,
            declared_name: info.declared_name.map(|name| name.to_string()),
            ty: format_type_schema(ty),
        });
    });

    annotations
}
