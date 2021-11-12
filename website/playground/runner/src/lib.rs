use serde::Serialize;
use std::{cell::RefCell, sync::Arc};
use wasm_bindgen::prelude::*;
use wipple_diagnostics::*;
use wipple_frontend::{lower::Info, project::Project};
use wipple_interpreter_backend::{ExternalFunction, ExternalValues, Value};

#[derive(Serialize)]
struct Result {
    annotations: Vec<Annotation>,
    output: Vec<String>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Serialize)]
struct Annotation {
    span: Span,
    value: String,
}

#[wasm_bindgen]
pub fn run(code: &str) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    wipple_frontend::id::reset();

    let mut diagnostics = Diagnostics::new();
    let project = Project::default();

    let (annotations, output) = (|| {
        let mut info = Info::new(&mut diagnostics, &project);

        wipple_frontend::project::load_string("playground", Arc::from(code), &mut info)?;
        let files = wipple_frontend::typecheck::typecheck(&info.files, info.diagnostics)?;

        let entrypoint = files.last().unwrap();
        let annotations = annotations(entrypoint);

        let output = Arc::<RefCell<Vec<String>>>::default();
        {
            let external = external({
                let output = Arc::clone(&output);
                move |text| output.borrow_mut().push(text)
            });

            if let Err(error) = wipple_interpreter_backend::eval(&files, external) {
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

fn annotations(file: &wipple_frontend::typecheck::File) -> Vec<Annotation> {
    file.statements.iter().flat_map(item_annotation).collect()
}

fn item_annotation(item: &wipple_frontend::typecheck::Item) -> Vec<Annotation> {
    use wipple_frontend::typecheck::ItemKind;

    let annotation = |info: &wipple_frontend::typecheck::ItemInfo| Annotation {
        span: info.info.span,
        value: info.to_string(),
    };

    let mut annotations = vec![annotation(&item.info)];

    match &item.kind {
        ItemKind::Block { statements } => {
            annotations.append(&mut statements.iter().flat_map(item_annotation).collect());
        }
        ItemKind::Apply { function, input } => {
            annotations.append(&mut item_annotation(function));
            annotations.append(&mut item_annotation(input));
        }
        ItemKind::Initialize {
            binding_info,
            value,
            ..
        } => {
            annotations.push(annotation(binding_info));
            annotations.append(&mut item_annotation(value));
        }
        ItemKind::Function { body, .. } => {
            annotations.append(&mut item_annotation(body));
        }
        ItemKind::Unit { .. }
        | ItemKind::Number { .. }
        | ItemKind::Text { .. }
        | ItemKind::Variable { .. }
        | ItemKind::FunctionInput
        | ItemKind::External { .. } => {}
    }

    annotations
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
