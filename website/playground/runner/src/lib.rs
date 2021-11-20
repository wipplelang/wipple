use serde::Serialize;
use std::{cell::RefCell, collections::HashMap, sync::Arc};
use wasm_bindgen::prelude::*;
use wipple_diagnostics::*;
use wipple_interpreter_backend::{ExternalFunction, ExternalValues, Value};

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
    let project = wipple_frontend::Project::default();

    let (annotations, output) = (|| {
        let mut info = wipple_frontend::Info::new(&mut diagnostics, &project);

        let entrypoint = wipple_frontend::load_string("playground", Arc::from(code), &mut info)?;
        let (files, types) = wipple_frontend::typecheck(&mut info)?;
        let annotations = annotations(&entrypoint, &types);

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

fn annotations(
    file: &wipple_frontend::File,
    types: &HashMap<wipple_frontend::ItemId, wipple_frontend::TypeSchema>,
) -> Vec<Annotation> {
    file.statements
        .iter()
        .flat_map(|statement| item_annotation(statement, types))
        .collect()
}

fn item_annotation(
    item: &wipple_frontend::Item,
    types: &HashMap<wipple_frontend::ItemId, wipple_frontend::TypeSchema>,
) -> Vec<Annotation> {
    use wipple_frontend::ItemKind;

    let mut annotations = vec![Annotation {
        span: item.debug_info.span,
        value: format!(
            "```wipple\n{}\n```",
            wipple_frontend::format_type_schema(types.get(&item.id).unwrap())
        ),
    }];

    match &item.kind {
        ItemKind::Block { statements } => {
            annotations.append(
                &mut statements
                    .iter()
                    .flat_map(|statement| item_annotation(statement, types))
                    .collect(),
            );
        }
        ItemKind::Apply { function, input } => {
            annotations.append(&mut item_annotation(function, types));
            annotations.append(&mut item_annotation(input, types));
        }
        ItemKind::Initialize { value, .. } => {
            annotations.append(&mut item_annotation(value, types));
        }
        ItemKind::Function { body, .. } => {
            annotations.append(&mut item_annotation(body, types));
        }
        ItemKind::Unit { .. }
        | ItemKind::Number { .. }
        | ItemKind::Text { .. }
        | ItemKind::Variable { .. }
        | ItemKind::FunctionInput
        | ItemKind::External { .. }
        | ItemKind::Annotate { .. } => {}
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
