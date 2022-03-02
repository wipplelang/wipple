use serde::Serialize;
use wasm_bindgen::prelude::*;

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct Result {
    annotations: Vec<Annotation>,
    output: Vec<String>,
    diagnostics: Vec<wipple::diagnostics::Diagnostic>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct Annotation {
    span: wipple::parser::Span,
    value: String,
}

#[wasm_bindgen]
pub fn run(code: &str) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    wipple::helpers::reset_ids();

    let (mut file, diagnostics) = wipple::compile("playground", code);

    let result = Result {
        annotations: file.as_mut().map(annotations).unwrap_or_default(),
        output: file
            .map(|output| vec![format!("{output:#?}")])
            .unwrap_or_default(),
        diagnostics: diagnostics.diagnostics,
    };

    JsValue::from_serde(&result).unwrap()
}

fn annotations(file: &mut wipple::compile::File) -> Vec<Annotation> {
    use wipple::compile::ExpressionKind;

    let mut annotations = Vec::new();

    let declarations = file.declarations.clone();

    macro_rules! format_ty {
        ($ty:expr) => {
            wipple::compile::format_type($ty, &declarations.types, |decl| decl.name.as_str())
        };
    }

    file.traverse(|expr| {
        let ty = format_ty!(&expr.ty);

        let name = match expr.kind {
            ExpressionKind::Variable(id) => Some(declarations.variables.get(&id).unwrap().name),
            ExpressionKind::Constant(id) => Some(declarations.constants.get(&id).unwrap().name),
            _ => None,
        };

        annotations.push(Annotation {
            span: expr.span,
            value: match name {
                Some(name) => format!("{name} :: {ty}"),
                None => ty,
            },
        });
    });

    for decl in declarations.types.values() {
        annotations.push(Annotation {
            span: decl.span,
            value: format!("{} : type", decl.name),
        });
    }

    for decl in declarations.traits.values() {
        annotations.push(Annotation {
            span: decl.span,
            value: format!("{} : trait", decl.name),
        });
    }

    for decl in declarations.variables.values() {
        annotations.push(Annotation {
            span: decl.span,
            value: format!("{} :: {}", decl.name, format_ty!(&decl.ty)),
        });
    }

    for decl in declarations.constants.values() {
        annotations.push(Annotation {
            span: decl.span,
            value: format!("{} :: {}", decl.name, format_ty!(&decl.ty)),
        });
    }

    annotations
}
