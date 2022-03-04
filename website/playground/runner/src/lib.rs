use serde::Serialize;
use std::borrow::Cow;
use wasm_bindgen::prelude::*;

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct CompileResult {
    output: Vec<String>,
    annotations: Vec<Annotation>,
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

    let mut compiler = wipple::Compiler::new(Loader { code });
    let path = wipple::FilePath::Virtual("playground");
    let mut program = compiler.build(path);

    let output = program
        .as_ref()
        .map(|output| vec![format!("{output:#?}")])
        .unwrap_or_default();

    let annotations = program.as_mut().map(annotations).unwrap_or_default();

    let diagnostics = compiler.finish();

    let result = CompileResult {
        output,
        annotations,
        diagnostics: diagnostics
            .diagnostics
            .into_iter()
            .filter(|diagnostic| {
                diagnostic
                    .notes
                    .iter()
                    .all(|note| belongs_to_playground(note.span))
            })
            .collect(),
    };

    JsValue::from_serde(&result).unwrap()
}

struct Loader<'a> {
    code: &'a str,
}

impl<'a> wipple::Loader for Loader<'a> {
    type Error = &'static str;

    fn load(&self, path: wipple::FilePath) -> Result<Cow<'static, str>, Self::Error> {
        match path {
            wipple::FilePath::Virtual("playground") => Ok(Cow::Owned(self.code.to_string())),
            wipple::FilePath::Prelude => Ok(Cow::Borrowed(include_str!(
                "../../../../support/prelude.wpl"
            ))),
            _ => Err("imports are not supported in the playground"),
        }
    }
}

fn annotations(program: &mut wipple::compile::Program) -> Vec<Annotation> {
    use wipple::compile::typecheck::ExpressionKind;

    let mut annotations = Vec::new();

    let declarations = program.declarations.clone();

    macro_rules! format_ty {
        ($ty:expr) => {
            wipple::compile::typecheck::format_type($ty, &declarations.types, |decl| {
                decl.name.as_str()
            })
        };
    }

    program.traverse(|expr| {
        if !belongs_to_playground(expr.span) {
            return;
        }

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
        if !belongs_to_playground(decl.span) {
            continue;
        }

        annotations.push(Annotation {
            span: decl.span,
            value: format!("{} : type", decl.name),
        });
    }

    for decl in declarations.traits.values() {
        if !belongs_to_playground(decl.span) {
            continue;
        }

        annotations.push(Annotation {
            span: decl.span,
            value: format!("{} : trait", decl.name),
        });
    }

    for decl in declarations.variables.values() {
        if !belongs_to_playground(decl.span) {
            continue;
        }

        annotations.push(Annotation {
            span: decl.span,
            value: format!("{} :: {}", decl.name, format_ty!(&decl.value)),
        });
    }

    for decl in declarations.constants.values() {
        if !belongs_to_playground(decl.span) {
            continue;
        }

        annotations.push(Annotation {
            span: decl.span,
            value: format!("{} :: {}", decl.name, format_ty!(&decl.value.ty)),
        });
    }

    annotations
}

fn belongs_to_playground(span: wipple::parser::Span) -> bool {
    matches!(span.path, wipple::FilePath::Virtual("playground"))
}
