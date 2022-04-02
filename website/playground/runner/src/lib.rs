use lazy_static::lazy_static;
use serde::Serialize;
use std::{borrow::Cow, sync::Mutex};
use wasm_bindgen::prelude::*;

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct CompileResult {
    output: Vec<(wipple_compiler::parser::Span, String)>,
    annotations: Vec<Annotation>,
    diagnostics: Vec<wipple_compiler::diagnostics::Diagnostic>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct Annotation {
    span: wipple_compiler::parser::Span,
    value: String,
}

lazy_static! {
    static ref PROGRAM: Mutex<Option<wipple_compiler::compile::Program>> = Default::default();
}

#[wasm_bindgen]
pub fn run(code: &str) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    let mut compiler = wipple_compiler::Compiler::new(
        Loader { code },
        wipple_compiler::CompilerOptions::default(),
    );

    let path = wipple_compiler::FilePath::Virtual(wipple_compiler::helpers::InternedString::new(
        "playground",
    ));

    let mut program = compiler.build(path);

    let annotations = program.as_mut().map(annotations).unwrap_or_default();
    let diagnostics = compiler.finish();

    let mut output = Vec::new();

    if let Some(program) = program {
        *PROGRAM.lock().unwrap() = Some(program.clone());

        if !diagnostics.contains_errors() {
            let result = {
                let interpreter =
                    wipple_interpreter_backend::Interpreter::handling_output_with_span(
                        |text, span| output.push((span, text.to_string())),
                    );

                interpreter.eval(program)
            };

            if let Err((error, callstack)) = result {
                let span = *callstack.last().unwrap();
                output.push((span, format!("fatal error: {error}")));
            }
        }
    }

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

impl<'a> wipple_compiler::Loader for Loader<'a> {
    type Error = &'static str;

    fn load(&self, path: wipple_compiler::FilePath) -> Result<Cow<'static, str>, Self::Error> {
        match path {
            wipple_compiler::FilePath::Path(_) => {
                Err("imports are not supported in the playground")
            }
            wipple_compiler::FilePath::Virtual(s) if s.as_str() == "playground" => {
                Ok(Cow::Owned(self.code.to_string()))
            }
            wipple_compiler::FilePath::Prelude => Ok(Cow::Borrowed(include_str!(
                "../../../../support/prelude.wpl"
            ))),
            _ => unimplemented!(),
        }
    }
}

fn annotations(program: &mut wipple_compiler::compile::Program) -> Vec<Annotation> {
    use wipple_compiler::compile::typecheck::{format_type_scheme, ExpressionKind, BUILTIN_TYPES};

    let mut annotations = Vec::new();

    let declarations = program.declarations.clone();

    macro_rules! format_ty {
        ($ty:expr) => {
            format_type_scheme(
                $ty,
                |name| {
                    declarations
                        .types
                        .get(&name)
                        .map(|decl| decl.name.to_string())
                        .unwrap_or_else(|| BUILTIN_TYPES.name(name).unwrap().to_string())
                        .to_string()
                },
                |param| {
                    declarations
                        .type_parameters
                        .get(&param)
                        .unwrap()
                        .name
                        .to_string()
                },
            )
        };
    }

    macro_rules! add_annotation {
        ($expr:expr) => {{
            let expr = $expr;

            if !belongs_to_playground(expr.span) {
                return;
            }

            let ty = format_ty!(&expr.scheme);

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
        }};
    }

    program.traverse(|expr| add_annotation!(expr));

    for decl in declarations.types.values() {
        if !belongs_to_playground(decl.span) {
            continue;
        }

        annotations.push(Annotation {
            span: decl.span,
            value: format!("{} : type", decl.name),
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
            value: format!("{} :: {}", decl.name, format_ty!(&decl.value.scheme)),
        });

        decl.value.traverse(|expr| add_annotation!(expr));
    }

    annotations
}

fn belongs_to_playground(span: wipple_compiler::parser::Span) -> bool {
    matches!(
        span.path,
        wipple_compiler::FilePath::Virtual(s) if s.as_str() == "playground"
    )
}

#[derive(Debug, Clone, Serialize)]
struct Completion {
    name: String,
    kind: usize,
}

#[wasm_bindgen]
pub fn get_completions(position: usize) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    let mut completions = Vec::new();

    macro_rules! add_completions {
        ($declarations:expr) => {{
            use wipple_compiler::compile::lower::ScopeValue;

            let declarations = $declarations;

            completions.extend(declarations.iter().map(|(&name, value)| Completion {
                name: name.to_string(),

                // https://microsoft.github.io/monaco-editor/api/enums/monaco.languages.CompletionItemKind.html
                kind: match value {
                    ScopeValue::Type(_) => 6,
                    ScopeValue::TypeParameter(_) => 24,
                    ScopeValue::Operator(_) => 11,
                    ScopeValue::Constant(_) => 14,
                    ScopeValue::Variable(_, _) => 4,
                },
            }));
        }};
    }

    if let Some(program) = PROGRAM.lock().unwrap().as_mut() {
        add_completions!(&program.top_level);

        program.traverse(|expr| {
            if let wipple_compiler::compile::typecheck::ExpressionKind::Block(_, declarations) =
                &expr.kind
            {
                if belongs_to_playground(expr.span)
                    && position >= expr.span.start
                    && position < expr.span.end
                {
                    add_completions!(declarations);
                }
            }
        });
    }

    JsValue::from_serde(&completions).unwrap()
}
