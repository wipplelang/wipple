use itertools::Itertools;
use lazy_static::lazy_static;
use serde::Serialize;
use std::{borrow::Cow, collections::HashMap, sync::Mutex};
use wasm_bindgen::prelude::*;

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct CompileResult {
    output: Vec<(wipple_compiler::parse::Span, String)>,
    annotations: Vec<Annotation>,
    diagnostics: Vec<wipple_compiler::diagnostics::Diagnostic>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct Annotation {
    span: wipple_compiler::parse::Span,
    value: String,
}

lazy_static! {
    static ref PROGRAM: Mutex<Option<wipple_compiler::compile::Program>> = Default::default();
}

#[wasm_bindgen]
pub fn run(code: &str) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    let mut compiler = wipple_compiler::Compiler::new(Loader { code });

    let path = wipple_compiler::FilePath::Virtual(wipple_compiler::helpers::InternedString::new(
        "playground",
    ));

    let mut program = compiler.build(path);

    let annotations = program.as_mut().map(annotations).unwrap_or_default();

    let program = program.map(|program| {
        compiler.lint(&program);
        (program.clone(), compiler.optimize(program))
    });

    let (_, diagnostics) = compiler.finish();

    let mut output = Vec::new();

    if let Some((program, optimized_program)) = program {
        *PROGRAM.lock().unwrap() = Some(program);

        if !diagnostics.contains_errors() {
            let result = {
                let interpreter =
                    wipple_interpreter_backend::Interpreter::handling_output_with_span(
                        |text, stack| {
                            let span = stack
                                .iter()
                                .rev()
                                .find(|&&span| belongs_to_playground(span))
                                .unwrap();

                            output.push((*span, text.to_string()))
                        },
                    );

                interpreter.eval(optimized_program)
            };

            if let Err((error, stack)) = result {
                let span = stack
                    .into_iter()
                    .rev()
                    .find(|&span| belongs_to_playground(span))
                    .unwrap();

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

    fn load(
        &mut self,
        path: wipple_compiler::FilePath,
        _current: Option<wipple_compiler::FilePath>,
    ) -> Result<(wipple_compiler::FilePath, Cow<'static, str>), Self::Error> {
        match path {
            wipple_compiler::FilePath::Path(_) => {
                Err("imports are not supported in the playground")
            }
            wipple_compiler::FilePath::Virtual(s) if s.as_str() == "playground" => Ok((
                wipple_compiler::FilePath::Virtual(s),
                Cow::Owned(self.code.to_string()),
            )),
            wipple_compiler::FilePath::Prelude => Ok((
                wipple_compiler::FilePath::Prelude,
                Cow::Borrowed(include_str!("../../../../support/prelude.wpl")),
            )),
            _ => unimplemented!(),
        }
    }
}

fn annotations(program: &mut wipple_compiler::compile::Program) -> Vec<Annotation> {
    use wipple_compiler::compile::typecheck::{format_type, ExpressionKind};

    let mut annotations = Vec::new();

    let declarations = program.declarations.clone();

    macro_rules! format_ty {
        ($ty:expr) => {
            format_type(
                $ty,
                |id| {
                    declarations
                        .types
                        .get(&id)
                        .and_then(|decl| decl.name)
                        .as_deref()
                        .unwrap_or("<unknown>")
                        .to_string()
                },
                |id| {
                    declarations
                        .type_parameters
                        .get(&id)
                        .and_then(|decl| decl.name)
                        .as_deref()
                        .unwrap_or("<unknown>")
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

            let ty = format_ty!(expr.ty.clone());

            let name = match expr.kind {
                ExpressionKind::Variable(id) => declarations
                    .variables
                    .get(&id)
                    .map(|decl| decl.name.unwrap().to_string()),
                ExpressionKind::Constant(id) => declarations
                    .monomorphized_constants
                    .get(&id)
                    .map(|((), decl)| decl.name.unwrap().to_string()),
                _ => None,
            };

            annotations.push(Annotation {
                span: expr.span,
                value: match name {
                    Some(name) => format!("{name} :: {ty}"),
                    None => ty.to_string(),
                },
            });
        }};
    }

    program.traverse_body_mut(|expr| add_annotation!(expr));

    for decl in declarations.types.values() {
        if !belongs_to_playground(decl.span) {
            continue;
        }

        annotations.push(Annotation {
            span: decl.span,
            value: format!("{} : type", decl.name.unwrap()),
        });
    }

    for decl in declarations.traits.values() {
        if !belongs_to_playground(decl.span) {
            continue;
        }

        annotations.push(Annotation {
            span: decl.span,
            value: format!("{} : trait", decl.name.unwrap()),
        });
    }

    for constant in declarations.generic_constants.values() {
        if !belongs_to_playground(constant.decl.span) {
            continue;
        }

        // Ignore instances, which are unnamed
        if let Some(name) = constant.decl.name {
            annotations.push(Annotation {
                span: constant.decl.span,
                value: format!("{} :: {}", name, format_ty!(constant.decl.value.ty.clone())),
            });
        }

        constant.decl.value.traverse(|expr| add_annotation!(expr));
    }

    for decl in declarations.variables.values() {
        if !belongs_to_playground(decl.span) {
            continue;
        }

        annotations.push(Annotation {
            span: decl.span,
            value: format!(
                "{} :: {}",
                decl.name.unwrap(),
                format_ty!(decl.value.clone())
            ),
        });
    }

    annotations
}

fn belongs_to_playground(span: wipple_compiler::parse::Span) -> bool {
    matches!(
        span.path,
        wipple_compiler::FilePath::Virtual(s) if s.as_str() == "playground"
    )
}

#[derive(Debug, Clone, Serialize)]
struct Completion {
    name: String,
    kind: usize,
    doc: Option<String>,
}

#[wasm_bindgen]
pub fn get_completions(position: usize) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    let keywords = [
        (
            "use",
            "Make the contents of another file available in this file.",
        ),
        ("when", "Make a decision out of several choices."),
        (
            "type",
            "Declare a new type that represents data in your program.",
        ),
        (
            "trait",
            "Declare a new trait that represents functionality shared between types.",
        ),
        ("instance", "Add a relationship between types via a trait."),
        ("where", "Specify additional bounds or conditions."),
        (
            "external",
            "Call a function implemented in an external library.",
        ),
    ];

    let mut completions = keywords
        .into_iter()
        .map(|(keyword, doc)| Completion {
            name: keyword.to_string(),
            kind: 17,
            doc: Some(doc.to_string()),
        })
        .collect::<Vec<_>>();

    fn add_completions(
        program: &mut wipple_compiler::compile::Program,
        declarations: HashMap<
            wipple_compiler::helpers::InternedString,
            wipple_compiler::compile::lower::ScopeValue,
        >,
        completions: &mut Vec<Completion>,
    ) {
        use wipple_compiler::compile::lower::ScopeValue;

        completions.extend(declarations.into_iter().map(|(name, value)| {
            fn join<'a>(
                doc: impl IntoIterator<Item = &'a wipple_compiler::helpers::InternedString>,
            ) -> String {
                Itertools::intersperse(doc.into_iter().map(|s| s.as_str()), "\n").collect()
            }

            // https://microsoft.github.io/monaco-editor/api/enums/monaco.languages.CompletionItemKind.html
            let (kind, doc) = match value {
                ScopeValue::Type(id) => {
                    let ty = program.declarations.types.get(&id).unwrap();
                    (6, Some(join(&ty.value.attributes.doc)))
                }
                ScopeValue::BuiltinType(_) => (6, None),
                ScopeValue::Trait(id) => {
                    let tr = program.declarations.traits.get(&id).unwrap();
                    (7, Some(join(&tr.value.attributes.doc)))
                }
                ScopeValue::TypeParameter(_) => (24, None),
                ScopeValue::Operator(_) => (11, None),
                ScopeValue::Constant(id, _) => {
                    let constant = program.declarations.generic_constants.get(&id).unwrap();
                    (
                        14,
                        constant
                            .attributes
                            .as_ref()
                            .map(|attributes| join(&attributes.doc)),
                    )
                }
                ScopeValue::Variable(_) => (4, None),
            };

            Completion {
                name: name.to_string(),
                kind,
                doc,
            }
        }));
    }

    if let Some(program) = PROGRAM.lock().unwrap().as_mut() {
        add_completions(program, program.top_level.clone(), &mut completions);

        let mut declarations = Vec::new();
        program.traverse_body_mut(|expr| {
            if let wipple_compiler::compile::typecheck::ExpressionKind::Block(
                _,
                block_declarations,
            ) = &expr.kind
            {
                if belongs_to_playground(expr.span)
                    && position >= expr.span.start
                    && position < expr.span.end
                {
                    declarations.push(block_declarations.clone());
                }
            }
        });
    }

    JsValue::from_serde(&completions).unwrap()
}
