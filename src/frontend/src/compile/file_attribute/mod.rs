mod builtin;
pub use builtin::*;

use crate::*;
use serde::Serialize;
use std::{fmt, vec::IntoIter};

pub fn evaluate_attributes(
    attributes: Vec<wipple_parser::FileAttribute>,
    stack: &Stack,
    info: &mut Info,
) -> Option<()> {
    let mut success = true;
    let builtin_file_attributes = builtin_file_attributes();

    for attribute in attributes {
        let mut exprs = parse_lines(attribute.lines).into_iter();

        let name = match exprs.next() {
            Some(Expr::Name(name)) => name,
            Some(_) => {
                info.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Expected name of attribute",
                    vec![Note::primary(attribute.span, "Expected name here")],
                ));

                return None;
            }
            None => {
                info.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Attribute may not be empty",
                    vec![Note::primary(attribute.span, "Try removing this attribute")],
                ));

                return None;
            }
        };

        let attribute = match builtin_file_attributes.get(&name.value) {
            Some(attribute) => attribute,
            None => {
                info.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Unknown file attribute",
                    vec![Note::primary(
                        attribute.span,
                        "Try using an expression attribute instead: '[ ... ]'",
                    )],
                ));

                return None;
            }
        };

        success = attribute.evaluate(exprs, name.span, stack, info).is_some();
    }

    success.then(|| ())
}

#[derive(Clone, Serialize)]
pub struct FileAttribute {
    pub arity: Option<usize>,

    #[serde(skip)]
    #[allow(clippy::type_complexity)]
    pub evaluate: Arc<dyn Fn(IntoIter<Expr>, Span, &Stack, &mut Info) -> Option<()>>,
}

impl FileAttribute {
    pub fn new(
        arity: Option<usize>,
        evaluate: impl Fn(IntoIter<Expr>, Span, &Stack, &mut Info) -> Option<()> + 'static,
    ) -> Self {
        FileAttribute {
            arity,
            evaluate: Arc::new(evaluate),
        }
    }

    pub fn evaluate(
        &self,
        exprs: IntoIter<Expr>,
        span: Span,
        stack: &Stack,
        info: &mut Info,
    ) -> Option<()> {
        if let Some(arity) = self.arity {
            assert_eq!(exprs.len(), arity);
        }

        (self.evaluate)(exprs, span, stack, info)
    }
}

impl fmt::Debug for FileAttribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FileAttribute")
            .field("arity", &self.arity)
            .finish()
    }
}
