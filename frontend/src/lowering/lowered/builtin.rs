use crate::{
    diagnostics::{Diagnostic, DiagnosticLevel, Diagnostics, Note},
    lowering::*,
    parser::Span,
};
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct LoweredBuiltinExpr {
    pub span: Span,
    pub kind: LoweredBuiltinExprKind,
}

#[derive(Debug, Clone, Serialize)]
pub enum LoweredBuiltinExprKind {
    Assign,
    External,
}

impl LoweredBuiltinExpr {
    pub fn new(span: Span, kind: LoweredBuiltinExprKind) -> Self {
        LoweredBuiltinExpr { span, kind }
    }

    pub fn apply(
        &self,
        inputs: &mut Vec<AnyExpr>,
        scope: &mut Scope,
        diagnostics: &mut Diagnostics,
    ) -> LoweredExpr {
        #[allow(clippy::match_single_binding)]
        match self.kind {
            LoweredBuiltinExprKind::Assign => {
                diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "':' may not be used as a function",
                    vec![Note::primary(
                        self.span,
                        "Try adding an expression to each side of ':'",
                    )],
                ));

                LoweredExpr::new(self.span, LoweredExprKind::Error)
            }
            LoweredBuiltinExprKind::External => {
                macro_rules! get {
                    () => {{
                        let expr = inputs.remove(0).lower(scope, diagnostics);

                        let text = match expr.kind {
                            LoweredExprKind::Constant(LoweredConstantExpr {
                                kind: LoweredConstantExprKind::Text(text),
                            }) => text,
                            _ => {
                                diagnostics.add(Diagnostic::new(
                                    DiagnosticLevel::Error,
                                    "Expected a text literal",
                                    vec![Note::primary(
                                        self.span,
                                        "Try providing a text literal here",
                                    )],
                                ));

                                return LoweredExpr::new(self.span, LoweredExprKind::Error);
                            }
                        };

                        (expr.span, text)
                    }};
                }

                let (namespace_span, namespace) = get!();

                if inputs.is_empty() {
                    diagnostics.add(Diagnostic::new(
                        DiagnosticLevel::Error,
                        "Incomplete 'external' definition",
                        vec![Note::primary(
                            self.span.with_end(namespace_span.end),
                            "Try providing a second text literal",
                        )],
                    ));

                    return LoweredExpr::new(self.span, LoweredExprKind::Error);
                }

                let (identifier_span, identifier) = get!();

                LoweredExpr::new(
                    self.span.with_end(identifier_span.end),
                    LoweredExprKind::ExternalReference(LoweredExternalReferenceExpr::new(
                        namespace, identifier,
                    )),
                )
            }
        }
    }
}
