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
}

impl LoweredBuiltinExpr {
    pub fn new(span: Span, kind: LoweredBuiltinExprKind) -> Self {
        LoweredBuiltinExpr { span, kind }
    }

    pub fn apply(
        &self,
        span: Span,
        _inputs: &mut Vec<AnyExpr>,
        diagnostics: &mut Diagnostics,
    ) -> LoweredExpr {
        #[allow(clippy::match_single_binding)]
        match self.kind {
            // eventually, builtin functions like 'data'
            LoweredBuiltinExprKind::Assign => {
                diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "':' may not be used as a function",
                    vec![Note::primary(
                        span,
                        "Try adding an expression to each side of ':'",
                    )],
                ));

                LoweredExpr::new(span, LoweredExprKind::Error(LoweredErrorExpr::new()))
            }
        }
    }
}
