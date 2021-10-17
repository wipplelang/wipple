use crate::lowering::*;
use wipple_diagnostics::*;
use wipple_parser::Intern;

#[derive(Debug)]
pub struct NameExpr {
    pub span: Span,
    pub value: Intern<String>,
}

impl NameExpr {
    pub fn new(span: Span, value: Intern<String>) -> Self {
        NameExpr { span, value }
    }
}

impl Expr for NameExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn lower(self, scope: &mut Scope, diagnostics: &mut Diagnostics) -> LoweredExpr {
        match scope.resolve(&self, diagnostics) {
            Some(mut expr) => {
                expr.span = self.span;
                expr
            }
            None => {
                diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    format!("'{}' is not defined", self.value),
                    vec![Note::primary(
                        self.span,
                        "This name does not resolve to a variable",
                    )],
                ));

                LoweredExpr::new(self.span, LoweredExprKind::Error)
            }
        }
    }

    fn operator(
        &self,
        scope: &Scope,
        diagnostics: &mut Diagnostics,
    ) -> Option<LoweredOperatorExpr> {
        scope
            .resolve(self, diagnostics)
            .and_then(|expr| match expr.kind {
                LoweredExprKind::Operator(operator) => Some(operator),
                _ => None,
            })
    }

    fn binding(self) -> Option<Binding> {
        Some(Binding::Name(self))
    }
}
