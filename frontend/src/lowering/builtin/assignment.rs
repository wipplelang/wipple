use crate::lowering::*;
use std::sync::Arc;
use wipple_diagnostics::*;

pub fn builtin_assignment_operator(span: Span, _: &mut Diagnostics) -> LoweredExpr {
    LoweredExpr::new(
        span,
        LoweredExprKind::Operator(LoweredOperatorExpr::new(
            span,
            OperatorPrecedence::new(9),
            OperatorAssociativity::None,
            Operator {
                apply: Arc::new(move |lhs, rhs, scope, diagnostics| {
                    let lhs_span = lhs.span;
                    let rhs_span = rhs.span;

                    let binding = match lhs.binding() {
                        Some(binding) => binding,
                        None => {
                            diagnostics.add(Diagnostic::new(
                                DiagnosticLevel::Error,
                                "Invalid variable binding",
                                vec![Note::primary(
                                    lhs_span,
                                    "Expected name or other binding here",
                                )],
                            ));

                            return LoweredExpr::new(
                                lhs_span.with_end(span.end),
                                LoweredExprKind::Error,
                            );
                        }
                    };

                    binding.assign(
                        lhs_span.with_end(rhs_span.end),
                        rhs.lower(scope, diagnostics),
                        scope,
                    )
                }),
                partially_apply_left: Arc::new(move |lhs, _, diagnostics| {
                    let span = lhs.span.with_end(span.end);

                    diagnostics.add(Diagnostic::new(
                        DiagnosticLevel::Error,
                        "':' may not be partially applied",
                        vec![Note::primary(
                            span,
                            "Try adding a value to the right-hand side",
                        )],
                    ));

                    LoweredExpr::new(span, LoweredExprKind::Error)
                }),
                partially_apply_right: Arc::new(move |rhs, _, diagnostics| {
                    diagnostics.add(Diagnostic::new(
                        DiagnosticLevel::Error,
                        "':' may not be partially applied",
                        vec![Note::primary(
                            span,
                            "Try adding a value to the left-hand side",
                        )],
                    ));

                    LoweredExpr::new(span.with_end(rhs.span.end), LoweredExprKind::Error)
                }),
            },
        )),
    )
}
