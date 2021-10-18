use crate::lowering::*;
use wipple_diagnostics::*;

pub enum Binding {
    Name(NameExpr),
    // eventually, destructuring and conversions
}

impl Binding {
    pub fn assign(
        self,
        assignment_span: Span,
        expr: LoweredExpr,
        scope: &mut Scope,
    ) -> LoweredExpr {
        match self {
            Binding::Name(name) => {
                let variable = Variable::new(name.span, name.value);

                let expr_is_runtime_value = !matches!(
                    expr.kind,
                    LoweredExprKind::Operator(_)
                        | LoweredExprKind::ExternalReference(_)
                        | LoweredExprKind::Builtin(_)
                );

                if expr_is_runtime_value {
                    scope.insert(name.value, move |span, _| {
                        LoweredExpr::new(
                            span,
                            LoweredExprKind::RuntimeVariable(LoweredRuntimeVariableExpr::new(
                                variable,
                            )),
                        )
                    });

                    LoweredExpr::new(
                        assignment_span,
                        LoweredExprKind::Initialize(LoweredInitializeExpr::new(variable, expr)),
                    )
                } else {
                    scope.insert(name.value, move |_, _| expr.clone());

                    LoweredExpr::new(
                        assignment_span,
                        LoweredExprKind::Declare(LoweredDeclareExpr::new(variable)),
                    )
                }
            }
        }
    }
}
