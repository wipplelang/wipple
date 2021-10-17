use crate::{lowering::*, parser::Span};

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
                let raw_name = name.value;

                let expr_is_runtime_value = !matches!(
                    expr.kind,
                    LoweredExprKind::Operator(_) | LoweredExprKind::Builtin(_)
                );

                if expr_is_runtime_value {
                    scope.insert(name.value, move |span, _| {
                        LoweredExpr::new(
                            span,
                            LoweredExprKind::RuntimeVariable(
                                LoweredRuntimeVariableExpr::in_current_scope(raw_name),
                            ),
                        )
                    });

                    LoweredExpr::new(
                        assignment_span,
                        LoweredExprKind::Initialize(LoweredInitializeExpr::new(raw_name, expr)),
                    )
                } else {
                    scope.insert(name.value, move |_, _| expr.clone());

                    LoweredExpr::new(
                        assignment_span,
                        LoweredExprKind::Unit(LoweredUnitExpr::new()),
                    )
                }
            }
        }
    }
}
