use crate::{diagnostics::Diagnostics, lowering::*, parser::Span};

pub fn builtin_external_function(span: Span, _: &mut Diagnostics) -> LoweredExpr {
    LoweredExpr::new(
        span,
        LoweredExprKind::Builtin(LoweredBuiltinExpr::new(
            span,
            LoweredBuiltinExprKind::External,
        )),
    )
}
