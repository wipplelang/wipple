mod apply;
mod block;
mod builtin;
mod constant;
mod data_block;
mod declare;
mod external_reference;
mod initialize;
mod operator;
mod runtime_variable;
mod unit;

pub use apply::*;
pub use block::*;
pub use builtin::*;
pub use constant::*;
pub use data_block::*;
pub use declare::*;
pub use external_reference::*;
pub use initialize::*;
pub use operator::*;
pub use runtime_variable::*;
pub use unit::*;

use serde::Serialize;
use wipple_diagnostics::*;

#[derive(Debug, Clone, Serialize)]
pub struct LoweredExpr {
    pub span: Span,
    pub kind: LoweredExprKind,
}

#[derive(Debug, Clone, Serialize)]
pub enum LoweredExprKind {
    Unit(LoweredUnitExpr),
    Constant(LoweredConstantExpr),
    Declare(LoweredDeclareExpr),
    Initialize(LoweredInitializeExpr),
    Block(LoweredBlockExpr),
    DataBlock(LoweredDataBlockExpr),
    Apply(LoweredApplyExpr),
    RuntimeVariable(LoweredRuntimeVariableExpr),
    Operator(LoweredOperatorExpr),
    ApplyOperator(LoweredApplyOperatorExpr),
    PartiallyApplyLeftOfOperator(LoweredPartiallyApplyLeftOfOperatorExpr),
    PartiallyApplyRightOfOperator(LoweredPartiallyApplyRightOfOperatorExpr),
    ExternalReference(LoweredExternalReferenceExpr),
    Builtin(LoweredBuiltinExpr),
    Error,
}

impl LoweredExpr {
    pub fn new(span: Span, kind: LoweredExprKind) -> Self {
        LoweredExpr { span, kind }
    }
}
