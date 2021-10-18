use crate::lowering::*;
use serde::Serialize;
use std::{fmt, sync::Arc};
use wipple_diagnostics::*;

#[derive(Debug, Clone, Serialize)]
pub struct LoweredOperatorExpr {
    pub span: Span,
    pub precedence: OperatorPrecedence,
    pub associativity: OperatorAssociativity,
    #[serde(skip)]
    pub operator: Operator,
}

impl LoweredOperatorExpr {
    pub fn new(
        span: Span,
        precedence: OperatorPrecedence,
        associativity: OperatorAssociativity,
        operator: Operator,
    ) -> Self {
        LoweredOperatorExpr {
            span,
            precedence,
            associativity,
            operator,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct OperatorPrecedence(u8);

impl OperatorPrecedence {
    pub fn new(value: u8) -> Self {
        debug_assert!(matches!(value, 0..=9));

        OperatorPrecedence(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum OperatorAssociativity {
    Left,
    Right,
    None,
}

#[derive(Debug, Clone, Serialize)]
pub struct LoweredApplyOperatorExpr {
    pub lhs: Box<LoweredExpr>,
    pub operator: Box<LoweredExpr>,
    pub rhs: Box<LoweredExpr>,
}

impl LoweredApplyOperatorExpr {
    pub fn new(lhs: LoweredExpr, operator: LoweredExpr, rhs: LoweredExpr) -> Self {
        LoweredApplyOperatorExpr {
            lhs: Box::new(lhs),
            operator: Box::new(operator),
            rhs: Box::new(rhs),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct LoweredPartiallyApplyLeftOfOperatorExpr {
    pub lhs: Box<LoweredExpr>,
    pub operator: Box<LoweredExpr>,
}

impl LoweredPartiallyApplyLeftOfOperatorExpr {
    pub fn new(lhs: LoweredExpr, operator: LoweredExpr) -> Self {
        LoweredPartiallyApplyLeftOfOperatorExpr {
            lhs: Box::new(lhs),
            operator: Box::new(operator),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct LoweredPartiallyApplyRightOfOperatorExpr {
    pub operator: Box<LoweredExpr>,
    pub rhs: Box<LoweredExpr>,
}

impl LoweredPartiallyApplyRightOfOperatorExpr {
    pub fn new(operator: LoweredExpr, rhs: LoweredExpr) -> Self {
        LoweredPartiallyApplyRightOfOperatorExpr {
            operator: Box::new(operator),
            rhs: Box::new(rhs),
        }
    }
}

#[derive(Clone)]
pub struct Operator {
    pub apply: Arc<dyn Fn(ListExpr, ListExpr, &mut Scope, &mut Diagnostics) -> LoweredExpr>,
    pub partially_apply_left: Arc<dyn Fn(ListExpr, &mut Scope, &mut Diagnostics) -> LoweredExpr>,
    pub partially_apply_right: Arc<dyn Fn(ListExpr, &mut Scope, &mut Diagnostics) -> LoweredExpr>,
}

impl Operator {
    fn apply(expr: LoweredExpr) -> Self {
        Self {
            apply: Arc::new(|lhs, rhs, scope, diagnostics| todo!()),
            partially_apply_left: Arc::new(|lhs, scope, diagnostics| todo!("LoweredFillExpr")),
            partially_apply_right: Arc::new(|rhs, scope, diagnostics| todo!("LoweredFillExpr")),
        }
    }
}

impl fmt::Debug for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("OperatorApplyTable").finish()
    }
}
