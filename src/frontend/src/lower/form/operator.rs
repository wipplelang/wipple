use crate::lower::*;
use serde::Serialize;
use std::sync::Arc;

#[derive(Clone, Serialize)]
pub struct OperatorForm {
    pub precedence: OperatorPrecedence,
    pub associativity: OperatorAssociativity,
    #[serde(skip)]
    pub apply: Arc<dyn Fn(ListExpr, ListExpr, Stack, &mut Info) -> SpannedForm>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct OperatorPrecedence(u8);

impl OperatorPrecedence {
    pub fn new(value: u8) -> Self {
        debug_assert!(matches!(value, 0..=9));

        OperatorPrecedence(value)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Serialize)]
pub enum OperatorAssociativity {
    Left,
    Right,
    None,
}

impl SpannedForm {
    pub fn operator(
        span: Span,
        precedence: OperatorPrecedence,
        associativity: OperatorAssociativity,
        apply: impl Fn(ListExpr, ListExpr, Stack, &mut Info) -> SpannedForm + 'static,
    ) -> Self {
        SpannedForm::new(
            span,
            Form::Operator(OperatorForm {
                precedence,
                associativity,
                apply: Arc::new(apply),
            }),
        )
    }
}
