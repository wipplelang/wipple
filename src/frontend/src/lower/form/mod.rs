mod builtin;
pub use builtin::*;

use crate::{lower::*, typecheck::Ty};
use kind::kind;
use serde::Serialize;
use std::{fmt, sync::Arc};

#[non_exhaustive]
#[derive(Debug, Clone, Serialize)]
pub struct Form {
    pub span: Span,
    pub kind: FormKind,
}

impl Form {
    pub fn new(span: Span, kind: FormKind) -> Self {
        Form { span, kind }
    }
}

#[kind(Form::new(span: Span))]
#[derive(Debug, Clone, Serialize)]
pub enum FormKind {
    Item {
        item: Item,
    },
    Operator {
        precedence: OperatorPrecedence,
        associativity: OperatorAssociativity,
        #[serde(skip)]
        apply: OperatorApply,
    },
    Template {
        // TODO
    },
    Ty {
        ty: Ty,
    },
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

#[derive(Clone)]
pub struct OperatorApply(pub Arc<dyn Fn(ListExpr, ListExpr, &Stack, &mut Info) -> Form>);

impl OperatorApply {
    pub fn new(apply: impl Fn(ListExpr, ListExpr, &Stack, &mut Info) -> Form + 'static) -> Self {
        OperatorApply(Arc::new(apply))
    }
}

impl fmt::Debug for OperatorApply {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("OperatorApply").finish()
    }
}
