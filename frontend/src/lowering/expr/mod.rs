mod block;
mod list;
mod name;
mod number;
mod text;

use std::fmt;

pub use block::*;
pub use list::*;
pub use name::*;
pub use number::*;
pub use text::*;

use crate::{
    diagnostics::Diagnostics,
    lowering::*,
    parser::{self, Span},
};
use enum_dispatch::enum_dispatch;

#[enum_dispatch]
pub trait Expr
where
    Self: fmt::Debug + Sized,
    AnyExpr: From<Self>,
{
    fn span(&self) -> Span;

    fn lower(self, scope: &mut Scope, diagnostics: &mut Diagnostics) -> LoweredExpr;

    fn operator(&self, _: &Scope, _: &mut Diagnostics) -> Option<LoweredOperatorExpr> {
        None
    }

    fn binding(self) -> Option<Binding> {
        None
    }

    // eventually quoted, type, etc.
}

#[enum_dispatch(Expr)]
#[derive(Debug)]
pub enum AnyExpr {
    BlockExpr,
    ListExpr,
    NameExpr,
    NumberExpr,
    TextExpr,
}

impl From<parser::Expr> for AnyExpr {
    fn from(expr: parser::Expr) -> Self {
        use parser::ExprKind::*;

        match expr.kind {
            Name(value) => AnyExpr::from(NameExpr::new(expr.span, value)),
            Text(value) => AnyExpr::from(TextExpr::new(expr.span, value)),
            Number(value) => AnyExpr::from(NumberExpr::new(expr.span, value)),
            Quote(_) => todo!(),
            List(exprs) => AnyExpr::from(ListExpr::new(
                expr.span,
                exprs.into_iter().map(From::from).collect(),
            )),
            Attribute(_) => todo!(),
            Block(exprs) => AnyExpr::from(BlockExpr::new(
                expr.span,
                exprs.into_iter().map(From::from).collect(),
            )),
        }
    }
}
