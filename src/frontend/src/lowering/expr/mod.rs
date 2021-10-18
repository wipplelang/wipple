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

use crate::lowering::*;
use enum_dispatch::enum_dispatch;
use wipple_diagnostics::*;
use wipple_parser as parser;

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

impl<'src> From<parser::Expr<'src>> for AnyExpr {
    fn from(expr: parser::Expr<'src>) -> Self {
        use parser::ExprKind::*;

        fn parse_lines(lines: Vec<parser::ListLine>) -> Vec<AnyExpr> {
            lines
                .into_iter()
                .filter_map(|line| {
                    (!line.exprs.is_empty()).then(|| line.exprs.into_iter().map(From::from))
                })
                .flatten()
                .collect()
        }

        match expr.kind {
            Name(value) => AnyExpr::from(NameExpr::new(expr.span, value)),
            Text(value) => AnyExpr::from(TextExpr::new(expr.span, value)),
            Number(value) => AnyExpr::from(NumberExpr::new(expr.span, value)),
            Quote(_) => todo!(),
            List(lines) => AnyExpr::from(ListExpr::new(expr.span, parse_lines(lines))),
            Attribute(_) => todo!(),
            Block(statements) => AnyExpr::from(BlockExpr::new(
                expr.span,
                statements
                    .into_iter()
                    .filter_map(|statement| {
                        let exprs = parse_lines(statement.lines);
                        (!exprs.is_empty()).then(|| AnyExpr::from(ListExpr::infer_span(exprs)))
                    })
                    .collect(),
            )),
        }
    }
}
