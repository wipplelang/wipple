mod block;
mod list;
mod name;
mod number;
mod text;

pub use block::*;
pub use list::*;
pub use name::*;
pub use number::*;
pub use text::*;

use crate::lower::*;
use enum_dispatch::enum_dispatch;
use std::fmt;
use wipple_parser as parser;

#[enum_dispatch]
pub trait Expr
where
    Self: fmt::Debug + Sized,
    SpannedExpr: From<Self>,
{
    fn span(&self) -> Span;

    fn lower_to_form(self, stack: Stack, diagnostics: &mut Diagnostics) -> SpannedForm {
        self.lower_to_item(stack, diagnostics).into()
    }

    fn lower_to_binding(self, stack: Stack, diagnostics: &mut Diagnostics) -> SpannedBinding;

    // eventually quoted, type, etc.

    fn lower_to_item(self, stack: Stack, diagnostics: &mut Diagnostics) -> SpannedItem {
        let form = self.lower_to_form(stack, diagnostics);

        match form.form {
            Form::Item(item) => SpannedItem::new(form.span, item),
            Form::Operator(_) => {
                diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Expected value, found operator",
                    vec![Note::primary(form.span, "Expected value here")],
                ));

                SpannedItem::error(form.span)
            }
            Form::Template(_) => {
                diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Expected value, found template",
                    vec![Note::primary(form.span, "Expected value here")],
                ));

                SpannedItem::error(form.span)
            }
        }
    }
}

#[enum_dispatch(Expr)]
#[derive(Debug)]
pub enum SpannedExpr {
    Block(BlockExpr),
    List(ListExpr),
    Name(NameExpr),
    Number(NumberExpr),
    Text(TextExpr),
}

impl<'src> From<parser::Expr<'src>> for SpannedExpr {
    fn from(expr: parser::Expr<'src>) -> Self {
        use parser::ExprKind::*;

        fn parse_lines(lines: Vec<parser::ListLine>) -> Vec<SpannedExpr> {
            lines
                .into_iter()
                .filter_map(|line| {
                    (!line.exprs.is_empty()).then(|| line.exprs.into_iter().map(From::from))
                })
                .flatten()
                .collect()
        }

        match expr.kind {
            Name(value) => SpannedExpr::from(NameExpr::new(expr.span, value)),
            Text(value) => SpannedExpr::from(TextExpr::new(expr.span, value)),
            Number(value) => SpannedExpr::from(NumberExpr::new(expr.span, value)),
            Quote(_) => todo!(),
            List(lines) => SpannedExpr::from(ListExpr::new(expr.span, parse_lines(lines))),
            Attribute(_) => todo!(),
            Block(statements) => SpannedExpr::from(BlockExpr::new(
                expr.span,
                statements
                    .into_iter()
                    .filter_map(|statement| {
                        let exprs = parse_lines(statement.lines);
                        (!exprs.is_empty()).then(|| SpannedExpr::from(ListExpr::infer_span(exprs)))
                    })
                    .collect(),
            )),
        }
    }
}
