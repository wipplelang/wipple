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
use wipple_parser as parser;

#[enum_dispatch]
pub trait ExprKind
where
    Self: Sized,
    Expr: From<Self>,
{
    fn span(&self) -> Span;

    fn lower_to_form(self, stack: &Stack, info: &mut Info) -> Form;

    fn lower_to_binding(self, _: &Stack, _: &mut Info) -> Option<SpannedBinding> {
        None
    }

    // eventually quoted, type, etc.

    fn lower_to_item(self, stack: &Stack, info: &mut Info) -> Item {
        let form = self.lower_to_form(stack, info);

        match form {
            Form::Item(item) => item,
            Form::Operator(operator) => {
                info.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Expected value, found operator",
                    vec![Note::primary(operator.span, "Expected value here")],
                ));

                Item::error(operator.span)
            }
            Form::Template(template) => {
                info.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Expected value, found template",
                    vec![Note::primary(template.span, "Expected value here")],
                ));

                Item::error(template.span)
            }
        }
    }
}

#[enum_dispatch(ExprKind)]
pub enum Expr {
    Block(BlockExpr),
    List(ListExpr),
    Name(NameExpr),
    Number(NumberExpr),
    Text(TextExpr),
}

impl<'src> From<parser::Expr<'src>> for Expr {
    fn from(expr: parser::Expr<'src>) -> Self {
        use parser::ExprKind::*;

        fn parse_lines(lines: Vec<parser::ListLine>) -> Vec<Expr> {
            lines
                .into_iter()
                .filter_map(|line| {
                    (!line.exprs.is_empty()).then(|| line.exprs.into_iter().map(From::from))
                })
                .flatten()
                .collect()
        }

        match expr.kind {
            Name(value) => Expr::from(NameExpr::new(expr.span, value)),
            Text(value) => Expr::from(TextExpr::new(expr.span, value)),
            Number(value) => Expr::from(NumberExpr::new(expr.span, value)),
            Quote(_) => todo!(),
            List(lines) => Expr::from(ListExpr::new(expr.span, parse_lines(lines))),
            Attribute(_) => todo!(),
            Block(statements) => Expr::from(BlockExpr::new(
                expr.span,
                statements
                    .into_iter()
                    .filter_map(|statement| {
                        let exprs = parse_lines(statement.lines);
                        (!exprs.is_empty()).then(|| Expr::from(ListExpr::infer_span(exprs)))
                    })
                    .collect(),
            )),
        }
    }
}
