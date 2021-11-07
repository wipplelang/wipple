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

use crate::{lower::*, typecheck::Ty};
use enum_dispatch::enum_dispatch;
use kind::kind;
use paste::paste;
use std::fmt;
use wipple_parser as parser;

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

macro_rules! lower {
    ($($name:ident { $var:ident: $ty:ty } = $str:literal,)*) => {
        #[kind(Form::new(span: Span))]
        #[derive(Debug, Clone, Serialize)]
        pub enum FormKind {
            $($name { $var: $ty },)*
        }

        impl fmt::Display for FormKind {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(FormKind::$name { .. } => write!(f, $str),)*
                }
            }
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum LowerContext {
            $($name,)*
        }

        paste! {
            #[enum_dispatch]
            pub trait ExprKind
            where
                Self: Sized + fmt::Debug,
                Expr: From<Self>,
            {
                fn span(&self) -> Span;
                fn lower(self, context: LowerContext, stack: &Stack, info: &mut Info) -> Option<Form>;

                $(
                    fn [<lower_to_ $var>](self, stack: &Stack, info: &mut Info) -> Option<$ty> {
                        let span = self.span();

                        match self.lower(LowerContext::$name, stack, info)?.kind {
                            FormKind::$name { $var } => Some($var),
                            kind => {
                                info.diagnostics.add(Diagnostic::new(
                                    DiagnosticLevel::Error,
                                    format!("Expected {}, found {}", $str, kind),
                                    vec![Note::primary(span, format!("Expected {} here", $str))],
                                ));

                                None
                            }
                        }
                    }
                )*
            }
        }
    };
}

lower! {
    Item { item: Item } = "item",
    Operator { operator: Operator } = "operator",
    Template { template: Template } = "template",
    Ty { ty: Ty } = "type",
    File { file: Rc<File> } = "file",
    Binding { binding: Binding } = "binding",
}

#[enum_dispatch(ExprKind)]
#[derive(Debug)]
pub enum Expr {
    Block(BlockExpr),
    List(ListExpr),
    Name(NameExpr),
    Number(NumberExpr),
    Text(TextExpr),
}

impl<'src> From<parser::Expr<'src>> for Expr {
    fn from(expr: parser::Expr<'src>) -> Self {
        match expr.kind {
            parser::ExprKind::Name(value) => Expr::from(NameExpr::new(expr.span, value)),
            parser::ExprKind::Text(value) => Expr::from(TextExpr::new(expr.span, value)),
            parser::ExprKind::Number(value) => Expr::from(NumberExpr::new(expr.span, value)),
            parser::ExprKind::Quote(_) => todo!(),
            parser::ExprKind::List(lines) => {
                Expr::from(ListExpr::new(expr.span, parse_lines(lines)))
            }
            parser::ExprKind::Attribute(_) => todo!(),
            parser::ExprKind::Block(statements) => Expr::from(BlockExpr::new(
                expr.span,
                statements.into_iter().filter_map(parse_statement).collect(),
            )),
        }
    }
}

fn parse_lines(lines: Vec<parser::ListLine>) -> Vec<Expr> {
    lines
        .into_iter()
        .filter_map(|line| (!line.exprs.is_empty()).then(|| line.exprs.into_iter().map(From::from)))
        .flatten()
        .collect()
}

pub(crate) fn parse_statement(statement: parser::Statement) -> Option<Expr> {
    let exprs = parse_lines(statement.lines);
    (!exprs.is_empty()).then(|| Expr::from(ListExpr::infer_span(exprs)))
}
