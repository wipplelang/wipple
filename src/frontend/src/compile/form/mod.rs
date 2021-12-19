mod builtin;
mod constructor;
mod operator;
mod template;

pub use builtin::*;
pub use constructor::*;
pub use operator::*;
pub use template::*;

use crate::{compile::*, *};
use paste::paste;
use serde::Serialize;
use std::fmt;

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

macro_rules! forms {
    ($($name:ident($($ty:tt)*) = $str:literal,)*) => {
        #[derive(Debug, Clone, Serialize)]
        pub enum FormKind {
            $($name($($ty)*),)*
        }

        paste! {
            impl Form {
                $(
                    pub fn [<$name:snake:lower>](span: Span, x: $($ty)*) -> Self {
                        Form::new(span, FormKind::$name(x))
                    }
                )*
            }
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
            $(
                pub trait [<LowerTo $name Ext>]
                where
                    Self: ExprKind,
                    Expr: From<Self>,
                {
                    fn [<lower_to_ $name:snake:lower>](self, stack: &Stack, info: &mut Info) -> Option<$($ty)*>;
                }

                impl<T: ExprKind> [<LowerTo $name Ext>] for T
                where
                    Expr: From<T>,
                {
                    fn [<lower_to_ $name:snake:lower>](self, stack: &Stack, info: &mut Info) -> Option<$($ty)*> {
                        let span = self.span();

                        match self.lower(LowerContext::$name, stack, info)?.kind {
                            FormKind::$name(x) => Some(x),
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
                }
            )*
        }
    };
}

forms! {
    Item(Item) = "item",
    Operator(Operator) = "operator",
    Template(Template) = "template",
    File(Arc<File>) = "file",
    Binding(Binding) = "binding",
    Constructor(Constructor) = "type",
    DataStructFieldDecl(DataStructFieldDecl) = "data structure field declaration",
    DataStructField(DataStructField) = "data structure field",
}

impl Form {
    pub fn as_decl_item(&self) -> Option<Item> {
        match &self.kind {
            FormKind::Constructor(Constructor::DataStruct { id, fields }) => Some(Item::data_decl(
                self.span,
                *id,
                fields
                    .values()
                    .map(|field| (field.info.name, field.constructor.clone()))
                    .collect(),
            )),
            _ => None,
        }
    }
}
