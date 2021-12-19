mod name;

pub use name::*;

use crate::{compile::*, *};
use enum_dispatch::enum_dispatch;
use serde::Serialize;

#[enum_dispatch]
pub trait BindingKind
where
    Self: Sized,
    Binding: From<Self>,
{
    fn span(&self) -> Span;
    fn assign(self, span: Span, form: Form, stack: &Stack, info: &mut Info) -> Item;
}

#[enum_dispatch(BindingKind)]
#[derive(Debug, Clone, Serialize)]
pub enum Binding {
    Name(NameBinding),
    // eventually, destructuring and conversions
}
