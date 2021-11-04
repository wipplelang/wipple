mod name;

pub use name::*;

use crate::lower::*;
use enum_dispatch::enum_dispatch;

#[enum_dispatch]
pub trait Binding
where
    Self: Sized,
    SpannedBinding: From<Self>,
{
    fn span(&self) -> Span;
    fn assign_in_block(self, span: Span, form: Form, stack: &Stack, info: &mut Info) -> Item;
}

#[enum_dispatch(Binding)]
pub enum SpannedBinding {
    Name(NameBinding),
    // eventually, destructuring and conversions
}
