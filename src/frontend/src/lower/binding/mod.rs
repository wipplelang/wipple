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
    fn assign(self, span: Span, form: SpannedForm, stack: Stack, info: &mut Info) -> SpannedItem;
}

#[enum_dispatch(Binding)]
pub enum SpannedBinding {
    Name(NameBinding),
    // eventually, destructuring and conversions
}
