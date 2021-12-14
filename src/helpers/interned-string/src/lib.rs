use lasso::ThreadedRodeo;
use lazy_static::lazy_static;
use serde::Serialize;
use std::{fmt, ops::Deref};

lazy_static! {
    static ref INTERNER: ThreadedRodeo = Default::default();
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InternedString {
    symbol: lasso::Spur,
}

impl InternedString {
    pub fn new(s: impl AsRef<str>) -> Self {
        InternedString {
            symbol: INTERNER.get_or_intern(s.as_ref()),
        }
    }
}

impl Deref for InternedString {
    type Target = str;

    fn deref(&self) -> &'static Self::Target {
        INTERNER.resolve(&self.symbol)
    }
}

impl AsRef<str> for InternedString {
    fn as_ref(&self) -> &'static str {
        INTERNER.resolve(&self.symbol)
    }
}

impl Serialize for InternedString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        (**self).serialize(serializer)
    }
}

impl fmt::Display for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", *self)
    }
}

impl fmt::Debug for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", *self)
    }
}
