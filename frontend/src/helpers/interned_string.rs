use lasso::ThreadedRodeo;
use lazy_static::lazy_static;
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

    pub fn as_str(&self) -> &'static str {
        INTERNER.resolve(&self.symbol)
    }
}

impl Deref for InternedString {
    type Target = str;

    fn deref(&self) -> &'static Self::Target {
        self.as_str()
    }
}

impl AsRef<str> for InternedString {
    fn as_ref(&self) -> &'static str {
        self.as_str()
    }
}

impl From<String> for InternedString {
    fn from(s: String) -> Self {
        InternedString::new(s)
    }
}

impl fmt::Display for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl fmt::Debug for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.as_str())
    }
}
