use lasso::ThreadedRodeo;
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
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

impl Serialize for InternedString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        (**self).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for InternedString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Self::new(String::deserialize(deserializer)?))
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

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for InternedString {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Self::new(String::arbitrary(u)?))
    }
}
