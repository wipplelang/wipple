use internment::Intern;
use std::{fmt, ops::Deref};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InternedString {
    symbol: Intern<str>,
}

impl InternedString {
    pub fn new(s: impl AsRef<str>) -> Self {
        InternedString {
            symbol: Intern::from(s.as_ref()),
        }
    }

    pub fn as_str(&self) -> &'static str {
        self.symbol.as_ref()
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

impl serde::Serialize for InternedString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.to_string().serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for InternedString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        String::deserialize(deserializer).map(InternedString::new)
    }
}
