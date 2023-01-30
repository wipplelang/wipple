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

#[cfg(feature = "serde")]
impl serde::Serialize for InternedString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        (**self).serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for InternedString {
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
        const NUM_CHARS: usize = 'Z' as usize - 'A' as usize;
        const MAX_LEN: usize = 2;

        Ok(Self::new(
            (0..u.int_in_range::<usize>(1..=MAX_LEN)?)
                .map(|_| {
                    Ok(('A'..='Z')
                        .nth(u.int_in_range(0..=(NUM_CHARS - 1))?)
                        .unwrap())
                })
                .collect::<Result<String, _>>()?,
        ))
    }
}
