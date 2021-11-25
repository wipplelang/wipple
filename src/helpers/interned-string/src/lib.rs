use std::fmt;

use lazy_static::lazy_static;
use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard};
use serde::Serialize;
use string_interner::StringInterner;

lazy_static! {
    static ref INTERNER: RwLock<StringInterner> = Default::default();
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InternedString {
    symbol: string_interner::DefaultSymbol,
}

impl InternedString {
    pub fn new(s: impl AsRef<str>) -> Self {
        InternedString {
            symbol: INTERNER.write().get_or_intern(s),
        }
    }

    pub fn get(&self) -> MappedRwLockReadGuard<str> {
        RwLockReadGuard::map(INTERNER.read(), |f| f.resolve(self.symbol).unwrap())
    }
}

impl Serialize for InternedString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.get().serialize(serializer)
    }
}

impl fmt::Display for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get())
    }
}

impl fmt::Debug for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.get())
    }
}
