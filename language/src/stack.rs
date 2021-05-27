use crate::*;
use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct Stack(HashMap<StackKey, Any>);

impl Stack {
    pub fn get<T: TypeInfo + Default>(&self, key: StackKey) -> Cow<T> {
        self.0
            .get(&key)
            .map(|x| Cow::Borrowed(x.cast()))
            .unwrap_or_else(|| Cow::Owned(T::default()))
    }

    pub fn get_mut<T: TypeInfo + Default>(&mut self, key: StackKey) -> &mut T {
        self.0
            .entry(key)
            .or_insert_with(|| T::default().into())
            .cast_mut()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StackKey(Id);

impl StackKey {
    #![allow(clippy::new_without_default)]
    pub fn new() -> Self {
        StackKey(Id::new())
    }
}
