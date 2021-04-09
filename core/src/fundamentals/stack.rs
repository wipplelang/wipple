use std::collections::HashMap;

use crate::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StackKey(Id);

impl StackKey {
    pub fn of<T: 'static>() -> Self {
        StackKey(Id::of::<T>())
    }

    pub fn new() -> Self {
        StackKey(Id::new())
    }
}

pub type StackValues = HashMap<StackKey, Dynamic>;

#[derive(Debug, Clone, Default)]
pub struct Stack {
    pub values: StackValues,
}

impl Stack {
    pub fn new() -> Self {
        Stack::default()
    }

    pub fn try_get(&self, key: StackKey) -> Option<&Dynamic> {
        self.values.get(&key)
    }

    pub fn get(&self, key: StackKey, default: Dynamic) -> Dynamic {
        self.values.get(&key).cloned().unwrap_or(default)
    }

    pub fn get_mut(&mut self, key: StackKey, default: Dynamic) -> &mut Dynamic {
        self.values.entry(key).or_insert(default)
    }
}
