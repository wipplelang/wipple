use std::collections::HashMap;

use crate::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StackKey(ID);

impl StackKey {
    pub fn of<T: 'static>() -> Self {
        StackKey(ID::of::<T>())
    }

    pub fn new() -> Self {
        StackKey(ID::new())
    }
}

pub type StackValues = HashMap<StackKey, Dynamic>;

#[derive(Debug, Clone)]
pub struct Stack {
    pub values: StackValues,
}

impl Stack {
    pub fn empty() -> Self {
        Stack {
            values: StackValues::new(),
        }
    }

    pub fn get(&self, key: StackKey) -> Option<&Dynamic> {
        self.values.get(&key)
    }

    pub fn get_or(self, key: StackKey, default: Dynamic) -> Dynamic {
        self.values.get(&key).cloned().unwrap_or(default)
    }

    pub fn with(mut self, key: StackKey, value: Dynamic) -> Self {
        self.values.insert(key, value);
        self
    }
}
