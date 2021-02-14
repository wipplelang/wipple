use crate::fundamentals::*;
use std::collections::HashSet;

#[derive(Clone)]
pub struct Value {
    pub traits: HashSet<AnyTrait>,
}

impl Value {
    pub fn empty() -> Value {
        Value {
            traits: HashSet::new(),
        }
    }
}
