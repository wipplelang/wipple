use crate::*;
use std::collections::HashMap;
use uuid::Uuid;

#[derive(Clone, Default)]
pub struct Environment {
    pub next_trait_id: usize,
    pub variables: HashMap<String, Value>,
    pub conformances: Vec<Conformance>,
    pub operator_precedences: Vec<PrecedenceGroup>,
    pub custom: HashMap<EnvironmentKey, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Environment::default()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct EnvironmentKey(pub Uuid);

impl EnvironmentKey {
    pub fn new() -> Self {
        EnvironmentKey(Uuid::new_v4())
    }
}

impl Default for EnvironmentKey {
    fn default() -> Self {
        Self::new()
    }
}
