use crate::builtins::*;
use crate::fundamentals::*;
use std::collections::HashMap;
use uuid::Uuid;

#[derive(Clone, Default)]
pub struct Environment {
    pub variables: HashMap<String, Value>,
    pub conformances: Vec<AnyConformance>,
    pub operator_precedences: Vec<PrecedenceGroup>,
    pub user_defined: HashMap<EnvironmentKey, Value>,
}

#[derive(Clone)]
pub struct EnvironmentKey {
    id: Uuid,
    debug_label: Option<String>,
}

impl EnvironmentKey {
    pub fn new() -> EnvironmentKey {
        EnvironmentKey {
            id: Uuid::new_v4(),
            debug_label: None,
        }
    }

    pub fn labeled(label: &str) -> EnvironmentKey {
        EnvironmentKey {
            id: Uuid::new_v4(),
            debug_label: Some(String::from(label)),
        }
    }
}
