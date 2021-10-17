use internment::Intern;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct LoweredRuntimeVariableExpr {
    pub ancestor: usize,
    pub name: Intern<String>,
}

impl LoweredRuntimeVariableExpr {
    pub fn in_current_scope(name: Intern<String>) -> Self {
        LoweredRuntimeVariableExpr { ancestor: 0, name }
    }

    pub fn in_ancestor_scope(mut self) -> Self {
        self.ancestor += 1;
        self
    }
}
