use std::collections::HashMap;
use wipple_bytecode::Instruction;

#[derive(Default)]
pub struct Interpreter {
    pub namespaces: HashMap<String, ExternNamespace>,
}

impl Interpreter {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn namespace(mut self, name: impl ToString, namespace: ExternNamespace) -> Self {
        self.namespaces.insert(name.to_string(), namespace);
        self
    }
}

pub struct ExternFn {
    // TODO
}

#[derive(Default)]
pub struct ExternNamespace {
    pub items: HashMap<String, ExternFn>,
}

impl ExternNamespace {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with(mut self, name: impl ToString, func: ExternFn) -> Self {
        self.items.insert(name.to_string(), func);
        self
    }
}

struct Value {
    // TODO
}

impl Interpreter {
    pub fn execute(&self, instruction: Instruction) {
        todo!()
    }
}
