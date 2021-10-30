use crate::Value;
use std::{collections::HashMap, rc::Rc};

#[derive(Default)]
pub struct ExternalFunctions(HashMap<String, HashMap<String, ExternalFunction>>);

impl ExternalFunctions {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get(&self, namespace: &str, identifier: &str) -> &ExternalFunction {
        self.0.get(namespace).unwrap().get(identifier).unwrap()
    }

    pub fn insert(
        mut self,
        namespace: impl ToString,
        identifier: impl ToString,
        func: ExternalFunction,
    ) -> Self {
        self.0
            .entry(namespace.to_string())
            .or_default()
            .insert(identifier.to_string(), func);

        self
    }
}

#[allow(clippy::type_complexity)]
pub struct ExternalFunction(Box<dyn Fn(Vec<Rc<Value>>) -> Rc<Value>>);

impl ExternalFunction {
    pub fn new(func: impl for<'a> Fn(Vec<Rc<Value>>) -> Rc<Value> + 'static) -> Self {
        ExternalFunction(Box::new(func))
    }

    pub fn call(&self, inputs: Vec<Rc<Value>>) -> Rc<Value> {
        (self.0)(inputs)
    }
}
