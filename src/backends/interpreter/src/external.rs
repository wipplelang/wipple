use crate::{Error, Value};
use serde::Serialize;
use std::{collections::HashMap, fmt, rc::Rc};

#[derive(Default)]
pub struct ExternalValues(HashMap<String, HashMap<String, Rc<Value>>>);

impl ExternalValues {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get(&self, namespace: &str, identifier: &str) -> Result<&Rc<Value>, Error> {
        self.0
            .get(namespace)
            .ok_or_else(|| Error::UnknownExternalNamespace(namespace.to_string()))?
            .get(identifier)
            .ok_or_else(|| Error::UnknownExternalIdentifier(identifier.to_string()))
    }

    pub fn insert(
        mut self,
        namespace: impl ToString,
        identifier: impl ToString,
        value: Value,
    ) -> Self {
        self.0
            .entry(namespace.to_string())
            .or_default()
            .insert(identifier.to_string(), Rc::new(value));

        self
    }
}

#[allow(clippy::type_complexity)]
#[derive(Clone, Serialize)]
pub struct ExternalFunction {
    #[serde(skip)]
    function: Rc<dyn Fn(Rc<Value>) -> Result<Rc<Value>, Error>>,
}

impl ExternalFunction {
    pub fn new(func: impl for<'a> Fn(Rc<Value>) -> Result<Rc<Value>, Error> + 'static) -> Self {
        ExternalFunction {
            function: Rc::new(func),
        }
    }

    pub fn call(&self, input: Rc<Value>) -> Result<Rc<Value>, Error> {
        (self.function)(input)
    }
}

impl fmt::Debug for ExternalFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("ExternalFunction").finish()
    }
}
