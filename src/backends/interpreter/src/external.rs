use crate::{Error, Value};
use serde::Serialize;
use std::{collections::HashMap, fmt, sync::Arc};

#[derive(Default)]
pub struct ExternalValues(HashMap<String, HashMap<String, Arc<Value>>>);

impl ExternalValues {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get(&self, namespace: &str, identifier: &str) -> Result<&Arc<Value>, Error> {
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
            .insert(identifier.to_string(), Arc::new(value));

        self
    }
}

#[allow(clippy::type_complexity)]
#[derive(Clone, Serialize)]
pub struct ExternalFunction {
    #[serde(skip)]
    function: Arc<dyn Fn(Arc<Value>) -> Result<Arc<Value>, Error>>,
}

impl ExternalFunction {
    pub fn new(func: impl for<'a> Fn(Arc<Value>) -> Result<Arc<Value>, Error> + 'static) -> Self {
        ExternalFunction {
            function: Arc::new(func),
        }
    }

    pub fn call(&self, input: Arc<Value>) -> Result<Arc<Value>, Error> {
        (self.function)(input)
    }
}

impl fmt::Debug for ExternalFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("ExternalFunction").finish()
    }
}
