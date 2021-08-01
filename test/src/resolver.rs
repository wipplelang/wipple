use std::collections::HashMap;
use wipple_bytecode as bytecode;
use wipple_interpreter as interpreter;

#[derive(Default)]
pub struct ClosureResolver {
    pub objects: HashMap<String, Object>,
}

impl ClosureResolver {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with(mut self, name: impl ToString, object: Object) -> Self {
        self.objects.insert(name.to_string(), object);
        self
    }
}

#[derive(Default)]
pub struct Object {
    pub symbols: HashMap<String, interpreter::ExternFn>,
}

impl Object {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with(
        mut self,
        name: impl ToString,
        func: impl Fn(&[bytecode::binary::Index], &mut interpreter::Context) -> interpreter::Result<()>
            + 'static,
    ) -> Self {
        self.symbols.insert(name.to_string(), Box::new(func));
        self
    }
}

impl interpreter::ExternResolver for ClosureResolver {
    fn resolve(&self, r#extern: &bytecode::binary::Extern) -> Option<&interpreter::ExternFn> {
        self.objects
            .get(r#extern.object.as_ref())
            .and_then(|object| object.symbols.get(r#extern.symbol.as_ref()))
    }
}
