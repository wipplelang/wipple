use std::collections::HashMap;
use wipple_bytecode as bytecode;

#[derive(Default)]
pub struct Resolver {
    pub objects: HashMap<String, Object>,
}

impl Resolver {
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
    pub symbols: HashMap<String, crate::ExternFn>,
}

impl Object {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with(
        mut self,
        name: impl ToString,
        func: impl Fn(&[bytecode::binary::Index], &mut crate::Context) -> crate::Result<()> + 'static,
    ) -> Self {
        self.symbols.insert(name.to_string(), Box::new(func));
        self
    }
}

impl crate::ExternResolver for Resolver {
    fn resolve(&self, r#extern: &bytecode::binary::Extern) -> Option<&crate::ExternFn> {
        self.objects
            .get(r#extern.object.as_ref())
            .and_then(|object| object.symbols.get(r#extern.symbol.as_ref()))
    }
}
