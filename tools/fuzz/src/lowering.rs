use arbitrary::{Arbitrary, Unstructured};
use std::sync::Arc;
use wipple_default_loader::Loader;
use wipple_frontend::{
    analysis,
    helpers::{IndexMap, InternedString},
    Compiler,
};

#[tokio::main]
pub async fn fuzz() {
    let loader = Loader::new(None, None);
    loader
        .virtual_paths
        .lock()
        .insert(InternedString::new("fuzz"), Arc::from(""));

    let compiler = Compiler::new(&loader);

    let data = std::iter::repeat_with(rand::random)
        .take(1024)
        .collect::<Vec<u8>>();

    if let Ok(file) = analysis::expand::File::arbitrary(&mut Unstructured::new(&data)) {
        let files = IndexMap::from([(file.span.path, Arc::new(file))]);
        compiler.lower_with(files, &Default::default());
    }
}
