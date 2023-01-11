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

    let mut file = match analysis::expand::File::arbitrary(&mut Unstructured::new(&data)) {
        Ok(file) => file,
        Err(_) => return,
    };

    file.attributes.no_std = true; // test only this file
    dbg!(&file);

    let files = IndexMap::from([(file.span.path, Arc::new(file))]);
    let options = analysis::Options::default();

    let (entrypoint, complete) = compiler.lower_with(files, &options);
    if compiler.has_errors() {
        return;
    }

    let program = compiler.typecheck_with(entrypoint, complete, &options);
    if compiler.has_errors() {
        return;
    }

    compiler.lint_with(&program, &options);
    if compiler.has_errors() {
        return;
    }

    compiler.ir_from(&program);
}
