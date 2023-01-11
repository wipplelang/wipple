use std::sync::Arc;
use wipple_default_loader::Loader;
use wipple_frontend::{
    analysis,
    helpers::{IndexMap, InternedString},
    Compiler,
};

#[tokio::main]
pub async fn fuzz(file: &analysis::expand::File) {
    let mut file = file.clone();

    let loader = Loader::new(None, None);
    loader
        .virtual_paths
        .lock()
        .insert(InternedString::new("fuzz"), Arc::from(""));

    let compiler = Compiler::new(&loader).set_backtrace_enabled(true);

    file.attributes.no_std = true; // test only this file

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
