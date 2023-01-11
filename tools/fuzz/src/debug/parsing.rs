use std::sync::Arc;
use wipple_default_loader::Loader;
use wipple_frontend::{helpers::InternedString, Compiler, FilePath};

pub fn fuzz(source: &str) {
    let path = InternedString::new("fuzz");

    let loader = Loader::new(None, None);
    loader.virtual_paths.lock().insert(path, Arc::from(source));

    let compiler = Compiler::new(&loader).set_backtrace_enabled(true);

    compiler.parse(FilePath::Virtual(path), source);
}
