use arbitrary::{Arbitrary, Unstructured};
use std::sync::Arc;
use wipple_default_loader::Loader;
use wipple_frontend::{
    diagnostics::FinalizedDiagnostics, helpers::InternedString, Compiler, FilePath, Loader as _,
};

pub fn fuzz(source: String, quiet: bool) -> FinalizedDiagnostics {
    if !quiet {
        println!("Source:\n{}", source);
    }

    let path = FilePath::arbitrary(&mut Unstructured::new(&[])).unwrap();

    let loader = Loader::new(None, None);
    loader.virtual_paths.lock().insert(
        InternedString::new(path.as_str()),
        Arc::from(source.as_str()),
    );

    loader
        .source_map()
        .lock()
        .insert(path, Arc::from(source.as_str()));

    let compiler = Compiler::new(&loader).set_backtrace_enabled(true);

    compiler.parse(path, &source);

    compiler.finish_analysis()
}
