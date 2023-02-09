use arbitrary::{Arbitrary, Unstructured};
use std::sync::Arc;
use wipple_default_loader::Loader;
use wipple_frontend::{
    diagnostics::FinalizedDiagnostics, helpers::InternedString, Compiler, FilePath, Loader as _,
};

pub(super) fn fuzz(source: String, args: super::Args) -> FinalizedDiagnostics {
    if !args.quiet {
        println!("Source:\n{}", source);
    }

    let path = FilePath::arbitrary(&mut Unstructured::new(&[])).unwrap();

    let loader = Loader::new(None, None);
    loader.virtual_paths().lock().insert(
        InternedString::new(path.as_str()),
        Arc::from(source.as_str()),
    );

    loader
        .source_map()
        .lock()
        .insert(path, Arc::from(source.as_str()));

    let compiler = Compiler::new(loader);
    #[cfg(debug_assertions)]
    let compiler = compiler.set_backtrace_enabled(true);

    compiler.parse(path, &source);

    compiler.finish_analysis()
}
