use std::sync::Arc;
use wipple_default_loader::Loader;
use wipple_frontend::{analysis, diagnostics::FinalizedDiagnostics, Compiler, Loader as _};

#[tokio::main]
pub async fn fuzz(file: analysis::lower::File, quiet: bool) -> FinalizedDiagnostics {
    if !quiet {
        println!("File: {:#?}", file);
    }

    let loader = Loader::new(None, None);
    loader
        .source_map()
        .lock()
        .insert(file.span.path, Arc::from(""));

    let compiler = Compiler::new(&loader).set_backtrace_enabled(true);

    let options = analysis::Options::default();
    compiler.typecheck_with(file, false, &options);

    compiler.finish_analysis()
}
