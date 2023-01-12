use std::sync::Arc;
use wipple_default_loader::Loader;
use wipple_frontend::{
    analysis,
    diagnostics::FinalizedDiagnostics,
    helpers::{IndexMap, InternedString},
    Compiler, Loader as _,
};

#[tokio::main]
pub async fn fuzz(mut file: analysis::expand::File, quiet: bool) -> FinalizedDiagnostics {
    file.attributes.no_std = true; // test only this file (FIXME: Move to Arbitrary implementation)

    if !quiet {
        println!("File: {:#?}", file);
    }

    let loader = Loader::new(None, None);
    loader
        .virtual_paths
        .lock()
        .insert(InternedString::new(file.path.as_str()), Arc::from(""));

    loader.source_map().lock().insert(file.path, Arc::from(""));

    let compiler = Compiler::new(&loader).set_backtrace_enabled(true);

    let files = IndexMap::from([(file.span.path, Arc::new(file))]);
    let options = analysis::Options::default();

    let (entrypoint, complete) = compiler.lower_with(files, &options);
    let program = compiler.typecheck_with(entrypoint, complete, &options);
    compiler.lint_with(&program, &options);

    if compiler.has_errors() {
        return compiler.finish_analysis();
    }

    compiler.ir_from(&program);

    compiler.finish_analysis()
}
