use std::sync::Arc;
use wipple_default_loader::Loader;
use wipple_frontend::{analysis, diagnostics::FinalizedDiagnostics, Compiler, Loader as _};

#[tokio::main]
pub(super) async fn fuzz(file: analysis::lower::File, args: super::Args) -> FinalizedDiagnostics {
    let loader = Loader::new(None, None);
    loader
        .source_map()
        .lock()
        .insert(file.span.path, Arc::from(""));

    let compiler = Compiler::new(loader);
    #[cfg(debug_assertions)]
    let compiler = compiler.set_backtrace_enabled(true);

    let options = analysis::Options::default();
    let program = compiler.typecheck_with(file, false, &options);

    if !args.quiet {
        clearscreen::clear().ok();
        print!("Program:\n{}", program);
    }

    compiler.finish_analysis()
}
