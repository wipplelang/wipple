use clap::{Parser, ValueEnum};
use serde::Serialize;
use std::{
    fs,
    io::{self, Read},
    path::PathBuf,
    process::ExitCode,
    str::FromStr,
    sync::Arc,
};
use wipple_default_loader as loader;
use wipple_frontend::{Compiler, Loader};

#[derive(Parser)]
#[clap(
    name = "Wipple",
    bin_name = "wipple",
    about = "The Wipple programming language"
)]
enum Args {
    #[clap(trailing_var_arg = true)]
    Run {
        #[clap(flatten)]
        options: BuildOptions,

        #[clap(long, value_enum, default_value = "interpreter")]
        backend: RunBackend,

        #[clap(multiple_values = true)]
        backend_args: Vec<std::ffi::OsString>,
    },
    #[clap(trailing_var_arg = true)]
    Compile {
        #[clap(flatten)]
        options: BuildOptions,

        #[clap(short)]
        output: PathBuf,

        #[clap(long, value_enum, /* default_value = ".." */)]
        backend: CompileBackend,

        #[clap(multiple_values = true)]
        backend_args: Vec<std::ffi::OsString>,
    },
    Doc {
        #[clap(flatten)]
        options: BuildOptions,

        #[clap(long)]
        full: bool,
    },
    Cache {
        #[clap(long)]
        clear: bool,
    },
    Dump {
        #[clap(flatten)]
        options: BuildOptions,

        #[clap(value_enum)]
        target: DumpTarget,
    },
}

#[derive(Parser)]
struct BuildOptions {
    path: String,

    #[clap(long)]
    std: Option<String>,

    #[clap(long)]
    base_path: Option<PathBuf>,

    #[cfg(debug_assertions)]
    #[clap(long)]
    trace: bool,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum RunBackend {
    Interpreter,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum CompileBackend {}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum DumpTarget {
    Analysis,
    Ir,
}

#[tokio::main]
async fn main() -> ExitCode {
    match run().await {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            if !error.to_string().is_empty() {
                eprintln!("{error}");
            }

            ExitCode::FAILURE
        }
    }
}

#[cfg(not(debug_assertions))]
const PROGRESS_BAR_TICK_SPEED: u64 = 80;

async fn run() -> anyhow::Result<()> {
    let args = Args::parse();

    #[cfg(debug_assertions)]
    let progress_bar = None::<Arc<indicatif::ProgressBar>>;

    #[cfg(not(debug_assertions))]
    let progress_bar = Some(Arc::new({
        let progress_bar = indicatif::ProgressBar::new(0).with_style(
            indicatif::ProgressStyle::default_spinner()
                .tick_strings(&["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]),
        );

        progress_bar.enable_steady_tick(PROGRESS_BAR_TICK_SPEED);

        progress_bar
    }));

    let emit_diagnostics = |diagnostics: wipple_frontend::diagnostics::FinalizedDiagnostics<_>,
                            options: &BuildOptions| {
        #[cfg(not(debug_assertions))]
        let _ = options;

        let (codemap, diagnostics) = diagnostics.into_console_friendly(
            #[cfg(debug_assertions)]
            options.trace,
        );

        if !diagnostics.is_empty() {
            let mut emitter = codemap_diagnostic::Emitter::stderr(
                codemap_diagnostic::ColorConfig::Auto,
                Some(&codemap),
            );

            emitter.emit(&diagnostics);
        }
    };

    match args {
        Args::Run {
            options,
            backend,
            backend_args: _,
        } => {
            let (ir, diagnostics) = generate_ir(&options, progress_bar.clone()).await;

            let error = diagnostics.contains_errors();
            emit_diagnostics(diagnostics, &options);
            if error {
                if let Some(progress_bar) = progress_bar.as_ref() {
                    progress_bar.finish_and_clear();
                }

                return Err(anyhow::Error::msg(""));
            }

            let ir = ir.unwrap();

            match backend {
                RunBackend::Interpreter => {
                    if let Some(progress_bar) = progress_bar.as_ref() {
                        progress_bar.finish_and_clear();
                    }

                    let interpreter =
                        wipple_interpreter_backend::Interpreter::handling_output(|text| {
                            print!("{}", text);
                        });

                    if let Err(error) = interpreter.run(&ir) {
                        eprintln!("fatal error: {}", error);
                    }
                }
            }
        }
        Args::Compile {
            options: _,
            output: _,
            backend: _,
            backend_args: _,
        } => {
            unimplemented!();
        }
        Args::Dump { options, target } => match target {
            DumpTarget::Analysis => {
                let (program, _, diagnostics) = analyze(&options, progress_bar.clone()).await;

                if let Some(progress_bar) = progress_bar.as_ref() {
                    progress_bar.finish_and_clear();
                }

                #[derive(Serialize)]
                struct Output {
                    program: Option<wipple_frontend::analysis::Program>,
                    diagnostics: Vec<wipple_frontend::diagnostics::Diagnostic>,
                }

                let error = diagnostics.contains_errors();

                let output = Output {
                    program,
                    diagnostics: diagnostics.diagnostics,
                };

                serde_json::to_writer(io::stdout(), &output).unwrap();

                if error {
                    return Err(anyhow::Error::msg(""));
                }
            }
            DumpTarget::Ir => {
                let (ir, diagnostics) = generate_ir(&options, progress_bar.clone()).await;

                if let Some(progress_bar) = progress_bar.as_ref() {
                    progress_bar.finish_and_clear();
                }

                let error = diagnostics.contains_errors();
                emit_diagnostics(diagnostics, &options);
                if error {
                    return Err(anyhow::Error::msg(""));
                }

                if let Some(ir) = ir {
                    print!("{}", ir);
                }
            }
        },
        Args::Doc { options, full } => {
            let root = match PathBuf::from_str(&options.path) {
                Ok(path) => path,
                Err(_) => {
                    return Err(anyhow::Error::msg(
                        "`doc` may only be used with local paths, unless `full` is set",
                    ));
                }
            };

            let (program, source_map, diagnostics) = analyze(&options, progress_bar.clone()).await;

            if let Some(progress_bar) = progress_bar.as_ref() {
                progress_bar.finish_and_clear();
            }

            let error = diagnostics.contains_errors();
            emit_diagnostics(diagnostics, &options);
            if error {
                return Err(anyhow::Error::msg(""));
            }

            if let Some(progress_bar) = progress_bar.as_ref() {
                progress_bar.set_message("Generating documentation");
            }

            let doc = if full {
                wipple_frontend::doc::Documentation::new(program.unwrap(), source_map, &root)
            } else {
                wipple_frontend::doc::Documentation::with_filter(
                    program.unwrap(),
                    source_map,
                    &root,
                    |path| {
                        let path = match path {
                            wipple_frontend::FilePath::Path(path) => path,
                            _ => return false,
                        };

                        let path = match PathBuf::from_str(&path) {
                            Ok(path) => path,
                            Err(_) => return false,
                        };

                        root.ends_with(path)
                    },
                )
            };

            if let Some(progress_bar) = progress_bar.as_ref() {
                progress_bar.finish_and_clear();
            }

            serde_json::to_writer(io::stdout(), &doc).unwrap();
        }
        Args::Cache { clear } => {
            let cache_dir = match loader::Fetcher::cache_dir() {
                Some(dir) => dir,
                None => return Err(anyhow::Error::msg("cache not supported on this platform")),
            };

            if clear {
                if let Err(error) = fs::remove_dir_all(cache_dir) {
                    match error.kind() {
                        io::ErrorKind::NotFound => {}
                        _ => return Err(error.into()),
                    }
                };

                if let Some(progress_bar) = progress_bar.as_ref() {
                    progress_bar.finish_and_clear();
                }

                eprintln!("cache cleared successfully");
            } else {
                if let Some(progress_bar) = progress_bar.as_ref() {
                    progress_bar.finish_and_clear();
                }

                println!("{}", cache_dir.to_string_lossy());
            }
        }
    }

    Ok(())
}

async fn analyze(
    options: &BuildOptions,
    progress_bar: Option<Arc<indicatif::ProgressBar>>,
) -> (
    Option<wipple_frontend::analysis::Program>,
    wipple_frontend::SourceMap,
    wipple_frontend::diagnostics::FinalizedDiagnostics<loader::Loader>,
) {
    build_with_passes(options, progress_bar, |_, _, program| program).await
}

async fn generate_ir(
    options: &BuildOptions,
    progress_bar: Option<Arc<indicatif::ProgressBar>>,
) -> (
    Option<wipple_frontend::ir::Program>,
    wipple_frontend::diagnostics::FinalizedDiagnostics<loader::Loader>,
) {
    let (ir, _, diagnostics) =
        build_with_passes(options, progress_bar, |progress_bar, compiler, program| {
            if let Some(progress_bar) = progress_bar {
                progress_bar.set_message("Generating IR");
            }

            compiler.ir_from(&program)
        })
        .await;

    (ir, diagnostics)
}

async fn build_with_passes<P>(
    options: &BuildOptions,
    progress_bar: Option<Arc<indicatif::ProgressBar>>,
    passes: impl FnOnce(
        Option<&indicatif::ProgressBar>,
        &mut Compiler<loader::Loader>,
        wipple_frontend::analysis::Program,
    ) -> P,
) -> (
    Option<P>,
    wipple_frontend::SourceMap,
    wipple_frontend::diagnostics::FinalizedDiagnostics<loader::Loader>,
) {
    let analysis_progress = {
        let progress_bar = progress_bar.clone();

        move |progress| {
            use wipple_frontend::analysis;

            if let Some(progress_bar) = progress_bar.as_ref() {
                match progress {
                    analysis::Progress::Resolving { path, count } => {
                        progress_bar.set_message(format!("({count} files) Resolving {}", path))
                    }
                    analysis::Progress::Lowering {
                        path,
                        current,
                        total,
                    } => progress_bar.set_message(format!("({current}/{total}) Lowering {path}")),
                    analysis::Progress::Typechecking(progress) => match progress {
                        analysis::typecheck::Progress::Typechecking {
                            path,
                            current,
                            total,
                        } => progress_bar
                            .set_message(format!("({current}/{total}) Typechecking {path}")),
                        analysis::typecheck::Progress::Finalizing => {
                            progress_bar.set_message("Finalizing types")
                        }
                    },
                }
            }
        }
    };

    let base = options
        .base_path
        .clone()
        .unwrap_or_else(|| PathBuf::from(&options.path).parent().unwrap().to_path_buf())
        .to_string_lossy()
        .to_string();

    let loader = loader::Loader::new(
        Some(wipple_frontend::FilePath::Path(
            wipple_frontend::helpers::InternedString::new(base),
        )),
        Some({
            let path = options.std.as_deref();

            wipple_frontend::FilePath::Path(
                #[cfg(debug_assertions)]
                wipple_frontend::helpers::InternedString::new(
                    path.unwrap_or(concat!(env!("CARGO_WORKSPACE_DIR"), "pkg/std/std.wpl")),
                ),
                #[cfg(not(debug_assertions))]
                {
                    let path = path.unwrap_or(loader::STD_URL);

                    if loader::is_url(path) {
                        wipple_frontend::helpers::InternedString::new(path)
                    } else {
                        wipple_frontend::helpers::InternedString::new(
                            PathBuf::from(path)
                                .canonicalize()
                                .unwrap()
                                .to_str()
                                .unwrap(),
                        )
                    }
                },
            )
        }),
    );

    if options.path == "-" {
        let mut stdin = String::new();
        io::stdin().read_to_string(&mut stdin).unwrap();

        loader.virtual_paths.lock().insert(
            wipple_frontend::helpers::InternedString::new("stdin"),
            Arc::from(stdin),
        );
    }

    let mut compiler = Compiler::new(loader);

    let path = if options.path == "-" {
        wipple_frontend::FilePath::Virtual(wipple_frontend::helpers::InternedString::new("stdin"))
    } else {
        wipple_frontend::FilePath::Path(options.path.clone().into())
    };

    let program = compiler
        .analyze_with_progress(path, analysis_progress)
        .await;

    let program = program.map(|program| {
        if let Some(progress_bar) = progress_bar.as_deref() {
            progress_bar.set_message("Linting");
        }

        compiler.lint(&program);

        passes(progress_bar.as_deref(), &mut compiler, program)
    });

    let diagnostics = compiler.finish();

    let source_map = compiler.loader.source_map().lock().clone();

    (program, source_map, diagnostics)
}
