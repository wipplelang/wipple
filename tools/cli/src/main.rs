use clap::{Parser, ValueEnum};
use std::{
    env, fs,
    io::{self, Write},
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
    Run {
        #[clap(flatten)]
        options: BuildOptions,
    },
    Compile {
        #[clap(flatten)]
        options: BuildOptions,

        #[clap(long)]
        #[clap(value_enum)]
        target: Target,
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
}

#[derive(Parser)]
struct BuildOptions {
    path: String,

    #[clap(long)]
    std: Option<String>,

    #[clap(long, conflicts_with = "std")]
    no_std: bool,

    #[cfg(debug_assertions)]
    #[clap(long)]
    trace: bool,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum Target {
    Analysis,
    Ir,
    Rust,
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

        progress_bar.enable_steady_tick(80);

        progress_bar
    }));

    let emit_diagnostics = |diagnostics: wipple_frontend::diagnostics::FinalizedDiagnostics<_>,
                            options: &BuildOptions| {
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
        Args::Run { options } => {
            let ir = generate_ir(&options, progress_bar.clone()).await;

            if let Some(progress_bar) = progress_bar.as_ref() {
                progress_bar.finish_and_clear();
            }

            let ir = match ir {
                Ok(ir) => ir,
                Err(diagnostics) => {
                    emit_diagnostics(diagnostics, &options);

                    return Err(anyhow::Error::msg(""));
                }
            };

            #[allow(unused_mut)]
            let mut interpreter =
                wipple_interpreter_backend::Interpreter::handling_output(|text| {
                    print!("{}", text);
                    io::stdout().flush().unwrap();
                });

            if let Err(error) = interpreter.run(&ir) {
                eprintln!("fatal error: {}", error);
            }
        }
        Args::Compile { options, target } => {
            match target {
                Target::Analysis => {
                    let program = match analyze(&options, progress_bar.clone()).await {
                        Ok((program, _)) => program,
                        Err(diagnostics) => {
                            if let Some(progress_bar) = progress_bar.as_ref() {
                                progress_bar.finish_and_clear();
                            }

                            emit_diagnostics(diagnostics, &options);

                            return Err(anyhow::Error::msg(""));
                        }
                    };

                    serde_json::to_writer(io::stdout(), &program).unwrap();
                }
                Target::Ir => {
                    let ir = match generate_ir(&options, progress_bar.clone()).await {
                        Ok(ir) => ir,
                        Err(diagnostics) => {
                            if let Some(progress_bar) = progress_bar.as_ref() {
                                progress_bar.finish_and_clear();
                            }

                            emit_diagnostics(diagnostics, &options);

                            return Err(anyhow::Error::msg(""));
                        }
                    };

                    if let Some(progress_bar) = progress_bar.as_ref() {
                        progress_bar.finish_and_clear();
                    }

                    print!("{}", ir)
                }
                Target::Rust => {
                    let ir = match generate_ir(&options, progress_bar.clone()).await {
                        Ok(ir) => ir,
                        Err(diagnostics) => {
                            if let Some(progress_bar) = progress_bar.as_ref() {
                                progress_bar.finish_and_clear();
                            }

                            emit_diagnostics(diagnostics, &options);

                            return Err(anyhow::Error::msg(""));
                        }
                    };

                    if let Some(progress_bar) = progress_bar.as_deref() {
                        progress_bar.set_message("Compiling to Rust");
                    }

                    // TODO: Add a 'build' command that detects and installs
                    // rustc if needed, and then pipes this output into rustc
                    // (using `rustc -`) to build the final executable
                    let tt = wipple_rust_backend::compile(&ir)?;
                    let src = prettyplease::unparse(&syn::parse_file(&tt.to_string()).unwrap());

                    if let Some(progress_bar) = progress_bar.as_ref() {
                        progress_bar.finish_and_clear();
                    }

                    print!("{}", src)
                }
            }
        }
        Args::Doc { options, full } => {
            let root = match PathBuf::from_str(&options.path) {
                Ok(path) => path,
                Err(_) => {
                    return Err(anyhow::Error::msg(
                        "`doc` may only be used with local paths, unless `full` is set",
                    ));
                }
            };

            let (program, codemap) = match analyze(&options, progress_bar.clone()).await {
                Ok(ir) => ir,
                Err(diagnostics) => {
                    if let Some(progress_bar) = progress_bar.as_ref() {
                        progress_bar.finish_and_clear();
                    }

                    emit_diagnostics(diagnostics, &options);

                    return Err(anyhow::Error::msg("failed to build"));
                }
            };

            if let Some(progress_bar) = progress_bar.as_ref() {
                progress_bar.set_message("Generating documentation");
            }

            let doc = if full {
                wipple_frontend::doc::Documentation::new(program, codemap, &root)
            } else {
                wipple_frontend::doc::Documentation::with_filter(program, codemap, &root, |path| {
                    let path = match path {
                        wipple_frontend::FilePath::Path(path) => path,
                        _ => return false,
                    };

                    let path = match PathBuf::from_str(&path) {
                        Ok(path) => path,
                        Err(_) => return false,
                    };

                    root.ends_with(path)
                })
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

                eprintln!("cache cleared successfully");
            } else {
                println!("{}", cache_dir.to_string_lossy());
            }
        }
    }

    Ok(())
}

async fn analyze(
    options: &BuildOptions,
    progress_bar: Option<Arc<indicatif::ProgressBar>>,
) -> Result<
    (
        wipple_frontend::analysis::Program,
        wipple_frontend::SourceMap,
    ),
    wipple_frontend::diagnostics::FinalizedDiagnostics<loader::Loader>,
> {
    build_with_passes(options, progress_bar, |_, _, program| program).await
}

async fn generate_ir(
    options: &BuildOptions,
    progress_bar: Option<Arc<indicatif::ProgressBar>>,
) -> Result<
    wipple_frontend::ir::Program,
    wipple_frontend::diagnostics::FinalizedDiagnostics<loader::Loader>,
> {
    build_with_passes(options, progress_bar, |progress_bar, compiler, program| {
        if let Some(progress_bar) = progress_bar {
            progress_bar.set_message("Generating IR");
        }

        compiler.ir_from(&program)
    })
    .await
    .map(|(ir, _)| ir)
}

async fn build_with_passes<P>(
    options: &BuildOptions,
    progress_bar: Option<Arc<indicatif::ProgressBar>>,
    passes: impl FnOnce(
        Option<&indicatif::ProgressBar>,
        &mut Compiler<loader::Loader>,
        wipple_frontend::analysis::Program,
    ) -> P,
) -> Result<
    (P, wipple_frontend::SourceMap),
    wipple_frontend::diagnostics::FinalizedDiagnostics<loader::Loader>,
> {
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

    let loader = loader::Loader::new(
        Some(wipple_frontend::FilePath::Path(
            wipple_frontend::helpers::InternedString::new(
                env::current_dir().unwrap().to_string_lossy(),
            ),
        )),
        (!options.no_std).then(|| {
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

    let mut compiler = Compiler::new(loader);

    let path = wipple_frontend::FilePath::Path(options.path.clone().into());
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
    let success = !diagnostics.contains_errors();

    success
        .then(|| program.map(|program| (program, compiler.loader.source_map().lock().clone())))
        .flatten()
        .ok_or(diagnostics)
}
