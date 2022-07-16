use clap::Parser;
use std::{
    env, fs,
    io::{self, Write},
    path::PathBuf,
    process::ExitCode,
    str::FromStr,
    sync::Arc,
};
use wipple_compiler::{Compiler, Loader};
use wipple_default_loader as loader;

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
    DumpIr {
        #[clap(flatten)]
        options: BuildOptions,
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

    match args {
        Args::Run { options } => {
            let program = match build_ir(options).await {
                Some(program) => program,
                None => return Err(anyhow::Error::msg("")),
            };

            let interpreter = wipple_interpreter_backend::Interpreter::handling_output(|text| {
                print!("{}", text);
                io::stdout().flush().unwrap();
            });

            if let Err(error) = interpreter.run(&program) {
                eprintln!("fatal error: {}", error);
            }
        }
        Args::DumpIr { options } => {
            let program = match build_ir(options).await {
                Some(program) => program,
                None => return Err(anyhow::Error::msg("")),
            };

            let mut buf = Vec::new();
            program.to_writer_pretty(&mut buf).unwrap();
            let out = String::from_utf8(buf).unwrap();
            eprintln!("{}", out);
        }
        Args::Doc { options, full } => {
            let root = match PathBuf::from_str(&options.path) {
                Ok(path) => path,
                Err(_) => {
                    return Err(anyhow::Error::msg(
                        "`doc` may only be used with local paths, unless `full` is set",
                    ))
                }
            };

            let (program, codemap) = match build(options).await {
                Some(program) => program,
                None => return Err(anyhow::Error::msg("failed to build")),
            };

            let doc = if full {
                wipple_compiler::doc::Documentation::new(program, codemap, &root)
            } else {
                wipple_compiler::doc::Documentation::with_filter(program, codemap, &root, |path| {
                    let path = match path {
                        wipple_compiler::FilePath::Path(path) => path,
                        _ => return false,
                    };

                    let path = match PathBuf::from_str(&path) {
                        Ok(path) => path,
                        Err(_) => return false,
                    };

                    root.ends_with(path)
                })
            };

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

async fn build(
    options: BuildOptions,
) -> Option<(
    wipple_compiler::analysis::typecheck::Program,
    wipple_compiler::SourceMap,
)> {
    build_with_passes(options, |_, program| program).await
}

async fn build_ir(options: BuildOptions) -> Option<wipple_compiler::ir::Program> {
    build_with_passes(options, |compiler, program| {
        compiler.lint(&program);
        compiler.ir_from(program)
    })
    .await
    .map(|(ir, _)| ir)
}

async fn build_with_passes<P>(
    options: BuildOptions,
    passes: impl FnOnce(
        &mut Compiler<loader::Loader>,
        wipple_compiler::analysis::typecheck::Program,
    ) -> P,
) -> Option<(P, wipple_compiler::SourceMap)> {
    #[cfg(debug_assertions)]
    let progress_bar = Arc::new(None::<indicatif::ProgressBar>);

    #[cfg(not(debug_assertions))]
    let progress_bar = Arc::new(Some({
        let progress_bar = indicatif::ProgressBar::new(0).with_style(
            indicatif::ProgressStyle::default_spinner()
                .tick_strings(&["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]),
        );

        progress_bar.enable_steady_tick(80);

        progress_bar
    }));

    let progress = {
        let progress_bar = progress_bar.clone();

        move |progress| {
            use wipple_compiler::analysis::{build, typecheck};

            if let Some(progress_bar) = progress_bar.as_ref() {
                match progress {
                    build::Progress::Resolving { path, count } => {
                        progress_bar.set_message(format!("({count} files) Resolving {}", path))
                    }
                    build::Progress::Lowering {
                        path,
                        current,
                        total,
                    } => progress_bar.set_message(format!("({current}/{total}) Lowering {path}")),
                    build::Progress::Typechecking(progress) => match progress {
                        typecheck::Progress::Typechecking {
                            path,
                            current,
                            total,
                        } => progress_bar
                            .set_message(format!("({current}/{total}) Typechecking {path}")),
                        typecheck::Progress::Finalizing => progress_bar.set_message("Finalizing"),
                    },
                }
            }
        }
    };

    let loader = loader::Loader::new(
        Some(wipple_compiler::FilePath::Path(
            wipple_compiler::helpers::InternedString::new(
                env::current_dir().unwrap().to_string_lossy(),
            ),
        )),
        (!options.no_std).then(|| {
            let path = options.std.as_deref();

            wipple_compiler::FilePath::Path(
                #[cfg(debug_assertions)]
                wipple_compiler::helpers::InternedString::new(
                    path.unwrap_or(concat!(env!("CARGO_WORKSPACE_DIR"), "pkg/std/std.wpl")),
                ),
                #[cfg(not(debug_assertions))]
                {
                    let path = path.unwrap_or(loader::STD_URL);

                    if loader::is_url(path) {
                        wipple_compiler::helpers::InternedString::new(path)
                    } else {
                        wipple_compiler::helpers::InternedString::new(
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

    let path = wipple_compiler::FilePath::Path(options.path.into());
    let program = compiler.build_with_progress(path, progress).await;

    let program = program.map(|program| passes(&mut compiler, program));

    let diagnostics = compiler.finish();
    let success = !diagnostics.contains_errors();

    if let Some(progress_bar) = progress_bar.as_ref() {
        progress_bar.finish_and_clear();
    }

    let (loader, codemap, diagnostics) = diagnostics.into_console_friendly(
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

    success
        .then(|| program.map(|program| (program, loader.source_map().lock().clone())))
        .flatten()
}
