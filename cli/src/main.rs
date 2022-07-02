use clap::Parser;
use std::{
    env, fs,
    io::{self, Read, Write},
    os::unix::prelude::PermissionsExt,
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
    Doc {
        #[clap(flatten)]
        options: BuildOptions,

        #[clap(long)]
        full: bool,
    },
    Bundle {
        #[clap(flatten)]
        options: BuildOptions,

        #[clap(short)]
        output: PathBuf,

        #[clap(long)]
        runner: PathBuf,
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
            let (program, _) = match build_and_optimize(options).await {
                Some(program) => program,
                None => return Err(anyhow::Error::msg("")),
            };

            let interpreter = wipple_interpreter_backend::Interpreter::handling_output(|text| {
                print!("{}", text);
                io::stdout().flush().unwrap();
            });

            if let Err((error, _)) = interpreter.eval(program) {
                eprintln!("fatal error: {}", error);
            }
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
        Args::Bundle {
            options,
            runner,
            output: bin,
        } => {
            let runner = fs::File::open(runner)
                .and_then(|file| file.bytes().collect::<Result<Vec<_>, _>>())
                .map_err(|error| anyhow::Error::msg(format!("could not load runner: {error}")))?;

            let (program, _) = match build_and_optimize(options).await {
                Some(program) => program,
                None => return Err(anyhow::Error::msg("failed to build")),
            };

            let exe = wipple_bundled_backend::bundle(program, runner);

            let _ = fs::remove_file(&bin);

            fs::File::create(&bin).and_then(|mut file| {
                file.write_all(&exe)?;

                let mut permissions = file.metadata()?.permissions();
                permissions.set_mode(0o755);
                file.set_permissions(permissions)?;

                Ok(())
            })?;
        }
    }

    Ok(())
}

#[derive(Parser)]
struct BuildOptions {
    path: String,

    #[clap(long, default_value = loader::STD_URL)]
    std: String,

    #[clap(long, conflicts_with = "std")]
    no_std: bool,

    #[cfg(debug_assertions)]
    #[clap(long)]
    debug: bool,

    #[cfg(debug_assertions)]
    #[clap(long)]
    trace: bool,
}

async fn build(
    options: BuildOptions,
) -> Option<(
    wipple_compiler::compile::Program,
    wipple_compiler::SourceMap,
)> {
    build_with_passes(options, |_, program| program).await
}

async fn build_and_optimize(
    options: BuildOptions,
) -> Option<(
    wipple_compiler::optimize::Program,
    wipple_compiler::SourceMap,
)> {
    build_with_passes(options, |compiler, program| {
        compiler.lint(&program);
        compiler.optimize(program)
    })
    .await
}

async fn build_with_passes<P: std::fmt::Debug>(
    options: BuildOptions,
    passes: impl FnOnce(&mut Compiler<loader::Loader>, wipple_compiler::compile::Program) -> P,
) -> Option<(P, wipple_compiler::SourceMap)> {
    #[cfg(debug_assertions)]
    let progress_bar = Arc::new(None::<indicatif::ProgressBar>);

    #[cfg(not(debug_assertions))]
    let progress_bar = Arc::new(Some(
        indicatif::ProgressBar::new(0).with_style(indicatif::ProgressStyle::default_spinner()),
    ));

    let progress = {
        let progress_bar = progress_bar.clone();

        move |progress| {
            use wipple_compiler::compile::{build, typecheck};

            if let Some(progress_bar) = progress_bar.as_ref() {
                match progress {
                    build::Progress::Resolving { path, count } => {
                        progress_bar.set_message(format!("({count} files) Resolving {}", path))
                    }
                    build::Progress::Lowering {
                        path,
                        current,
                        total,
                    } => progress_bar.set_message(format!("({current}/{total}) Compiling {path}")),
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
            wipple_compiler::FilePath::Path(wipple_compiler::helpers::InternedString::new(
                #[cfg(debug_assertions)]
                concat!(env!("CARGO_WORKSPACE_DIR"), "pkg/std/std.wpl"),
                #[cfg(not(debug_assertions))]
                if loader::is_url(&options.std) {
                    options.std
                } else {
                    PathBuf::from(options.std)
                        .canonicalize()
                        .unwrap()
                        .to_string_lossy()
                        .into_owned()
                },
            ))
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

    #[cfg(debug_assertions)]
    if options.debug {
        eprintln!("{:#?}", program);
    }

    success
        .then(|| program.map(|program| (program, loader.source_map().lock().clone())))
        .flatten()
}
