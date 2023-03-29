mod document;
mod lsp;

use clap::{Parser, ValueEnum};
use parking_lot::Mutex;
use std::{
    fs,
    io::{self, Read, Write},
    path::PathBuf,
    process::ExitCode,
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
        path: String,

        #[clap(flatten)]
        options: BuildOptions,
    },
    Compile {
        path: String,

        #[clap(short, value_enum, default_value = "ir")]
        format: CompileFormat,

        #[clap(short)]
        output: Option<PathBuf>,

        #[clap(flatten)]
        options: BuildOptions,
    },
    Document {
        path: String,

        #[clap(short)]
        output: Option<PathBuf>,

        #[clap(flatten)]
        doc_options: document::Options,

        #[clap(flatten)]
        build_options: BuildOptions,
    },
    Cache {
        #[clap(long)]
        clear: bool,
    },
    Lsp,
}

#[derive(Parser)]
struct BuildOptions {
    #[cfg_attr(debug_assertions, clap(long))]
    #[cfg_attr(not(debug_assertions), clap(long, default_value = "true"))]
    progress: bool,

    #[clap(long)]
    std: Option<String>,

    #[clap(long)]
    base_path: Option<PathBuf>,

    #[clap(long)]
    show_expansion_history: bool,

    #[cfg(debug_assertions)]
    #[clap(long)]
    trace: bool,

    #[clap(long)]
    ide: bool,

    #[clap(short = 'O')]
    optimize: bool,

    #[clap(long)]
    no_lint: bool,
}

#[derive(Clone, Copy, ValueEnum)]
enum CompileFormat {
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

const PROGRESS_BAR_TICK_SPEED: u64 = 80;

async fn run() -> anyhow::Result<()> {
    let args = Args::parse();

    let progress_bar = || {
        Arc::new({
            let progress_bar = indicatif::ProgressBar::new(0).with_style(
                indicatif::ProgressStyle::default_spinner()
                    .tick_strings(&["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]),
            );

            progress_bar.enable_steady_tick(PROGRESS_BAR_TICK_SPEED);

            progress_bar
        })
    };

    let emit_diagnostics = |diagnostics: wipple_frontend::diagnostics::FinalizedDiagnostics,
                            options: &BuildOptions|
     -> anyhow::Result<()> {
        #[cfg(not(debug_assertions))]
        let _ = options;

        let (files, diagnostics) = diagnostics.into_console_friendly(
            options.show_expansion_history,
            #[cfg(debug_assertions)]
            options.trace,
        );

        let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
            codespan_reporting::term::termcolor::ColorChoice::Auto,
        );

        let config = codespan_reporting::term::Config::default();

        for diagnostic in diagnostics {
            codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)?;
        }

        Ok(())
    };

    match args {
        Args::Run { path, options } => {
            let progress_bar = options.progress.then(progress_bar);

            let (ir, diagnostics) = generate_ir(&path, &options, progress_bar.clone()).await;

            let success = !diagnostics.contains_errors();
            emit_diagnostics(diagnostics, &options)?;

            if let Some(progress_bar) = progress_bar.as_ref() {
                progress_bar.finish_and_clear();
            }

            let ir = match ir {
                Some(ir) if success => ir,
                _ => return Err(anyhow::Error::msg("")),
            };

            let mut interpreter = wipple_interpreter_backend::Interpreter::new(|request| {
                match request {
                    wipple_interpreter_backend::ConsoleRequest::Display(text, callback) => {
                        println!("{text}");
                        callback();
                    }
                    wipple_interpreter_backend::ConsoleRequest::Prompt(
                        prompt,
                        input_tx,
                        valid_rx,
                        callback,
                    ) => {
                        let prompt = prompt.to_string();
                        let input_tx = Arc::new(Mutex::new(input_tx));
                        let valid_rx = Arc::new(Mutex::new(valid_rx));

                        std::thread::spawn(move || {
                            dialoguer::Input::new()
                                .with_prompt(prompt)
                                .validate_with(|input: &String| {
                                    tokio::runtime::Builder::new_current_thread()
                                        .enable_all()
                                        .build()
                                        .unwrap()
                                        .block_on(async {
                                            input_tx.lock().send(input.to_string()).await.unwrap();
                                            let valid = valid_rx.lock().recv().await.unwrap();
                                            valid.then_some(()).ok_or("invalid input")
                                        })
                                })
                                .interact()
                                .unwrap();

                            valid_rx.lock().close();

                            callback();
                        });
                    }
                    wipple_interpreter_backend::ConsoleRequest::Choice(
                        prompt,
                        choices,
                        callback,
                    ) => {
                        let index = dialoguer::Select::new()
                            .with_prompt(prompt)
                            .items(&choices)
                            .default(0)
                            .interact()
                            .map_err(|err| err.to_string())?;

                        callback(index);
                    }
                    wipple_interpreter_backend::ConsoleRequest::LoadUi(_, _)
                    | wipple_interpreter_backend::ConsoleRequest::MessageUi(_, _, _, _) => {
                        // TODO: Prevent code that uses custom UI elements from even compiling
                        // (eg. a `platform` attribute)
                        panic!("custom UI elements are only supported in the Wipple Playground")
                    }
                }

                Ok(())
            });

            if let Err(error) = interpreter.run(&ir).await {
                eprintln!("fatal error: {error}");
            }
        }
        Args::Compile {
            path,
            format,
            options,
            output,
        } => {
            let output = || -> std::io::Result<Box<dyn Write>> {
                Ok(match output {
                    Some(output) => Box::new(std::fs::File::create(output)?),
                    None => Box::new(std::io::stdout()),
                })
            };

            match format {
                CompileFormat::Analysis => {
                    let progress_bar = options.progress.then(progress_bar);

                    let (program, diagnostics) =
                        analyze(&path, &options, progress_bar.clone()).await;

                    emit_diagnostics(diagnostics, &options)?;

                    if let Some(progress_bar) = progress_bar.as_ref() {
                        progress_bar.finish_and_clear();
                    }

                    let mut output = output()?;
                    output.write_all(program.to_string().as_bytes())?;
                    writeln!(output)?;
                }
                CompileFormat::Ir => {
                    let (ir, progress_bar) = {
                        let progress_bar = options.progress.then(progress_bar);

                        let (ir, diagnostics) =
                            generate_ir(&path, &options, progress_bar.clone()).await;

                        let error = diagnostics.contains_errors();
                        emit_diagnostics(diagnostics, &options)?;

                        match ir {
                            Some(ir) if !error => (ir, progress_bar),
                            _ => {
                                if let Some(progress_bar) = progress_bar.as_ref() {
                                    progress_bar.finish_and_clear();
                                }

                                return Err(anyhow::Error::msg(""));
                            }
                        }
                    };

                    if let Some(progress_bar) = progress_bar.as_ref() {
                        progress_bar.finish_and_clear();
                    }

                    let mut output = output()?;
                    output.write_all(ir.to_string().as_bytes())?;
                }
            }
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
        Args::Document {
            path,
            output,
            doc_options,
            build_options,
        } => {
            let progress_bar = build_options.progress.then(progress_bar);

            let (program, diagnostics) = analyze(&path, &build_options, progress_bar.clone()).await;

            let success = !diagnostics.contains_errors();
            emit_diagnostics(diagnostics, &build_options)?;

            if let Some(progress_bar) = progress_bar.as_ref() {
                progress_bar.finish_and_clear();
            }

            if !success {
                return Err(anyhow::Error::msg(""));
            }

            let output: Box<dyn Write> = match output {
                Some(output) => Box::new(std::fs::File::create(output)?),
                None => Box::new(std::io::stdout()),
            };

            document::document(&program, doc_options, output)?;
        }
        Args::Lsp => {
            lsp::run().await;
        }
    }

    Ok(())
}

async fn analyze(
    path: &str,
    options: &BuildOptions,
    progress_bar: Option<Arc<indicatif::ProgressBar>>,
) -> (
    wipple_frontend::analysis::Program,
    wipple_frontend::diagnostics::FinalizedDiagnostics,
) {
    build_with_passes(
        path,
        options,
        progress_bar,
        |progress_bar, compiler, success, mut program| {
            if !success {
                return program;
            }

            if options.optimize {
                if let Some(progress_bar) = progress_bar {
                    progress_bar.set_message("Optimizing");
                }

                program = compiler.optimize_with(program, Default::default());
            }

            program
        },
    )
    .await
}

async fn generate_ir(
    path: &str,
    options: &BuildOptions,
    progress_bar: Option<Arc<indicatif::ProgressBar>>,
) -> (
    Option<wipple_frontend::ir::Program>,
    wipple_frontend::diagnostics::FinalizedDiagnostics,
) {
    let (ir, diagnostics) = build_with_passes(
        path,
        options,
        progress_bar,
        |progress_bar, compiler, success, mut program| {
            if !success {
                return None;
            }

            if options.optimize {
                if let Some(progress_bar) = progress_bar {
                    progress_bar.set_message("Optimizing");
                }

                program = compiler.optimize_with(program, Default::default());
            }

            if let Some(progress_bar) = progress_bar {
                progress_bar.set_message("Generating IR");
            }

            let mut ir = compiler.ir_from(&program);

            if options.optimize {
                if let Some(progress_bar) = progress_bar {
                    progress_bar.set_message("Optimizing IR");
                }

                ir = compiler.optimize_with(ir, Default::default());
            }

            Some(ir)
        },
    )
    .await;

    (ir, diagnostics)
}

async fn build_with_passes<P>(
    path: &str,
    options: &BuildOptions,
    progress_bar: Option<Arc<indicatif::ProgressBar>>,
    passes: impl FnOnce(
        Option<&indicatif::ProgressBar>,
        &Compiler,
        bool,
        wipple_frontend::analysis::Program,
    ) -> P,
) -> (P, wipple_frontend::diagnostics::FinalizedDiagnostics) {
    let analysis_progress = {
        let progress_bar = progress_bar.clone();

        move |progress| {
            use wipple_frontend::analysis;

            if let Some(progress_bar) = progress_bar.as_ref() {
                match progress {
                    analysis::Progress::Resolving { path, count } => {
                        progress_bar.set_message(format!("({count} files) Resolving {path}"))
                    }
                    analysis::Progress::Lowering {
                        path,
                        current,
                        total,
                    } => progress_bar.set_message(format!("({current}/{total}) Lowering {path}")),
                    analysis::Progress::Typechecking(progress) => match progress {
                        analysis::typecheck::Progress::CollectingTypes => {
                            progress_bar.set_message("Collecting types")
                        }
                        analysis::typecheck::Progress::ResolvingDeclarations {
                            count,
                            remaining,
                        } => progress_bar.set_message(format!(
                            "({}/{}) Resolving declarations",
                            count,
                            count + remaining
                        )),
                    },
                }
            }
        }
    };

    let base = options
        .base_path
        .clone()
        .unwrap_or_else(|| PathBuf::from(path).parent().unwrap().to_path_buf())
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

    if path == "-" {
        let mut stdin = String::new();
        io::stdin().read_to_string(&mut stdin).unwrap();

        loader.virtual_paths().lock().insert(
            wipple_frontend::helpers::InternedString::new("stdin"),
            Arc::from(stdin),
        );
    }

    let compiler = Compiler::new(loader);

    #[cfg(debug_assertions)]
    let compiler = compiler.set_backtrace_enabled(options.trace);

    let path = if path == "-" {
        wipple_frontend::FilePath::Virtual(wipple_frontend::helpers::InternedString::new("stdin"))
    } else {
        wipple_frontend::FilePath::Path(wipple_frontend::helpers::InternedString::new(path))
    };

    let (program, diagnostics) = compiler
        .analyze_with(
            path,
            &wipple_frontend::analysis::Options::new()
                .tracking_progress(analysis_progress)
                .lint(!options.no_lint),
        )
        .await;

    let success = !diagnostics.contains_errors();

    let program = passes(progress_bar.as_deref(), &compiler, success, program);

    (program, diagnostics)
}
