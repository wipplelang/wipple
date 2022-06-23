use clap::Parser;
use std::{
    env, fs,
    io::{self, Read, Write},
    os::unix::prelude::PermissionsExt,
    path::PathBuf,
    process::ExitCode,
    str::FromStr,
};
use wipple_compiler::Compiler;

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

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            if !error.to_string().is_empty() {
                eprintln!("{error}");
            }

            ExitCode::FAILURE
        }
    }
}

fn run() -> anyhow::Result<()> {
    let args = Args::parse();

    match args {
        Args::Run { options } => {
            let (program, _) = match build_and_optimize(options) {
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

            let (program, codemap) = match build(options) {
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

            let (program, _) = match build_and_optimize(options) {
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

    #[cfg(debug_assertions)]
    #[clap(long)]
    debug: bool,

    #[cfg(debug_assertions)]
    #[clap(long)]
    trace: bool,
}

mod loader {
    use reqwest::Url;
    use std::{borrow::Cow, collections::HashMap, fs, path::PathBuf, str::FromStr};
    use wipple_compiler::{helpers::InternedString, FilePath};

    pub type SourceMap = HashMap<wipple_compiler::FilePath, Cow<'static, str>>;

    pub struct Loader {
        pub base: String,
        pub source_map: SourceMap,
    }

    impl wipple_compiler::Loader for Loader {
        type Error = anyhow::Error;

        fn load(
            &mut self,
            path: FilePath,
            current: Option<FilePath>,
        ) -> Result<(FilePath, Cow<'static, str>), Self::Error> {
            match path {
                FilePath::Path(path) => {
                    let (path, code) = match Url::from_str(&path) {
                        Ok(url) => (FilePath::Path(path), download(url)?),
                        Err(_) => {
                            let base = match current {
                                Some(FilePath::Path(path)) => path.as_str(),
                                Some(_) => panic!("cannot load file from virtual path or prelude"),
                                None => &self.base,
                            };

                            let parsed_path = PathBuf::from(path.as_str());

                            if parsed_path.is_relative() {
                                match Url::from_str(base) {
                                    Ok(base) => {
                                        let url = base.join(path.as_str())?;

                                        (
                                            FilePath::Path(InternedString::from(url.to_string())),
                                            download(url)?,
                                        )
                                    }
                                    Err(_) => {
                                        let path = PathBuf::from(&self.base)
                                            .parent()
                                            .unwrap()
                                            .join(parsed_path);

                                        (
                                            FilePath::Path(InternedString::new(
                                                path.to_str().unwrap(),
                                            )),
                                            fs::read_to_string(path)?,
                                        )
                                    }
                                }
                            } else {
                                (FilePath::Path(path), fs::read_to_string(parsed_path)?)
                            }
                        }
                    };

                    // FIXME: Return a stable reference to the code inside the
                    // source map instead of cloning
                    self.source_map.insert(path, Cow::Owned(code.clone()));

                    Ok((path, Cow::Owned(code)))
                }
                FilePath::Virtual(_) => Err(anyhow::Error::msg("virtual paths are not supported")),
                FilePath::Prelude => {
                    let code = include_str!("../../support/prelude.wpl");
                    self.source_map.insert(path, Cow::Borrowed(code));

                    Ok((FilePath::Prelude, Cow::Borrowed(code)))
                }
                _ => unimplemented!(),
            }
        }
    }

    fn download(url: Url) -> anyhow::Result<String> {
        Ok(reqwest::blocking::get(url)?.text()?)
    }
}

fn build(options: BuildOptions) -> Option<(wipple_compiler::compile::Program, loader::SourceMap)> {
    build_with_passes(options, |_, program| program)
}

fn build_and_optimize(
    options: BuildOptions,
) -> Option<(wipple_compiler::optimize::Program, loader::SourceMap)> {
    build_with_passes(options, |compiler, program| {
        compiler.lint(&program);
        compiler.optimize(program)
    })
}

fn build_with_passes<P: std::fmt::Debug>(
    options: BuildOptions,
    passes: impl FnOnce(&mut Compiler<loader::Loader>, wipple_compiler::compile::Program) -> P,
) -> Option<(P, loader::SourceMap)> {
    #[cfg(debug_assertions)]
    let progress_bar = None::<indicatif::ProgressBar>;

    #[cfg(not(debug_assertions))]
    let progress_bar = Some(
        indicatif::ProgressBar::new(0).with_style(indicatif::ProgressStyle::default_spinner()),
    );

    let progress = |progress| {
        use wipple_compiler::compile::{build, typecheck};

        if let Some(progress_bar) = progress_bar.as_ref() {
            match progress {
                build::Progress::Resolving { count } => {
                    progress_bar.set_message(format!("Resolving {} files", count))
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
                    } => {
                        progress_bar.set_message(format!("({current}/{total}) Typechecking {path}"))
                    }
                    typecheck::Progress::Finalizing => progress_bar.set_message("Finalizing"),
                },
            }
        }
    };

    let loader = loader::Loader {
        base: env::current_dir().unwrap().to_string_lossy().into_owned(),
        source_map: Default::default(),
    };

    let mut compiler = Compiler::new(loader);

    let path = wipple_compiler::FilePath::Path(options.path.into());
    let program = compiler.build_with_progress(path, progress);

    let program = program.map(|program| passes(&mut compiler, program));

    let (loader, diagnostics) = compiler.finish();
    let success = !diagnostics.contains_errors();

    if let Some(progress_bar) = progress_bar {
        progress_bar.finish_and_clear();
    }

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

    #[cfg(debug_assertions)]
    if options.debug {
        eprintln!("{:#?}", program);
    }

    success
        .then(|| program.map(|program| (program, loader.source_map)))
        .flatten()
}
