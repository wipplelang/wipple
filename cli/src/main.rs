use clap::{ArgEnum, Parser};
use std::{
    borrow::Cow,
    env, fs,
    io::{self, Read, Write},
    os::unix::prelude::PermissionsExt,
    path::PathBuf,
    process,
    str::FromStr,
};
use url::Url;

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
    Build {
        #[clap(flatten)]
        options: BuildOptions,

        #[clap(arg_enum, long)]
        target: BuildTarget,
    },
    Bundle {
        #[clap(flatten)]
        options: BuildOptions,

        #[clap(short)]
        output: PathBuf,

        #[clap(long)]
        runner: PathBuf,
    },
    Lsp,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    match args {
        Args::Run { options } => {
            let program = match build(options) {
                Some(program) => program,
                None => process::exit(1),
            };

            let interpreter = wipple_interpreter_backend::Interpreter::handling_output(|text| {
                print!("{}", text);
                io::stdout().flush().unwrap();
            });

            if let Err((error, _)) = interpreter.eval(program) {
                eprintln!("fatal error: {}", error);
            }
        }
        Args::Build { options, target } => {
            let program = match build(options) {
                Some(program) => program,
                None => process::exit(1),
            };

            let code = codegen(program, target);

            print!("{}", code);
        }
        Args::Bundle {
            options,
            runner,
            output: bin,
        } => {
            let runner = fs::File::open(runner)
                .and_then(|file| file.bytes().collect::<Result<Vec<_>, _>>())
                .map_err(|error| anyhow::Error::msg(format!("could not load runner: {error}")))?;

            let program = match build(options) {
                Some(program) => program,
                None => process::exit(1),
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
        Args::Lsp => wipple_lsp_backend::start()?,
    }

    Ok(())
}

#[derive(Parser)]
pub struct BuildOptions {
    path: String,

    #[cfg(debug_assertions)]
    #[clap(long)]
    debug: bool,

    #[cfg(debug_assertions)]
    #[clap(long)]
    trace: bool,
}

pub fn build(options: BuildOptions) -> Option<wipple_compiler::optimize::Program> {
    use wipple_compiler::{helpers::InternedString, FilePath};

    struct Loader {
        base: String,
    }

    impl wipple_compiler::Loader for Loader {
        type Error = anyhow::Error;

        fn load(
            &self,
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

                    Ok((path, Cow::Owned(code)))
                }
                FilePath::Virtual(_) => Err(anyhow::Error::msg("virtual paths are not supported")),
                FilePath::Prelude => Ok((
                    FilePath::Prelude,
                    Cow::Borrowed(include_str!("../../support/prelude.wpl")),
                )),
                _ => unimplemented!(),
            }
        }
    }

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

    let loader = Loader {
        base: env::current_dir().unwrap().to_string_lossy().into_owned(),
    };

    let mut compiler = wipple_compiler::Compiler::new(loader);

    let path = wipple_compiler::FilePath::Path(options.path.into());
    let program = compiler.build_with_progress(path, progress);

    let program = program.map(|program| {
        compiler.lint(&program);
        compiler.optimize(program)
    });

    let diagnostics = compiler.finish();
    let success = !diagnostics.contains_errors();

    if let Some(progress_bar) = progress_bar {
        progress_bar.finish_and_clear();
    }

    let (codemap, _, diagnostics) = diagnostics.into_console_friendly(
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

    success.then(|| program).flatten()
}

fn download(url: Url) -> anyhow::Result<String> {
    Ok(reqwest::blocking::get(url)?.text()?)
}

#[derive(ArgEnum, Clone, Copy, PartialEq, Eq)]
enum BuildTarget {
    Node,
}

fn codegen(program: wipple_compiler::optimize::Program, target: BuildTarget) -> String {
    match target {
        BuildTarget::Node => wipple_node_backend::compile(program),
    }
}
