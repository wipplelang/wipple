use clap::Parser;
use std::{
    borrow::Cow,
    env, fs,
    io::{Read, Write},
    os::unix::prelude::PermissionsExt,
    path::PathBuf,
    process,
};

#[derive(Parser)]
#[clap(
    name = "Wipple",
    bin_name = "wipple",
    about = "The Wipple programming language"
)]
enum Args {
    Run {
        #[clap(flatten)]
        options: Options,
    },
    Bundle {
        #[clap(flatten)]
        options: Options,

        #[clap(short)]
        output: PathBuf,

        #[clap(long)]
        runner: PathBuf,
    },
}

#[derive(Parser)]
struct Options {
    path: String,

    #[clap(long)]
    debug: bool,

    #[cfg(debug_assertions)]
    #[clap(long)]
    trace: bool,

    #[clap(flatten)]
    compiler_options: wipple_compiler::CompilerOptions,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    match args {
        Args::Run { options } => {
            let program = build(options);

            let interpreter = wipple_interpreter_backend::Interpreter::handling_output(|text| {
                println!("{}", text)
            });

            if let Err((error, _)) = interpreter.eval(program) {
                eprintln!("fatal error: {}", error);
            }
        }
        Args::Bundle {
            options,
            runner,
            output: bin,
        } => {
            let runner = fs::File::open(runner)
                .and_then(|file| file.bytes().collect::<Result<Vec<_>, _>>())
                .map_err(|error| anyhow::Error::msg(format!("could not load runner: {error}")))?;

            let program = build(options);

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

fn build(options: Options) -> wipple_compiler::compile::Program {
    struct Loader {
        base: PathBuf,
    }

    impl wipple_compiler::Loader for Loader {
        type Error = anyhow::Error;

        fn load(&self, path: wipple_compiler::FilePath) -> Result<Cow<'static, str>, Self::Error> {
            match path {
                wipple_compiler::FilePath::Path(path) => {
                    let path = self.base.join(path.as_str());
                    let code = fs::read_to_string(path)?;
                    Ok(Cow::Owned(code))
                }
                wipple_compiler::FilePath::Virtual(_) => {
                    Err(anyhow::Error::msg("virtual paths are not supported"))
                }
                wipple_compiler::FilePath::Prelude => {
                    Ok(Cow::Borrowed(include_str!("../../support/prelude.wpl")))
                }
                _ => unimplemented!(),
            }
        }
    }

    let loader = Loader {
        base: env::current_dir().unwrap(),
    };

    let mut compiler = wipple_compiler::Compiler::new(loader, options.compiler_options);

    let path = wipple_compiler::FilePath::Path(options.path.into());
    let program = compiler.build(path);
    let diagnostics = compiler.finish();
    let success = !diagnostics.contains_errors();

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

    if options.debug {
        println!("{program:#?}");
    }

    match program {
        Some(program) if success => program,
        _ => process::exit(1),
    }
}