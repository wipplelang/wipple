use codemap_diagnostic::{ColorConfig, Emitter};
use interned_string::InternedString;
use std::{
    fs,
    io::{Read, Write},
    os::unix::prelude::PermissionsExt,
    path::PathBuf,
    process,
};
use structopt::StructOpt;
use wipple_diagnostics::*;

#[derive(StructOpt)]
#[structopt(
    name = "Wipple",
    bin_name = "wipple",
    about = "The Wipple programming language"
)]
struct Args {
    #[structopt(subcommand)]
    command: Command,
}

#[derive(StructOpt)]
enum Command {
    Run {
        #[structopt(flatten)]
        options: SharedOptions,
    },

    Bundle {
        #[structopt(flatten)]
        options: SharedOptions,

        #[structopt(long)]
        runner: PathBuf,

        #[structopt(long)]
        bin: PathBuf,
    },
}

#[derive(StructOpt)]
struct SharedOptions {
    #[structopt(name = "file")]
    path: PathBuf,

    #[structopt(long)]
    project: Option<PathBuf>,

    #[structopt(long)]
    cache: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::from_args();

    match args.command {
        Command::Run { options } => {
            let item = match compile(options)? {
                Some(item) => item,
                None => process::exit(1),
            };

            wipple_interpreter_backend::set_output(|text| println!("{}", text));

            if let Err((error, callstack)) = wipple_interpreter_backend::eval(&item) {
                eprintln!("Fatal error: {}", error);

                for (function, span) in callstack {
                    eprintln!(
                        "  {} ({:?})",
                        function.as_deref().unwrap_or("<function>"),
                        span
                    )
                }
            }
        }
        Command::Bundle {
            options,
            runner,
            bin,
        } => {
            let item = match compile(options)? {
                Some(item) => item,
                None => process::exit(1),
            };

            let runner = fs::File::open(runner)
                .and_then(|file| file.bytes().collect::<Result<Vec<_>, _>>())
                .expect("Could not load runner");

            let exe = wipple_bundled_backend_builder::bundle(item, runner);

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

fn compile(options: SharedOptions) -> anyhow::Result<Option<wipple_frontend::typecheck::Item>> {
    let path_str = InternedString::new(options.path.to_str().unwrap());

    let mut diagnostics = Diagnostics::new();

    let project = wipple_frontend::project::Project {
        base: options.project.map(wipple_frontend::project::Base::Path),
        cache_path: Some(options.cache.unwrap_or_else(|| {
            directories::BaseDirs::new()
                .expect("User directories not set")
                .cache_dir()
                .join("wipple")
        })),
    };

    let item = {
        let mut info = wipple_frontend::compile::Info::with_prelude(&mut diagnostics, &project);

        wipple_frontend::project::load_path(&path_str, &options.path, &mut info)?.and_then(|_| {
            let (well_typed, item) = wipple_frontend::typecheck::typecheck(info);
            well_typed.then(|| item)
        })
    };

    let (codemap, diagnostics) = diagnostics.into_console_friendly();

    if !diagnostics.is_empty() {
        let mut emitter = Emitter::stderr(ColorConfig::Auto, Some(&codemap));
        emitter.emit(&diagnostics);
    }

    Ok(item)
}
