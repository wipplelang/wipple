use codemap_diagnostic::{ColorConfig, Emitter};
use interned_string::InternedString;
use serde::Serialize;
use std::{fs, io, path::PathBuf, process, sync::Arc};
use structopt::StructOpt;
use strum::{EnumString, ToString};
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
    #[structopt(about = "Parse a Wipple source file")]
    Parse {
        #[structopt(flatten)]
        options: SharedOptions,
    },

    #[structopt(about = "Format a Wipple source file")]
    Format {
        #[structopt(flatten)]
        options: SharedOptions,

        #[structopt(long)]
        in_place: bool,
    },

    // FIXME: Temporary
    Test {
        #[structopt(flatten)]
        options: SharedOptions,

        #[structopt(long)]
        project: Option<PathBuf>,

        #[structopt(long)]
        cache: Option<PathBuf>,

        #[structopt(long)]
        no_run: bool,
    },
}

#[derive(StructOpt)]
struct SharedOptions {
    #[structopt(name = "file")]
    path: String,

    #[structopt(long, default_value)]
    output_style: OutputStyle,
}

#[derive(Clone, Copy, EnumString, ToString)]
#[strum(serialize_all = "kebab-case")]
enum OutputStyle {
    Console,
    Json,
}

impl Default for OutputStyle {
    fn default() -> Self {
        OutputStyle::Console
    }
}

fn main() -> anyhow::Result<()> {
    let args = Args::from_args();

    match args.command {
        Command::Parse { options } => {
            let code = Arc::from(fs::read_to_string(&options.path)?);
            let path = InternedString::new(options.path);

            let mut diagnostics = Diagnostics::new();
            diagnostics.add_file(path, Arc::clone(&code));

            let result = wipple_parser::parse(path, &code, &mut diagnostics);

            match options.output_style {
                OutputStyle::Console => {
                    let (codemap, diagnostics) = diagnostics.into_console_friendly();

                    let mut emitter = Emitter::stderr(ColorConfig::Auto, Some(&codemap));
                    emitter.emit(&diagnostics);

                    if let Some(result) = result {
                        serde_json::to_writer_pretty(io::stdout(), &result)?;
                    } else {
                        process::exit(1);
                    }
                }
                OutputStyle::Json => {
                    let output = ParseOutput {
                        file: result,
                        diagnostics: diagnostics.diagnostics,
                    };

                    serde_json::to_writer(io::stdout(), &output)?;

                    if output.file.is_none() {
                        process::exit(1);
                    }
                }
            }
        }
        Command::Format { options, in_place } => {
            let code = Arc::from(fs::read_to_string(&options.path)?);
            let path = InternedString::new(options.path);

            let result = wipple_parser::parse(path, &code, &mut Diagnostics::new())
                .map(wipple_formatter::format);

            let result = match result {
                Some(result) => result,
                None => process::exit(1),
            };

            if in_place {
                todo!();
            } else {
                println!("{}", result);
            }
        }
        Command::Test {
            options,
            project,
            cache,
            no_run,
        } => {
            let path = InternedString::new(options.path);

            let mut diagnostics = Diagnostics::new();

            let project = wipple_frontend::Project {
                base: project.map(wipple_frontend::Base::Path),
                cache_path: Some(cache.unwrap_or_else(|| {
                    directories::BaseDirs::new()
                        .expect("User directories not set")
                        .cache_dir()
                        .join("wipple")
                })),
            };

            let files = {
                let mut info = wipple_frontend::Info::new(&mut diagnostics, &project);

                wipple_frontend::load_file(
                    &path.to_string(),
                    Span::new(path, Default::default()),
                    &mut info,
                )
                //.and_then(|_| Some(wipple_frontend::typecheck(&mut info)?.0))
            };

            let (codemap, diagnostics) = diagnostics.into_console_friendly();

            let mut emitter = Emitter::stderr(ColorConfig::Auto, Some(&codemap));
            emitter.emit(&diagnostics);

            if let Some(files) = files {
                if no_run {
                    serde_json::to_writer_pretty(io::stdout(), &files)?;
                } else {
                    // let external = wipple_playground_runner::external(|text| println!("{}", text));

                    // if let Err(error) = wipple_interpreter_backend::eval(&files, external) {
                    //     eprintln!("Fatal error: {:?}", error)
                    // }
                }
            } else {
                process::exit(1);
            }
        }
    }

    Ok(())
}

#[derive(Serialize)]
struct ParseOutput<'src> {
    file: Option<wipple_parser::File<'src>>,
    diagnostics: Vec<Diagnostic>,
}
