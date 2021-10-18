use codemap_diagnostic::{ColorConfig, Emitter};
use internment::Intern;
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
    Lower {
        #[structopt(flatten)]
        options: SharedOptions,
    },
}

#[derive(StructOpt)]
struct SharedOptions {
    #[structopt(name = "file")]
    path: PathBuf,

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
            let path = Intern::new(options.path);

            let mut diagnostics = Diagnostics::new();
            diagnostics.add_file(path, Arc::clone(&code));

            let file = wipple_parser::parse(path, &code, &mut diagnostics);

            match options.output_style {
                OutputStyle::Console => {
                    let (codemap, diagnostics) = diagnostics.into_console_friendly();

                    let mut emitter = Emitter::stderr(ColorConfig::Auto, Some(&codemap));
                    emitter.emit(&diagnostics);

                    if let Some(file) = file {
                        serde_json::to_writer_pretty(io::stdout(), &file)?;
                    } else {
                        process::exit(1);
                    }
                }
                OutputStyle::Json => {
                    let output = ParseOutput {
                        file,
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
            let path = Intern::new(options.path);

            let file = wipple_parser::parse(path, &code, &mut Diagnostics::new())
                .map(wipple_formatter::format);

            let file = match file {
                Some(file) => file,
                None => process::exit(1),
            };

            if in_place {
                todo!();
            } else {
                println!("{}", file);
            }
        }
        Command::Lower { options } => {
            let code = Arc::from(fs::read_to_string(&options.path)?);
            let path = Intern::new(options.path);

            let mut diagnostics = Diagnostics::new();
            diagnostics.add_file(path, Arc::clone(&code));

            let expr = wipple_parser::parse(path, &code, &mut diagnostics)
                .map(|file| wipple_frontend::lowering::lower(file, &mut diagnostics))
                .and_then(|expr| wipple_frontend::analysis::analyze(expr, &mut diagnostics));

            let (codemap, diagnostics) = diagnostics.into_console_friendly();

            let mut emitter = Emitter::stderr(ColorConfig::Auto, Some(&codemap));
            emitter.emit(&diagnostics);

            if let Some(expr) = expr {
                serde_json::to_writer_pretty(io::stdout(), &expr)?;
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
