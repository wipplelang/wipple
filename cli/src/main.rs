use codemap_diagnostic::{ColorConfig, Emitter};
use frontend::{
    diagnostics::{Diagnostic, Diagnostics},
    parser,
};
use internment::Intern;
use serde::Serialize;
use std::{fs, io, path::PathBuf, process, sync::Arc};
use structopt::StructOpt;
use strum::{EnumString, ToString};
use wipple_frontend as frontend;

#[derive(StructOpt)]
#[structopt(
    name = "Wipple",
    bin_name = "wipple",
    about = "The Wipple programming language"
)]
enum Args {
    #[structopt(name = "parse", about = "Parse a Wipple source file")]
    Parse {
        #[structopt(name = "file")]
        path: PathBuf,

        #[structopt(long, default_value)]
        output_style: OutputStyle,
    },

    // FIXME: Temporary
    #[structopt(name = "lower")]
    Lower {
        #[structopt(name = "file")]
        path: PathBuf,
    },
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

    match args {
        Args::Parse { path, output_style } => {
            let code = Arc::from(fs::read_to_string(&path)?);
            let path = Intern::new(path);

            let mut diagnostics = Diagnostics::new();
            diagnostics.add_file(path, Arc::clone(&code));

            let file = frontend::parser::parse(path, &code, &mut diagnostics);

            match output_style {
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
        Args::Lower { path } => {
            let code = Arc::from(fs::read_to_string(&path)?);
            let path = Intern::new(path);

            let mut diagnostics = Diagnostics::new();
            diagnostics.add_file(path, Arc::clone(&code));

            let expr = frontend::parser::parse(path, &code, &mut diagnostics)
                .map(|file| frontend::lowering::lower(file, &mut diagnostics));

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
    file: Option<parser::File<'src>>,
    diagnostics: Vec<Diagnostic>,
}
