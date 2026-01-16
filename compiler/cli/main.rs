#![allow(clippy::print_stderr)]

mod lsp;

use clap::Parser;
use colored::Colorize;
use std::{
    env, fs,
    io::{self, Read, Write},
    path::{Path, PathBuf},
    process, time,
};
use wipple::{
    codegen::{self, CodegenCtx},
    database::{Db, RenderConfig},
    driver,
    syntax::format,
};

const PRELUDE: &str = concat!(
    include_str!("../runtime/node-prelude.js"),
    include_str!("../runtime/runtime.js")
);

#[derive(Debug, clap::Parser)]
enum Command {
    Compile {
        #[clap(flatten)]
        options: CompileOptions,
    },

    Run {
        #[clap(flatten)]
        options: CompileOptions,
    },

    Format {},

    Lsp {
        #[clap(long, required = true)]
        stdio: bool,

        #[clap(long)]
        lib: Option<PathBuf>,
    },
}

#[derive(Debug, clap::Parser)]
struct CompileOptions {
    #[clap(long)]
    lib: Vec<PathBuf>,

    #[clap(long)]
    facts: bool,

    #[clap(short, long)]
    output: Option<PathBuf>,

    #[clap(long)]
    filter_feedback: Vec<String>,

    #[clap(required = true)]
    paths: Vec<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    match Command::parse() {
        Command::Compile { options } => {
            compile(&options)?;
        }
        Command::Run { options } => {
            let script = compile(&options)?;

            enum OutputPath<'a> {
                FromOptions(&'a Path),
                Temp(tempfile::NamedTempFile),
            }

            impl AsRef<Path> for OutputPath<'_> {
                fn as_ref(&self) -> &Path {
                    match self {
                        OutputPath::FromOptions(path) => path,
                        OutputPath::Temp(file) => file.path(),
                    }
                }
            }

            let output_path = match &options.output {
                Some(path) => OutputPath::FromOptions(path),
                None => {
                    let mut file = tempfile::Builder::new().suffix(".js").tempfile()?;
                    file.write_all(script.as_bytes())?;
                    OutputPath::Temp(file)
                }
            };

            process::Command::new("/usr/bin/env")
                .args(["node".as_ref(), output_path.as_ref()])
                .spawn()?
                .wait()?;
        }
        Command::Format {} => {
            let mut source = String::new();
            io::stdin().read_to_string(&mut source)?;

            let Some(formatted) = format(&source) else {
                return Err(anyhow::format_err!("failed to format"));
            };

            println!("{formatted}");
        }
        Command::Lsp { stdio, lib } => {
            let _ = stdio;
            lsp::lsp(lib)?;
        }
    }

    Ok(())
}

fn compile(options: &CompileOptions) -> anyhow::Result<String> {
    let mut db = Db::new();

    let mut layers = options
        .lib
        .iter()
        .map(|path| driver::read_layer(&mut db, path))
        .collect::<anyhow::Result<Vec<_>>>()?;

    let files = options
        .paths
        .iter()
        .map(|path| Ok(driver::read_file(&mut db, path)?))
        .collect::<anyhow::Result<Vec<_>>>()?;

    layers.push(driver::Layer {
        name: options
            .paths
            .iter()
            .map(|path| Ok(path.file_name().unwrap_or_default().to_string_lossy()))
            .collect::<anyhow::Result<Vec<_>>>()?
            .join(", "),
        files: files.clone(),
    });

    db.render_with(RenderConfig::new(|db, value, f| {
        if let Some(node) = value.link() {
            let value = value.to_string(db);

            if colored::control::SHOULD_COLORIZE.should_colorize() {
                write!(
                    f,
                    "{} {}",
                    value.cyan(),
                    format!("({})", db.span(node)).dimmed()
                )?;
            } else {
                write!(f, "`{}` ({})", value, db.span(node))?;
            }
        } else {
            value.write(f, db)?;
        }

        Ok(())
    }));

    let feedback_filter = move |id: &str| {
        options.filter_feedback.is_empty() || options.filter_feedback.iter().any(|s| s == id)
    };

    for layer in layers {
        eprint!("Compiling {}...", layer.name);

        let start = time::Instant::now();

        driver::compile(&mut db, &layer.files);
        eprint!(" done");

        if env::var("CI").is_err() {
            let duration = time::Instant::now().duration_since(start);
            eprintln!(" ({duration:.0?})");
        } else {
            eprintln!();
        }
    }

    if options.facts {
        println!("Facts:");
        println!("{db}");
    }

    let mut feedback = String::new();
    let feedback_count = driver::write_feedback(&mut feedback, &db, feedback_filter);
    print!("{feedback}");

    if feedback_count > 0 {
        return Err(anyhow::format_err!(
            "compilation failed with {feedback_count} feedback item(s)"
        ));
    }

    let codegen = CodegenCtx::new(
        &mut db,
        options
            .output
            .as_deref()
            .unwrap_or_else(|| Path::new("index.js"))
            .to_string_lossy(),
        codegen::Options {
            prelude: PRELUDE,
            module: false,
        },
    );

    colored::control::set_override(false);
    let script = codegen.to_string(&files)?;
    colored::control::unset_override();

    if let Some(path) = &options.output {
        fs::write(path, &script)?;
    }

    Ok(script)
}
