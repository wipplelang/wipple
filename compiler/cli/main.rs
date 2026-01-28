#![allow(clippy::print_stderr)]

mod lsp;

use clap::Parser;
use colored::Colorize;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::{
    env,
    fmt::Write as _,
    fs,
    io::{self, Read, Write},
    path::{Path, PathBuf},
    process, time,
};
use wipple::{
    codegen::{self, codegen},
    database::{Db, NodeRef, RenderConfig},
    driver,
    syntax::format,
};

const CODEGEN_OPTIONS: codegen::Options<'static> = codegen::Options {
    core: concat!(
        include_str!("../runtime/node.js"),
        include_str!("../runtime/core.js"),
    ),
    runtime: include_str!("../runtime/runtime.js"),
    module: false,
    sourcemap: true,
};

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

    Test {
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
            run(options.output.as_deref(), script, |cmd| cmd)?;
        }
        Command::Test { options } => {
            let outputs = test(&options)?;
            println!("{}", serde_json::to_string(&outputs)?);
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
    let (mut db, lib_files) = setup(options, true)?;

    let files = options
        .paths
        .iter()
        .map(|path| {
            let file = driver::read_file(&mut db, path)?;
            Ok((path.to_string_lossy(), file))
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    let (paths, files): (Vec<_>, Vec<_>) = files.iter().cloned().unzip();

    let layer = driver::Layer {
        name: paths.join(", "),
        files: files.clone(),
    };

    let (feedback_count, output) = compile_layer(options, &mut db, &layer, true);

    eprint!("{output}");

    if feedback_count == 0 {
        eprintln!();
    } else {
        return Err(anyhow::format_err!(
            "could not compile {}: {} feedback item(s)",
            layer.name,
            feedback_count
        ));
    }

    let script = codegen(&mut db, &files, &lib_files, CODEGEN_OPTIONS)?;

    if let Some(path) = &options.output {
        fs::write(path, &script)?;
    }

    Ok(script)
}

fn run(
    path: Option<&Path>,
    script: String,
    setup: impl FnOnce(&mut process::Command) -> &mut process::Command,
) -> Result<Vec<u8>, anyhow::Error> {
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

    let output_path = match path {
        Some(path) => OutputPath::FromOptions(path),
        None => {
            let mut file = tempfile::Builder::new().suffix(".js").tempfile()?;
            file.write_all(script.as_bytes())?;
            OutputPath::Temp(file)
        }
    };

    let output = setup(process::Command::new("/usr/bin/env").args([
        "node".as_ref(),
        "--enable-source-maps".as_ref(),
        output_path.as_ref(),
    ]))
    .spawn()?
    .wait_with_output()?;

    if !output.status.success() {
        return Err(anyhow::format_err!(
            "script exited with status {}",
            output.status
        ));
    }

    Ok(output.stdout)
}

fn test(options: &CompileOptions) -> anyhow::Result<Vec<serde_json::Value>> {
    let (db, lib_files) = setup(options, false)?;

    let layers = options
        .paths
        .iter()
        .map(|path| {
            let mut db = db.clone();

            let layer = driver::Layer {
                name: file_name(path),
                files: vec![driver::read_file(&mut db, path)?],
            };

            Ok((db, layer))
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    layers
        .into_par_iter()
        .map(|(mut db, layer)| {
            let (feedback_count, mut output) = compile_layer(options, &mut db, &layer, false);

            if feedback_count == 0 {
                let script = codegen(&mut db, &layer.files, &lib_files, CODEGEN_OPTIONS)?;
                let buf = run(None, script, |cmd| cmd.stdout(process::Stdio::piped()))?;
                writeln!(&mut output, "Output:").unwrap();
                output.push_str(str::from_utf8(&buf).unwrap());
            }

            Ok(serde_json::json!({
                "file": layer.name,
                "output": output,
            }))
        })
        .collect()
}

fn setup(options: &CompileOptions, time: bool) -> anyhow::Result<(Db, Vec<NodeRef>)> {
    let mut db = Db::new();

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

    let lib_layers = options
        .lib
        .iter()
        .map(|path| driver::read_layer(&mut db, path))
        .collect::<anyhow::Result<Vec<_>>>()?;

    let mut lib_files = Vec::new();
    for layer in lib_layers {
        compile_layer(options, &mut db, &layer, time);
        lib_files.extend(layer.files);
    }

    Ok((db, lib_files))
}

fn compile_layer(
    options: &CompileOptions,
    db: &mut Db,
    layer: &driver::Layer,
    time: bool,
) -> (usize, String) {
    if time {
        eprint!("Compiling {}...", layer.name);
    } else {
        eprintln!("Compiling {}", layer.name);
    }

    let start = time::Instant::now();

    driver::compile(db, &layer.files);

    if time {
        let duration = time::Instant::now().duration_since(start);
        eprintln!(" done ({duration:.0?})");
    }

    let mut output = String::new();

    if options.facts {
        let file_paths = layer
            .files
            .iter()
            .map(|file| db.span(file).path)
            .collect::<Vec<_>>();

        let node_is_in_layer = |node: &NodeRef| {
            env::var("WIPPLE_DEBUG").is_ok() || file_paths.contains(&db.span(node).path)
        };

        writeln!(&mut output, "Facts:\n{}", db.display(node_is_in_layer)).unwrap();
    }

    let feedback_filter = |id: &str| {
        options.filter_feedback.is_empty() || options.filter_feedback.iter().any(|s| s == id)
    };

    let feedback_count = driver::write_feedback(&mut output, db, feedback_filter);

    if feedback_count > 0 {
        writeln!(&mut output).unwrap();
    }

    (feedback_count, output)
}

fn file_name(path: &Path) -> String {
    path.file_name()
        .unwrap_or_default()
        .to_string_lossy()
        .to_string()
}
