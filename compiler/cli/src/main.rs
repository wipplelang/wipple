#![allow(clippy::print_stderr)]

mod driver;
mod read;

use crate::{driver::Driver, read::read_dir};
use clap::Parser;
use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use serde::Serialize;
use serde_json::json;
use std::{
    collections::{BTreeSet, HashMap},
    env, fs,
    io::{self, Write},
    ops::ControlFlow,
    path::{Path, PathBuf},
    process,
};
use wipple_core::{
    LibraryArtifact, TopLevel,
    codegen::{self, codegen, wasm},
    db::{Db, DbRef, Node},
    default_filter,
    visit::definitions::Defined,
};
use wipple_feedback::FeedbackWriter;
use wipple_syntax::parse;

#[derive(Debug, clap::Parser)]
enum Args {
    Compile {
        #[clap(short)]
        output: Option<PathBuf>,

        #[clap(flatten)]
        options: CompileOptions,
    },

    Run {
        #[clap(short)]
        output: Option<PathBuf>,

        #[clap(flatten)]
        options: CompileOptions,
    },

    Test {
        #[clap(flatten)]
        options: CompileOptions,
    },

    Doc {
        #[clap(flatten)]
        options: CompileOptions,
    },
}

#[derive(Debug, clap::Parser)]
struct CompileOptions {
    #[clap(long)]
    lib: Vec<PathBuf>,

    #[clap(long)]
    lib_artifact: Option<PathBuf>,

    #[clap(long)]
    facts: bool,

    #[clap(long)]
    lib_facts: bool,

    #[clap(long)]
    filter_facts: Option<String>,

    #[clap(long)]
    graph: bool,

    #[clap(long)]
    filter_feedback: Vec<String>,

    paths: Vec<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    match Args::parse() {
        Args::Compile { output, options } => {
            compile(&options, output.as_deref())?;
        }
        Args::Run { output, options } => {
            if let Some(wat) = compile(&options, None)? {
                run(&wat, output.as_deref(), |cmd| cmd)?;
            }
        }
        Args::Test { options } => {
            test(&options)?;
        }
        Args::Doc { options } => {
            doc(&options)?;
        }
    }

    Ok(())
}

fn setup(
    options: &CompileOptions,
    time: bool,
    mut out: impl io::Write,
) -> anyhow::Result<(Db, TopLevel, Vec<Node>)> {
    if let Some(path) = options.lib.first()
        && path.extension().and_then(|ext| ext.to_str()) == Some("bin")
    {
        if options.lib.len() > 1 {
            return Err(anyhow::format_err!(
                "only a single `--lib` is allowed when using a .bin artifact"
            ));
        }

        let bytes = fs::read(path)?;
        let artifact = rmp_serde::from_slice::<LibraryArtifact<Db>>(&bytes)?;

        return Ok((artifact.db, artifact.top_level, artifact.statements));
    }

    let mut db = Db::new();
    if env::var("WIPPLE_DEBUG").is_ok() {
        db.debug_enabled = true;
    }

    let mut top_level = TopLevel::default();

    let mut statements = Vec::new();
    for path in &options.lib {
        let parent = DbRef::new(db);
        db = Db::new();
        db.set_parent(parent);

        let name = path.file_name().unwrap_or_default().to_string_lossy();

        let files = read_dir(path)?;

        let mut driver = Driver::new(options, files, &mut out);
        driver.prefix = "Compiling ";
        driver.time = time;
        driver.hide_facts = !options.lib_facts;

        let lib_statements = driver
            .run(&mut db, &mut top_level, &name)?
            .ok_or_else(|| anyhow::format_err!("compilation failed"))?;

        statements.extend(lib_statements);
    }

    Ok((db, top_level, statements))
}

fn compile(options: &CompileOptions, output_path: Option<&Path>) -> anyhow::Result<Option<String>> {
    let (lib_db, mut top_level, lib_statements) = setup(options, true, io::stdout())?;

    if options.paths.is_empty() {
        return Ok(None);
    }

    let files = options
        .paths
        .iter()
        .map(|path| {
            let source = fs::read_to_string(path)?;
            Ok(parse(path.to_string_lossy(), source))
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    let name = options
        .paths
        .iter()
        .map(|path| path.to_string_lossy())
        .collect::<Vec<_>>()
        .join(", ");

    let mut db = Db::new();
    db.set_parent(DbRef::new(lib_db));

    let mut driver = Driver::new(options, files, io::stdout());
    driver.prefix = "Compiling ";
    driver.time = true;

    let statements = driver
        .run(&mut db, &mut top_level, &name)?
        .ok_or_else(|| anyhow::format_err!("compilation failed"))?;

    let program = codegen(&db, &statements, &lib_statements)?;

    let wat = wasm::write_to_string(&db, &program, codegen::Options::default())?;

    if let Some(output_path) = output_path {
        fs::write(output_path, &wat)?;
    }

    if let Some(path) = &options.lib_artifact {
        if path.extension().and_then(|ext| ext.to_str()) != Some("bin") {
            return Err(anyhow::format_err!(
                "expected .bin extension for `--lib-artifact`"
            ));
        }

        let artifact = LibraryArtifact {
            db,
            top_level,
            statements,
        };

        let bytes = rmp_serde::to_vec(&artifact)?;
        fs::write(path, bytes)?;
    }

    Ok(Some(wat))
}

fn run(
    wat: &str,
    path: Option<&Path>,
    setup: impl FnOnce(&mut process::Command) -> &mut process::Command,
) -> anyhow::Result<process::Output> {
    let (path, cleanup) = match path {
        Some(path) => {
            fs::create_dir_all(path)?;
            (path.to_path_buf(), false)
        }
        None => {
            let tempdir = tempfile::Builder::new().prefix("wipple").tempdir()?;
            (tempdir.keep(), true)
        }
    };

    macro_rules! copy {
        ($name:literal) => {
            fs::write(
                path.join($name),
                include_bytes!(concat!("../node-runtime/", $name)),
            )?;
        };
    }

    copy!("package.json");
    copy!("runtime.js");
    copy!("index.js");

    fs::write(path.join("main.wat"), wat)?;

    let wasm = wat::parse_str(wat)?;
    fs::write(path.join("main.wasm"), wasm)?;

    let output = setup(process::Command::new("/usr/bin/env").args([
        "node".as_ref(),
        "--enable-source-maps".as_ref(),
        path.as_path(),
    ]))
    .spawn()?
    .wait_with_output()?;

    if !output.status.success() {
        return Err(anyhow::format_err!(
            "script exited with status {}",
            output.status
        ));
    }

    if cleanup {
        fs::remove_dir_all(&path)?;
    }

    Ok(output)
}

fn test(options: &CompileOptions) -> anyhow::Result<()> {
    let mut out = Vec::new();
    let (lib_db, top_level, lib_statements) = setup(options, false, &mut out)?;

    let lib_db = DbRef::new(lib_db);

    let files = options
        .paths
        .iter()
        .map(|path| {
            let file_name = path
                .file_name()
                .unwrap_or_default()
                .to_string_lossy()
                .to_string();

            let source = fs::read_to_string(path)?;

            let file = parse(&file_name, source);

            Ok((file_name, file))
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    #[derive(Serialize)]
    struct RunResult {
        file: String,
        output: String,
        graph: String,
    }

    let files_count = files.len();
    let run = |(index, (name, file)): (usize, (String, Box<_>))| {
        let mut db = Db::new();
        db.set_parent(lib_db.clone());

        let mut out = Vec::new();

        let mut driver = Driver::new(options, vec![file], &mut out);
        driver.progress = Some((index, files_count));

        if let Some(statements) = driver.run(&mut db, &mut top_level.clone(), &name)? {
            let program = codegen(&db, &statements, &lib_statements)?;

            let wat = wasm::write_to_string(&db, &program, codegen::Options::default())?;

            let output = run(&wat, None, |cmd| cmd.stdout(process::Stdio::piped()))?.stdout;

            writeln!(out, "Output:")?;
            out.write_all(&output)?;
        }

        let mask = db
            .owned_nodes()
            .filter(|&node| default_filter(&db, node))
            .collect::<BTreeSet<_>>();

        let mut graph = String::new();
        db.graph.build(&db, &mask).write_dot(&mut graph)?;

        Ok(RunResult {
            file: name,
            output: String::from_utf8_lossy(&out).into_owned(),
            graph,
        })
    };

    let block_size = if env::var("WIPPLE_TEST_SEQUENTIAL").is_ok() {
        1
    } else {
        usize::MAX
    };

    let results = files
        .into_par_iter()
        .enumerate()
        .by_uniform_blocks(block_size)
        .map(run)
        .collect::<anyhow::Result<Vec<_>>>();

    eprintln!();

    let results = results?;

    println!("{}", serde_json::to_string_pretty(&results)?);

    Ok(())
}

fn doc(options: &CompileOptions) -> anyhow::Result<()> {
    let (lib_db, mut top_level, _) = setup(options, true, io::stdout())?;

    let files = options
        .paths
        .iter()
        .map(|path| {
            let source = fs::read_to_string(path)?;
            Ok(parse(path.to_string_lossy(), source))
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    let name = options
        .paths
        .iter()
        .map(|path| path.to_string_lossy())
        .collect::<Vec<_>>()
        .join(", ");

    let mut db = Db::new();
    db.set_parent(DbRef::new(lib_db));

    let mut driver = Driver::new(options, files, io::stdout());
    driver.prefix = "Compiling ";
    driver.time = true;

    driver
        .run(&mut db, &mut top_level, &name)?
        .ok_or_else(|| anyhow::format_err!("compilation failed"))?;

    let mut items = HashMap::new();
    db.for_each_fact::<Defined, ()>(&mut |db, node, _| {
        if let Some(documentation) = wipple_queries::documentation(db, node) {
            let Some(name) = documentation.name else {
                return ControlFlow::Continue(());
            };

            let mut writer = FeedbackWriter::default();
            writer.comments(db, &documentation.comments);
            let (docs, _) = writer.finish(db, |db, segment| segment.markdown(db, false));

            items.insert(
                name.to_string(),
                json!({
                    "declaration": documentation.declaration,
                    "kind": documentation.kind,
                    "docs": docs,
                }),
            );
        }
        ControlFlow::Continue(())
    });

    println!("{}", serde_json::to_string_pretty(&items)?);

    Ok(())
}
