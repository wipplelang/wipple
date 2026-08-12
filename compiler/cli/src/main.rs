#![allow(clippy::print_stderr)]

mod driver;
mod read;

use crate::{driver::Driver, read::read_dir};
use clap::Parser;
use colored::Colorize;
use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use serde::Serialize;
use serde_json::json;
use std::{
    collections::{BTreeSet, HashMap},
    env, fs,
    io::{self, Write},
    net,
    ops::ControlFlow,
    path::{Path, PathBuf},
    process,
    sync::{
        Arc,
        atomic::{self, AtomicUsize},
    },
};
use wipple_core::{
    LibraryArtifact, TopLevel,
    ast::AstKey,
    codegen::{
        self, codegen,
        js::{self, JsResult},
    },
    db::{Db, DbRef, Node},
    default_filter,
    render::RenderMarkdownOptions,
    visit::definitions::Defined,
};
use wipple_feedback::FeedbackWriter;
use wipple_queries::QueryCtx;
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

    Repl {
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

    Format,
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

    #[clap(long)]
    trace: bool,

    #[clap(long)]
    source_map: bool,

    paths: Vec<PathBuf>,
}

fn make_temp_dir() -> io::Result<PathBuf> {
    Ok(tempfile::Builder::new().prefix("wipple").tempdir()?.keep())
}

fn main() -> anyhow::Result<()> {
    match Args::parse() {
        Args::Compile { output, options } => {
            compile(&options, output.as_deref())?;
        }
        Args::Run { output, options } => {
            let (output, cleanup) = match output.as_deref() {
                Some(path) => (path.to_path_buf(), false),
                None => (make_temp_dir()?, true),
            };

            if compile(&options, Some(&output))?.is_some() {
                run(&output, |cmd| cmd)?;
            }

            if cleanup {
                fs::remove_dir_all(&output)?;
            }
        }
        Args::Repl { options } => {
            repl(&options)?;
        }
        Args::Test { options } => {
            test(&options)?;
        }
        Args::Doc { options } => {
            doc(&options)?;
        }
        Args::Format => {
            format()?;
        }
    }

    Ok(())
}

fn setup(
    options: &CompileOptions,
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

    let mut db = Db::new(None);
    if env::var("WIPPLE_DEBUG").is_ok() {
        db.debug_enabled = true;
    }

    let mut top_level = TopLevel::default();

    let mut source_files = Vec::new();
    let mut statements = Vec::new();
    for path in &options.lib {
        db = Db::new(Some(DbRef::new(db)));

        let name = path.file_name().unwrap_or_default().to_string_lossy();

        let files = read_dir(&mut db, path)?;

        let mut driver = Driver::new(options, files, &mut out);
        driver.prefix = "Compiling ";
        driver.hide_facts = !options.lib_facts;

        let (_, lib_source_files, lib_statements) = driver
            .run(&mut db, &mut top_level, &name)?
            .ok_or_else(|| anyhow::format_err!("compilation failed"))?;

        source_files.extend(lib_source_files);
        statements.extend(lib_statements);
    }

    Ok((db, top_level, statements))
}

fn compile(
    options: &CompileOptions,
    output_path: Option<&Path>,
) -> anyhow::Result<Option<JsResult>> {
    let (lib_db, mut top_level, lib_statements) = setup(options, io::stdout())?;

    if options.paths.is_empty() {
        return Ok(None);
    }

    let mut db = Db::new(Some(DbRef::new(lib_db)));
    if env::var("WIPPLE_DEBUG").is_ok() {
        db.debug_enabled = true;
    }

    let files = options
        .paths
        .iter()
        .map(|path| {
            let source = fs::read_to_string(path)?;
            Ok(parse(&mut db, path.to_string_lossy(), source))
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    let name = options
        .paths
        .iter()
        .map(|path| path.to_string_lossy())
        .collect::<Vec<_>>()
        .join(", ");

    let mut driver = Driver::new(options, files, io::stdout());
    driver.prefix = "Compiling ";

    let (_, source_files, statements) = driver
        .run(&mut db, &mut top_level, &name)?
        .ok_or_else(|| anyhow::format_err!("compilation failed"))?;

    let program = codegen(&db, &source_files, &statements, &lib_statements, false)?;

    let result = js::to_js(
        &db,
        &program,
        codegen::Options {
            file_name: Some(JS_FILE_NAME),
            source_root: &format!("{}/", env::current_dir()?.display()),
            trace: if options.trace {
                codegen::TraceOptions::All
            } else {
                codegen::TraceOptions::None
            },
            incremental: false,
        },
    )?;

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

    if let Some(path) = output_path {
        write_js(&result, path)?;
    }

    Ok(Some(result))
}

static JS_FILE_NAME: &str = "main.js";

fn write_js(js: &JsResult, path: &Path) -> anyhow::Result<()> {
    fs::create_dir_all(path)?;

    fs::write(path.join(JS_FILE_NAME), &js.module)?;
    fs::write(path.join(format!("{JS_FILE_NAME}.map")), &js.source_map)?;

    macro_rules! copy {
        ($name:literal) => {
            fs::write(
                path.join($name),
                include_bytes!(concat!("../node-runtime/", $name)),
            )?;
        };
    }

    copy!("package.json");
    copy!("index.js");
    copy!("env.js");

    Ok(())
}

fn run(
    path: &Path,
    setup: impl FnOnce(&mut process::Command) -> &mut process::Command,
) -> anyhow::Result<process::Output> {
    let output = setup(process::Command::new("/usr/bin/env").args([
        "node".as_ref(),
        "--enable-source-maps".as_ref(),
        path,
    ]))
    .spawn()?
    .wait_with_output()?;

    if !output.status.success() {
        return Err(anyhow::format_err!(
            "script exited with status {}",
            output.status
        ));
    }

    Ok(output)
}

fn repl(options: &CompileOptions) -> anyhow::Result<()> {
    let (lib_db, mut top_level, lib_statements) = setup(options, io::stdout())?;

    let addr = net::TcpListener::bind("127.0.0.1:0")?.local_addr()?;

    eprintln!("{} (listening on port {})", "Wipple".bold(), addr.port());
    eprintln!(
        "{}",
        "Press Return twice to run, `show` to display output, ^C to exit".dimmed()
    );

    let _repl = process::Command::new("/usr/bin/env")
        .args([
            "node",
            "--enable-source-maps",
            "-e",
            concat!(
                include_str!("../node-runtime/env.js"),
                include_str!("../node-runtime/repl.js"),
            ),
            "--",
            addr.port().to_string().as_str(),
        ])
        .env("WIPPLE_REPL", "1")
        .stdin(process::Stdio::null())
        .stdout(process::Stdio::inherit())
        .stderr(process::Stdio::inherit())
        .spawn()?;

    let client = reqwest::blocking::Client::new();

    #[derive(
        Default, rustyline::Completer, rustyline::Helper, rustyline::Highlighter, rustyline::Hinter,
    )]
    struct Validator;

    impl rustyline::validate::Validator for Validator {
        fn validate(
            &self,
            ctx: &mut rustyline::validate::ValidationContext<'_>,
        ) -> rustyline::Result<rustyline::validate::ValidationResult> {
            // Require a second line break to submit
            if ctx.input().ends_with("\n") {
                Ok(rustyline::validate::ValidationResult::Valid(None))
            } else {
                Ok(rustyline::validate::ValidationResult::Incomplete)
            }
        }
    }

    let mut rl = rustyline::Editor::new()?;
    rl.set_helper(Some(Validator));

    let mut db = lib_db;
    let mut first = true;
    loop {
        match rl.readline("\n> ") {
            Ok(input) => {
                rl.add_history_entry(input.trim_end())?;

                db = Db::new(Some(DbRef::new(db)));
                if env::var("WIPPLE_DEBUG").is_ok() {
                    db.debug_enabled = true;
                }

                let name = format!("<repl#{}>", db.layer());

                let files = vec![parse(&mut db, &name, &input)];

                let mut driver = Driver::new(options, files, io::stdout());
                driver.silent = true;

                let Some((_, source_files, statements)) =
                    driver.run(&mut db, &mut top_level, &name)?
                else {
                    continue;
                };

                let program = codegen(&db, &source_files, &statements, &lib_statements, first)?;

                let result = js::to_js(
                    &db,
                    &program,
                    codegen::Options {
                        file_name: None,
                        source_root: &name,
                        trace: codegen::TraceOptions::None,
                        incremental: true,
                    },
                )?;

                client
                    .post(format!("http://{addr}"))
                    .body(result.module)
                    .send()?;
            }
            Err(
                rustyline::error::ReadlineError::Interrupted | rustyline::error::ReadlineError::Eof,
            ) => break,
            Err(err) => return Err(err.into()),
        }

        first = false;
    }

    Ok(())
}

fn test(options: &CompileOptions) -> anyhow::Result<()> {
    let mut out = Vec::new();
    let (lib_db, top_level, lib_statements) = setup(options, &mut out)?;

    let lib_db = DbRef::new(lib_db);

    let files = options
        .paths
        .iter()
        .map(|path| {
            let mut db = Db::new(Some(lib_db.clone()));

            let file_name = path
                .file_name()
                .unwrap_or_default()
                .to_string_lossy()
                .to_string();

            let source = fs::read_to_string(path)?;

            let file = parse(&mut db, &file_name, source);

            Ok((db, file_name, file))
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    #[derive(Serialize)]
    struct RunResult {
        file: String,
        output: String,
        graph: String,
    }

    let files_count = files.len();
    let counter = AtomicUsize::new(0);
    let run = |(mut db, name, file): (Db, String, AstKey)| {
        let mut out = Vec::new();

        let mut driver = Driver::new(options, vec![file], &mut out);
        driver.render_options = RenderMarkdownOptions::default().rich();
        driver.progress = Some((counter.fetch_add(1, atomic::Ordering::Relaxed), files_count));

        if let Some((_, source_files, statements)) =
            driver.run(&mut db, &mut top_level.clone(), &name)?
        {
            let program = codegen(&db, &source_files, &statements, &lib_statements, false)?;

            let js = js::to_js(
                &db,
                &program,
                codegen::Options {
                    file_name: Some(JS_FILE_NAME),
                    source_root: "",
                    trace: Default::default(),
                    incremental: false,
                },
            )?;

            let output_path = make_temp_dir()?;
            write_js(&js, &output_path)?;

            let output = run(&output_path, |cmd| cmd.stdout(process::Stdio::piped()))?.stdout;
            writeln!(out, "Output:")?;
            out.write_all(&output)?;

            fs::remove_dir_all(output_path)?;
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
        .by_uniform_blocks(block_size)
        .map(run)
        .collect::<anyhow::Result<Vec<_>>>();

    eprintln!();

    let results = results?;

    println!("{}", serde_json::to_string_pretty(&results)?);

    Ok(())
}

fn doc(options: &CompileOptions) -> anyhow::Result<()> {
    let (lib_db, mut top_level, _) = setup(options, io::stdout())?;

    let mut db = Db::new(Some(DbRef::new(lib_db)));

    let files = options
        .paths
        .iter()
        .map(|path| {
            let source = fs::read_to_string(path)?;
            Ok(parse(&mut db, path.to_string_lossy(), source))
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    let name = options
        .paths
        .iter()
        .map(|path| path.to_string_lossy())
        .collect::<Vec<_>>()
        .join(", ");

    let mut driver = Driver::new(options, files, io::stdout());
    driver.prefix = "Compiling ";

    driver
        .run(&mut db, &mut top_level, &name)?
        .ok_or_else(|| anyhow::format_err!("compilation failed"))?;

    let ctx = QueryCtx::new(&db, Arc::new(default_filter));

    let mut items = HashMap::new();
    db.for_each_fact::<Defined, ()>(&mut |db, node, _| {
        if let Some(documentation) = wipple_queries::documentation(&ctx, node) {
            let Some(name) = documentation.name else {
                return ControlFlow::Continue(());
            };

            let mut writer = FeedbackWriter::with_filter(&default_filter);
            writer.comments(db, &documentation.comments);
            let docs = writer
                .finish(db, |db, segment| {
                    segment.markdown(db, RenderMarkdownOptions::default().rich())
                })
                .message;

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

fn format() -> anyhow::Result<()> {
    let source = io::read_to_string(io::stdin())?;

    let formatted =
        wipple_parse::format(&source).ok_or_else(|| anyhow::format_err!("syntax error"))?;

    println!("{formatted}");

    Ok(())
}
