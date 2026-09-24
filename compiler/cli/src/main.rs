#![allow(clippy::print_stderr)]

mod driver;
mod interpreter;
mod read;

use crate::{driver::Driver, interpreter::create_interpreter, read::read_dir};
use clap::Parser;
use colored::Colorize;
use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use serde::Serialize;
use serde_json::json;
use std::{
    collections::{BTreeSet, HashMap},
    env, fs,
    io::{self, Write},
    ops::ControlFlow,
    path::PathBuf,
    str::FromStr,
    sync::{
        Arc,
        atomic::{self, AtomicUsize},
    },
};
use wipple_core::{
    LibraryArtifact, TopLevel,
    ast::AstKey,
    codegen,
    db::{Db, DbRef, Node, NodeId},
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
        #[clap(flatten)]
        options: CompileOptions,
    },

    Run {
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
    filter_facts: Vec<FilterFacts>,

    #[clap(long)]
    graph: bool,

    #[clap(long)]
    explain: bool,

    #[clap(long)]
    filter_feedback: Vec<String>,

    #[clap(long)]
    all_feedback: bool,

    #[clap(long)]
    trace: bool,

    #[clap(long)]
    mir: Option<PathBuf>,

    paths: Vec<PathBuf>,
}

#[derive(Debug, Clone)]
enum FilterFacts {
    Feedback,
    Node(NodeId),
}

impl FromStr for FilterFacts {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "feedback" {
            Ok(FilterFacts::Feedback)
        } else if let Ok(id) = s.parse::<NodeId>() {
            Ok(FilterFacts::Node(id))
        } else {
            Err(anyhow::format_err!("invalid filter {s:?}"))
        }
    }
}

fn main() -> anyhow::Result<()> {
    match Args::parse() {
        Args::Compile { options } => {
            compile(&options)?;
        }
        Args::Run { options } => {
            if let Some(mir) = compile(&options)? {
                let interpreter = create_interpreter(io::stdout());
                interpreter.run(&mir)?;
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
        driver.render_options = RenderMarkdownOptions::default()
            .rich(true)
            .color(supports_color());

        let (_, lib_source_files, lib_statements) = driver
            .run(&mut db, &mut top_level, &name)?
            .ok_or_else(|| anyhow::format_err!("compilation failed"))?;

        source_files.extend(lib_source_files);
        statements.extend(lib_statements);
    }

    Ok((db, top_level, statements))
}

fn compile(options: &CompileOptions) -> anyhow::Result<Option<codegen::mir::Program>> {
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
    driver.render_options = RenderMarkdownOptions::default()
        .rich(true)
        .color(supports_color());

    let (_, source_files, statements) = driver
        .run(&mut db, &mut top_level, &name)?
        .ok_or_else(|| anyhow::format_err!("compilation failed"))?;

    let hir = codegen::hir::Program::from_statements(
        &db,
        &source_files,
        &statements,
        &lib_statements,
        Default::default(),
    )?;

    let mut mir = codegen::mir::Program::default();
    mir.extend_from_hir(
        &db,
        &hir,
        &mut Default::default(),
        codegen::mir::Options {
            trace: if options.trace {
                codegen::mir::TraceOptions::All
            } else {
                codegen::mir::TraceOptions::None
            },
        },
    )?;

    if let Some(mir_path) = &options.mir {
        serde_json::to_writer_pretty(fs::File::create(mir_path)?, &mir)?;
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

    Ok(Some(mir))
}

fn repl(options: &CompileOptions) -> anyhow::Result<()> {
    let (lib_db, mut top_level, lib_statements) = setup(options, io::stdout())?;

    let mut program = codegen::mir::Program::default();

    let mut map = codegen::mir::IndexMap::default();
    let mut codegen = move |program: &mut codegen::mir::Program,
                            db: &Db,
                            include_definitions: codegen::hir::IncludeDefinitions,
                            source_files: &[Node],
                            statements: &[Node]|
          -> Result<(), codegen::CodegenError> {
        let hir = codegen::hir::Program::from_statements(
            db,
            source_files,
            statements,
            &lib_statements,
            include_definitions,
        )?;

        program.extend_from_hir(
            db,
            &hir,
            &mut map,
            codegen::mir::Options {
                trace: if options.trace {
                    codegen::mir::TraceOptions::All
                } else {
                    codegen::mir::TraceOptions::None
                },
            },
        )?;

        Ok(())
    };

    codegen(
        &mut program,
        &lib_db,
        codegen::hir::IncludeDefinitions::for_library(),
        &[],
        &[],
    )?;

    eprintln!("{}", "Wipple".bold());
    eprintln!(
        "{}",
        "Press Return twice to run, `show` to display output, ^C to exit".dimmed()
    );

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

    let mut db = DbRef::new(lib_db);
    loop {
        match rl.readline("\n> ") {
            Ok(input) => {
                rl.add_history_entry(input.trim_end())?;

                let mut next_db = Db::new(Some(db.clone()));
                if env::var("WIPPLE_DEBUG").is_ok() {
                    next_db.debug_enabled = true;
                }

                let name = format!("repl#{}", next_db.layer());

                let files = vec![parse(&mut next_db, &name, &input)];

                let mut driver = Driver::new(options, files, io::stdout());
                driver.silent = true;
                driver.render_options = RenderMarkdownOptions::default()
                    .rich(true)
                    .color(supports_color());

                let Some((_, source_files, statements)) =
                    driver.run(&mut next_db, &mut top_level, &name)?
                else {
                    continue;
                };

                codegen(
                    &mut program,
                    &next_db,
                    codegen::hir::IncludeDefinitions::for_repl(),
                    &source_files,
                    &statements,
                )?;

                let interpreter = create_interpreter(io::stdout());
                interpreter.run(&program)?;

                db = DbRef::new(next_db);
            }
            Err(
                rustyline::error::ReadlineError::Interrupted | rustyline::error::ReadlineError::Eof,
            ) => break,
            Err(err) => return Err(err.into()),
        }
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
        driver.render_options = RenderMarkdownOptions::default().rich(true);
        driver.progress = Some((counter.fetch_add(1, atomic::Ordering::Relaxed), files_count));

        if let Some((_, source_files, statements)) =
            driver.run(&mut db, &mut top_level.clone(), &name)?
        {
            let hir = codegen::hir::Program::from_statements(
                &db,
                &source_files,
                &statements,
                &lib_statements,
                Default::default(),
            )?;

            let mut mir = codegen::mir::Program::default();
            mir.extend_from_hir(&db, &hir, &mut Default::default(), Default::default())?;

            writeln!(out, "Output:")?;

            let interpreter = create_interpreter(&mut out);
            interpreter.run(&mir)?;
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

            let mut writer = FeedbackWriter::new(&default_filter, Vec::new());
            writer.comments(db, &documentation.comments);
            let docs = writer
                .finish(db, |db, segment| {
                    segment.markdown(db, RenderMarkdownOptions::default().rich(true))
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

fn supports_color() -> bool {
    supports_color::on(supports_color::Stream::Stdout).is_some()
}
