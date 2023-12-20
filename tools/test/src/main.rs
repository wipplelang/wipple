#![warn(clippy::dbg_macro, clippy::todo)]

use clap::Parser;
use colored::Colorize;
use parking_lot::Mutex;
use std::{
    env, fs,
    io::{self, Write},
    path::PathBuf,
    sync::{atomic::AtomicUsize, Arc},
};
use wipple_default_loader as loader;
use wipple_frontend::helpers::Shared;

#[derive(Parser)]
struct Args {
    files: Vec<PathBuf>,

    #[clap(long)]
    write: bool,

    #[clap(long)]
    junit: bool,

    #[clap(short = 'O')]
    optimize: bool,

    #[clap(long)]
    show_expansion_history: bool,

    #[cfg(debug_assertions)]
    #[clap(long)]
    trace: bool,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let loader = loader::Loader::new(
        None::<&str>,
        Some(
            env::current_dir()
                .unwrap()
                .join("std/std.wpl")
                .as_os_str()
                .to_str()
                .unwrap(),
        ),
    )
    .with_fetcher(loader::Fetcher::new().with_default_path_handler());

    let compiler = wipple_frontend::Compiler::new(loader);

    #[cfg(debug_assertions)]
    let compiler = compiler.set_backtrace_enabled(args.trace);

    let pass_count = AtomicUsize::new(0);
    let fail_count = AtomicUsize::new(0);
    let update_count = AtomicUsize::new(0);
    let results = Mutex::new(Vec::new());

    let run_path = |src_path: PathBuf| {
        let pass_count = &pass_count;
        let fail_count = &fail_count;
        let update_count = &update_count;
        let results = &results;
        let compiler = &compiler;

        async move {
            let test_name = src_path.to_string_lossy().to_string();
            let stdout_path = src_path.with_extension("stdout");
            let stderr_path = src_path.with_extension("stderr");

            eprint!(
                "{} {}",
                " RUNS ".black().on_bright_black(),
                test_name.bold(),
            );

            let src = fs::read_to_string(&src_path)?;
            let stdout = fs::read_to_string(&stdout_path).ok();
            let stderr = fs::read_to_string(&stderr_path).ok();

            let result = run(RunOptions {
                src: &src,
                stdout: stdout.as_deref(),
                stderr: stderr.as_deref(),
                compiler: compiler.clone(),
                optimize: args.optimize,
                show_expansion_history: args.show_expansion_history,
                #[cfg(debug_assertions)]
                trace_diagnostics: args.trace,
            })
            .await?;

            if result.passed() {
                pass_count.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

                eprintln!(
                    "\r{} {}",
                    " PASS ".bright_white().on_bright_green(),
                    test_name.bold()
                );
            } else if args.write {
                update_count.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

                eprintln!(
                    "\r{} {}",
                    " UPDATE ".bright_white().on_bright_black(),
                    test_name.bold()
                );

                if !result.output.is_empty() {
                    fs::write(&stdout_path, &result.output)?;
                }

                if !result.diagnostics.is_empty() {
                    fs::write(&stderr_path, &result.diagnostics)?;
                }
            } else {
                fail_count.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

                eprintln!(
                    "\r{} {}",
                    " FAIL ".bright_white().on_bright_red(),
                    test_name.bold()
                );

                if !result.output_diff.is_empty() {
                    eprintln!("\nOutput:\n");
                    result.output_diff.print();
                }

                if !result.diagnostics_diff.is_empty() {
                    eprintln!("\nDiagnostics:\n");
                    result.diagnostics_diff.print();
                }

                if !result.output_diff.is_empty() || !result.diagnostics_diff.is_empty() {
                    eprintln!();
                }
            }

            results.lock().push((test_name, src_path, result));

            anyhow::Result::<()>::Ok(())
        }
    };

    for file in args.files {
        run_path(file).await?;
    }

    let pass_count = pass_count.into_inner();
    let fail_count = fail_count.into_inner();
    let update_count = update_count.into_inner();
    let results = results.into_inner();

    eprintln!(
        "\n{} tests, {}, {}, {}",
        pass_count + fail_count + update_count,
        format!("{pass_count} passed").green(),
        format!("{fail_count} failed").red(),
        format!("{update_count} updated").bright_black(),
    );

    if args.junit {
        junit_report::ReportBuilder::new()
            .add_testsuite(
                junit_report::TestSuiteBuilder::new("tests")
                    .add_testcases(results.into_iter().map(|(test_name, path, result)| {
                        if result.passed() {
                            junit_report::TestCaseBuilder::success(
                                &test_name,
                                junit_report::Duration::ZERO,
                            )
                        } else {
                            let mut buf = Vec::new();

                            if !result.output_diff.is_empty() {
                                writeln!(buf, "\nOutput:\n").unwrap();
                                result.output_diff.write_to(&mut buf, false);
                            }

                            if !result.diagnostics_diff.is_empty() {
                                writeln!(buf, "\nDiagnostics:\n").unwrap();
                                result.diagnostics_diff.write_to(&mut buf, false);
                            }

                            let msg = String::from_utf8(buf).unwrap();

                            junit_report::TestCaseBuilder::failure(
                                &test_name,
                                junit_report::Duration::ZERO,
                                "output did not match snapshot",
                                &msg,
                            )
                        }
                        .set_filepath(&path.to_string_lossy())
                        .build()
                    }))
                    .build(),
            )
            .build()
            .write_xml(io::stdout())?;
    }

    Ok(())
}

#[derive(Debug)]
struct Diff(Vec<(similar::ChangeTag, String)>);

impl Diff {
    fn is_empty(&self) -> bool {
        self.0
            .iter()
            .all(|(tag, _)| matches!(tag, similar::ChangeTag::Equal))
    }

    fn print(&self) {
        self.write_to(io::stderr(), true);
    }

    fn write_to(&self, mut f: impl io::Write, colored: bool) {
        for (tag, text) in &self.0 {
            let color = match tag {
                similar::ChangeTag::Equal => colored::Color::White,
                similar::ChangeTag::Delete => colored::Color::Red,
                similar::ChangeTag::Insert => colored::Color::Green,
            };

            let line = format!("{}{}", tag, text.trim_end_matches('\n'));

            writeln!(
                f,
                "{}",
                if colored {
                    line.color(color).to_string()
                } else {
                    line
                }
            )
            .unwrap();
        }
    }
}

#[derive(Debug)]
struct TestResult {
    output_diff: Diff,
    output: String,
    diagnostics_diff: Diff,
    diagnostics: String,
}

impl TestResult {
    fn passed(&self) -> bool {
        self.output_diff.is_empty() && self.diagnostics_diff.is_empty()
    }
}

struct RunOptions<'a> {
    src: &'a str,
    stdout: Option<&'a str>,
    stderr: Option<&'a str>,
    compiler: wipple_frontend::Compiler,
    optimize: bool,
    show_expansion_history: bool,
    #[cfg(debug_assertions)]
    trace_diagnostics: bool,
}

async fn run(options: RunOptions<'_>) -> anyhow::Result<TestResult> {
    let test_path = wipple_frontend::helpers::InternedString::new("test");

    options
        .compiler
        .loader
        .insert_virtual(test_path, Arc::from(options.src));

    let (program, diagnostics) = options
        .compiler
        .analyze_with(test_path, &Default::default())
        .await;

    let success = !diagnostics.contains_errors();

    let output = {
        let buf = Shared::new(Vec::new());

        if success {
            let mut ir = options.compiler.ir_from(&program);

            if options.optimize {
                ir = options.compiler.optimize_with(ir, Default::default());
            }

            let interpreter = wipple_interpreter_backend::Interpreter::new({
                let buf = buf.clone();

                move |request| {
                    let buf = buf.clone();

                    Box::pin(async move {
                        match request {
                            wipple_interpreter_backend::IoRequest::Display(_, text, completion) => {
                                writeln!(buf.lock(), "{text}").unwrap();
                                completion();
                            }
                            _ => unimplemented!(),
                        }

                        Ok(())
                    })
                }
            });

            if let Err(error) = interpreter.run(&ir).await {
                write!(buf.lock(), "error: {error}")?;
            }
        }

        String::from_utf8(buf.into_unique()).unwrap()
    };

    let mut diagnostics = {
        let mut buf = Vec::new();
        {
            let (files, diagnostics) = diagnostics.into_console_friendly(
                loader::make_example_url,
                options.show_expansion_history,
                #[cfg(debug_assertions)]
                options.trace_diagnostics,
            );

            let mut writer = codespan_reporting::term::termcolor::NoColor::new(&mut buf);

            let config = codespan_reporting::term::Config::default();

            for diagnostic in diagnostics {
                codespan_reporting::term::emit(&mut writer, &config, &files, &diagnostic)?;
            }
        }

        String::from_utf8(buf).unwrap()
    };

    diagnostics = diagnostics.replace(env!("CARGO_WORKSPACE_DIR"), "<dir>/");

    Ok(TestResult {
        output_diff: diff(options.stdout.unwrap_or_default(), &output),
        output,
        diagnostics_diff: diff(options.stderr.unwrap_or_default(), &diagnostics),
        diagnostics,
    })
}

fn diff(old: &str, new: &str) -> Diff {
    // Remove trailing whitespace
    let trim = |str: &str| {
        str.lines()
            .map(|line| line.trim_end_matches(' ').to_string() + "\n")
            .collect::<String>()
    };

    let old = trim(old);
    let new = trim(new);

    Diff(
        similar::TextDiff::from_lines(&old, &new)
            .iter_all_changes()
            .map(|change| (change.tag(), change.to_string_lossy().into_owned()))
            .collect(),
    )
}
