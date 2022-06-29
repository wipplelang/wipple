use clap::Parser;
use colored::Colorize;
use serde::Deserialize;
use std::{
    cell::RefCell,
    env, fs,
    io::{self, Write},
    path::PathBuf,
    sync::Arc,
};
use wipple_default_loader as loader;

#[derive(Parser)]
struct Args {
    path: PathBuf,

    #[clap(long)]
    junit: bool,

    #[cfg(debug_assertions)]
    #[clap(long)]
    debug: bool,

    #[cfg(debug_assertions)]
    #[clap(long)]
    trace: bool,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let mut loader = loader::Loader::new(
        String::new(),
        Some(wipple_compiler::FilePath::Path(
            wipple_compiler::helpers::InternedString::new(
                env::current_dir()
                    .unwrap()
                    .join("std/std.wpl")
                    .as_os_str()
                    .to_str()
                    .unwrap(),
            ),
        )),
    );

    let mut pass_count = 0usize;
    let mut fail_count = 0usize;
    let mut results = Vec::new();

    let mut run_path = |path: PathBuf| -> anyhow::Result<()> {
        let test_name = path.to_string_lossy().into_owned();

        eprint!(
            "{} {}",
            " RUNS ".black().on_bright_black(),
            test_name.as_str().bold()
        );

        let file = fs::File::open(&path)?;
        let test_case = serde_yaml::from_reader(file)?;
        let result = run(
            &test_case,
            wipple_compiler::Compiler::new(&mut loader),
            #[cfg(debug_assertions)]
            args.debug,
            #[cfg(debug_assertions)]
            args.trace,
        )?;

        if result.passed() {
            pass_count += 1;

            eprintln!(
                "\r{} {}",
                " PASS ".bright_white().on_bright_green(),
                test_name.as_str().bold()
            );
        } else {
            fail_count += 1;

            eprintln!(
                "\r{} {}",
                " FAIL ".bright_white().on_bright_red(),
                test_name.as_str().bold()
            );

            if !result.output_diff.is_empty() {
                eprintln!("\nOutput:\n");
                result.output_diff.print();
            }

            if !result.diagnostics_diff.is_empty() {
                eprintln!("\nDiagnostics:\n");
                result.diagnostics_diff.print();
            }

            eprintln!();
        }

        results.push((test_name, path, result));

        Ok(())
    };

    if args.path.is_file() {
        run_path(args.path)?;
    } else {
        let mut entries = fs::read_dir(args.path)?
            .map(|entry| entry.map(|e| e.path()))
            .collect::<Result<Vec<_>, _>>()?;

        entries.sort_unstable();

        for entry in entries {
            run_path(entry)?;
        }
    }

    eprintln!(
        "\n{} tests, {}, {}",
        pass_count + fail_count,
        format!("{} passed", pass_count).green(),
        format!("{} failed", fail_count).red(),
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

#[derive(Deserialize)]
struct TestCase {
    code: String,
    output: String,
    diagnostics: String,
}

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

struct TestResult {
    output_diff: Diff,
    diagnostics_diff: Diff,
}

impl TestResult {
    fn passed(&self) -> bool {
        self.output_diff.is_empty() && self.diagnostics_diff.is_empty()
    }
}

fn run(
    test_case: &TestCase,
    mut compiler: wipple_compiler::Compiler<loader::Loader>,
    #[cfg(debug_assertions)] debug: bool,
    #[cfg(debug_assertions)] trace: bool,
) -> anyhow::Result<TestResult> {
    let test_path = wipple_compiler::helpers::InternedString::new("test");

    compiler
        .loader
        .virtual_paths
        .insert(test_path, Arc::from(test_case.code.as_str()));

    let program = compiler
        .build(wipple_compiler::FilePath::Virtual(test_path))
        .map(|program| compiler.optimize(program));

    let diagnostics = compiler.finish();
    let success = !diagnostics.contains_errors();

    #[cfg(debug_assertions)]
    if debug {
        println!("{:#?}", program.as_ref().map(|p| &p.body));
    }

    let output = {
        let buf = RefCell::new(Vec::new());

        if success {
            if let Some(program) = program {
                let interpreter =
                    wipple_interpreter_backend::Interpreter::handling_output(|text| {
                        write!(buf.borrow_mut(), "{}", text).unwrap()
                    });

                if let Err((error, _)) = interpreter.eval(program) {
                    write!(buf.borrow_mut(), "fatal error: {}", error)?;
                }
            }
        }

        String::from_utf8(buf.into_inner()).unwrap()
    };

    let mut diagnostics = {
        let mut buf = Vec::new();
        {
            let (codemap, diagnostics) = diagnostics.into_console_friendly(
                #[cfg(debug_assertions)]
                trace,
            );

            let mut emitter = codemap_diagnostic::Emitter::new(Box::new(&mut buf), Some(&codemap));
            emitter.emit(&diagnostics);
        }

        String::from_utf8(buf).unwrap()
    };

    diagnostics = diagnostics.replace(env!("CARGO_WORKSPACE_DIR"), "<dir>/");

    Ok(TestResult {
        output_diff: diff(test_case.output.trim(), output.trim()),
        diagnostics_diff: diff(test_case.diagnostics.trim(), diagnostics.trim()),
    })
}

fn diff(old: &str, new: &str) -> Diff {
    Diff(
        similar::TextDiff::from_lines(old, new)
            .iter_all_changes()
            .map(|change| (change.tag(), change.to_string_lossy().into_owned()))
            .collect(),
    )
}
