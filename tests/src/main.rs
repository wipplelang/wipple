use colored::{ColoredString, Colorize};
use std::{process::exit, time::Duration};
use structopt::StructOpt;
use wipple_tests::*;

#[derive(StructOpt)]
pub struct Args {
    #[structopt(long = "file")]
    pub single_file: Option<String>,

    #[structopt(long = "test")]
    pub single_test: Option<String>,
}

fn main() {
    exit(run())
}

fn run() -> i32 {
    let args = Args::from_args();

    let mut test_files = load_tests();

    if let Some(file_name) = args.single_file {
        test_files.retain(|file| file.name == file_name);
    }

    if let Some(test_name) = args.single_test {
        test_files = test_files
            .into_iter()
            .filter_map(|mut file| {
                let tests = std::mem::take(&mut file.tests);

                if let Some(test) = tests.into_iter().find(|test| test.name == test_name) {
                    file.tests.push(test);
                    Some(file)
                } else {
                    None
                }
            })
            .collect();
    }

    let mut pass_count = 0;
    let mut fail_count = 0;

    let mut space = false;

    for file in test_files {
        if space {
            eprintln!();
        }

        eprintln!("{}\n", file.name.bold().underline());

        for test in file.tests {
            let result = (test.run)();

            fn duration_text(duration: Duration) -> ColoredString {
                format!("{}ms", duration.as_millis()).dimmed()
            }

            match result {
                TestResult::Passed { duration } => {
                    eprintln!(
                        "{} {} {}",
                        "PASS".bold().green(),
                        test.name,
                        duration_text(duration)
                    );

                    pass_count += 1;
                }
                TestResult::Failed {
                    expected,
                    found,
                    duration,
                } => {
                    if space {
                        eprintln!();
                    }

                    eprintln!(
                        "{} {} {}\n    Expected:\n{}\n    Found:\n{}",
                        "FAIL".bold().red(),
                        test.name,
                        duration_text(duration),
                        indent(2, &expected).dimmed(),
                        indent(2, &found).dimmed(),
                    );

                    if space {
                        eprintln!();
                    }

                    fail_count += 1;
                }
            }

            space = true;
        }
    }

    macro_rules! count_text {
        ($count:expr, $text:expr, $color:ident) => {{
            let text = format!("{} {}", $count, $text);

            if $count > 0 {
                text.$color()
            } else {
                text.normal()
            }
        }};
    }

    if space {
        eprintln!();
    }

    eprintln!(
        "{}",
        format!(
            "{} tests, {}, {}",
            pass_count + fail_count,
            count_text!(pass_count, "passed", green),
            count_text!(fail_count, "failed", red),
        )
        .bold()
    );

    // TODO: JUnit report

    if fail_count > 0 {
        1
    } else {
        0
    }
}

fn indent(n: usize, string: &str) -> String {
    let indent = "    ".repeat(n);

    string
        .split('\n')
        .map(|line| indent.clone() + line)
        .collect::<Vec<_>>()
        .join("\n")
}
