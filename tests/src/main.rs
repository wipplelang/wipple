use colored::Colorize;
use junit_report::*;
use std::{
    cell::RefCell,
    fs::{self, File},
    path::{Path, PathBuf},
    process::exit,
    rc::Rc,
};
use std::{env, time::Instant};
use wipple::*;

macro_rules! println_interactive {
    ($($arg:tt)*) => {
        if env::var("CI").is_err() {
            println!($($arg)*);
        }
    };
}

fn main() {
    let tests_folder = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");

    let args = std::env::args().collect::<Vec<_>>();

    let single_test = if args.get(1).filter(|&s| s == "--single").is_some() {
        Some(&args[2])
    } else {
        None
    };

    let mut pass_count = 0;
    let mut fail_count = 0;

    for path in fs::read_dir(tests_folder).unwrap() {
        let path = path.unwrap().path();

        if single_test.is_none() {
            println_interactive!("{}: {}", "TEST".blue(), path.to_string_lossy());
        }

        let mut suite = TestSuite::new(&path.to_string_lossy());

        for test_case in parse_test_file(&path) {
            if let Some(single_test) = single_test {
                if &test_case.name != single_test {
                    continue;
                }
            }

            let (output, duration) = test(&test_case.code);

            let reported_duration = Duration::from_std(duration).unwrap();

            if output == test_case.expected_output {
                println_interactive!(
                    "{}",
                    indent(
                        1,
                        &format!(
                            "{}: {} {}",
                            "PASS".green(),
                            test_case.name,
                            format!("(took {:.3} sec)", duration.as_secs_f32()).bright_black()
                        )
                    )
                );

                suite = suite.add_testcase(TestCase::success(&test_case.name, reported_duration));

                pass_count += 1;
            } else {
                let message = format!(
                    "Expected:\n{}\nFound:\n{}",
                    indent(1, &test_case.expected_output),
                    indent(1, &output),
                );

                println_interactive!(
                    "{}\n{}",
                    indent(
                        1,
                        &format!(
                            "{}: {} {}",
                            "FAIL".red(),
                            test_case.name,
                            format!("(took {:.3} sec)", duration.as_secs_f32()).bright_black()
                        )
                    ),
                    indent(2, &message),
                );

                suite = suite.add_testcase(TestCase::failure(
                    &test_case.name,
                    reported_duration,
                    "Invalid output",
                    &message,
                ));

                fail_count += 1;
            }
        }

        let report = Report::new().add_testsuite(suite);

        if env::var("CI").is_ok() {
            report
                .write_xml(&mut File::create(path.with_extension("report.xml")).unwrap())
                .unwrap();
        }
    }

    println_interactive!(
        "{}: {} tests, {}, {}",
        "DONE".blue(),
        pass_count + fail_count,
        format!("{} passed", pass_count).green(),
        {
            let text = format!("{} failed", fail_count);

            if fail_count > 0 {
                text.red().to_string()
            } else {
                text
            }
        },
    );

    exit(if fail_count > 0 { 1 } else { 0 });
}

const INDENT: &str = "  ";

fn indent(n: usize, string: &str) -> String {
    let indent = INDENT.repeat(n);

    string
        .split('\n')
        .map(|line| format!("{}{}", indent, line))
        .collect::<Vec<_>>()
        .join("\n")
}

fn filter_lines(string: &str, filter: impl Fn(&&str) -> bool) -> String {
    string
        .split('\n')
        .filter(filter)
        .collect::<Vec<_>>()
        .join("\n")
}

struct Test {
    name: String,
    code: String,
    expected_output: String,
}

fn parse_test_file(path: &Path) -> Vec<Test> {
    let file = fs::read_to_string(path).unwrap();

    let file = filter_lines(&file, |line| !line.starts_with('#'));

    file.split(">>>")
        .skip(1)
        .map(|test_code| {
            let parts: Vec<_> = test_code.split("---").map(|x| x.trim()).collect();
            let first_part: Vec<_> = parts[0].splitn(2, '\n').map(|x| x.trim()).collect();

            Test {
                name: String::from(first_part[0]),
                code: String::from(first_part[1]),
                expected_output: String::from(parts[1]),
            }
        })
        .collect()
}

fn test(code: &str) -> (String, std::time::Duration) {
    let start = Instant::now();

    let mut stack = Stack::new();

    let program = wipple_projects::load_string(code, None, &stack).expect("Failed to parse file");

    let output = Rc::new(RefCell::new(Vec::new()));

    wipple::setup();
    setup(output.clone(), &mut stack);

    wipple_stdlib::setup(&Environment::global(), &stack).expect("Failed to load standard library");

    if let Err(error) = wipple_projects::import_program_with_parent_env(
        program,
        None,
        &Environment::global(),
        &stack,
    ) {
        output.replace(vec![error.into_error(&stack).to_string()]);
    }

    let output = output.borrow().join("\n");
    let duration = start.elapsed();

    (output, duration)
}

fn setup(output: Rc<RefCell<Vec<String>>>, stack: &mut Stack) {
    *wipple_stdlib::show_mut_in(stack) = wipple_stdlib::ShowFn::new(move |value, env, stack| {
        let source_text = value.format(env, stack)?;
        let output_text = value.evaluate(env, stack)?.format(env, stack)?;

        output
            .borrow_mut()
            .push(format!("{} ==> {}", source_text, output_text));

        Ok(())
    });
}
