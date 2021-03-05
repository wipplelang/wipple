use colored::Colorize;
use std::{cell::RefCell, fs, path::PathBuf, rc::Rc};
use wipple::*;

fn main() {
    let tests_folder = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");

    let mut pass_count = 0;
    let mut fail_count = 0;

    for path in fs::read_dir(tests_folder).unwrap() {
        let path = path.unwrap().path();

        println!("{}: {}", "TEST".blue(), path.to_string_lossy());

        for test_case in parse_test_file(&path) {
            let output = test(&test_case.code);

            if output == test_case.expected_output {
                print_indent(1, &format!("{}: {}", "PASS".green(), test_case.name));
                pass_count += 1;
            } else {
                print_indent(1, &format!("{}: {}", "FAIL".red(), test_case.name));
                print_indent(2, "Expected:");
                print_indent(3, &test_case.expected_output);
                print_indent(2, "Found:");
                print_indent(3, &output);
                fail_count += 1;
            }
        }

        println!();
    }

    println!(
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
}

const INDENT: &str = "  ";

fn print_indent(n: usize, string: &str) {
    let indent = INDENT.repeat(n);

    for line in string.split('\n') {
        println!("{}{}", indent, line);
    }
}

fn filter_lines(string: &str, filter: impl Fn(&&str) -> bool) -> String {
    string
        .split('\n')
        .filter(filter)
        .collect::<Vec<_>>()
        .join("\n")
}

struct TestCase {
    name: String,
    code: String,
    expected_output: String,
}

fn parse_test_file(path: &PathBuf) -> Vec<TestCase> {
    let file = fs::read_to_string(path).unwrap();

    let file = filter_lines(&file, |line| !line.starts_with('#'));

    file.split(">>>")
        .skip(1)
        .map(|test_code| {
            let parts: Vec<_> = test_code.split("---").map(|x| x.trim()).collect();
            let first_part: Vec<_> = parts[0].splitn(2, '\n').map(|x| x.trim()).collect();

            TestCase {
                name: String::from(first_part[0]),
                code: String::from(first_part[1]),
                expected_output: String::from(parts[1]),
            }
        })
        .collect()
}

fn test(code: &str) -> String {
    let ast = wipple_parser::parse(code).unwrap();
    let program = wipple_parser::convert(&ast, None);

    let output = Rc::new(RefCell::new(Vec::new()));

    let env = prelude().into_ref();
    setup(output.clone(), &env);

    if let Err(error) = program.evaluate(&env, &Stack::new()) {
        output.replace(vec![error.to_string()]);
    }

    let output = output.borrow();
    output.join("\n")
}

fn setup(output: Rc<RefCell<Vec<String>>>, env: &EnvironmentRef) {
    env.borrow_mut().variables().insert(
        String::from("show"),
        Value::of(Function::new(move |value, env, stack| {
            let source_text = value.format(env, stack);
            let output_text = value.evaluate(env, stack)?.format(env, stack);

            output
                .borrow_mut()
                .push(format!("{} ==> {}", source_text, output_text));

            Ok(Value::empty())
        })),
    );
}
