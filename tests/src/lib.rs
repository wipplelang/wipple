use fancy_regex::Regex;
use std::{
    fs,
    path::PathBuf,
    time::{Duration, Instant},
};
use wipple_parser::line_col;
use wipple_stdlib::*;

pub struct TestFile {
    pub path: PathBuf,
    pub name: String,
    pub tests: Vec<Test>,
}

pub struct Test {
    pub name: String,
    pub line: usize,
    pub run: Box<dyn FnOnce() -> TestResult>,
}

pub enum TestResult {
    Passed {
        duration: Duration,
    },
    Failed {
        expected: String,
        found: String,
        duration: Duration,
    },
}

pub fn load_tests() -> Vec<TestFile> {
    let tests_folder = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");

    let regex = Regex::new(
        r"(?sx)
        >>>\ (?P<name>[^\n]*\n*)     # test name
        (?P<code>(?:.(?!\n---))*)    # test code
        \n*---\n*                    # separator
        (?P<output>(.(?!\n>>>\ ))*)  # expected output
        ",
    )
    .unwrap();

    let mut tests = fs::read_dir(tests_folder)
        .unwrap()
        .filter_map(|entry| {
            let entry = match entry {
                Ok(entry) if !is_hidden(&entry) => entry,
                _ => return None,
            };

            let path = entry.path();
            let file_name = entry.file_name().to_string_lossy().into_owned();

            let code = fs::read_to_string(&path).unwrap();
            let lc = line_col::LineColLookup::new(&code);

            let tests = regex
                .find_iter(&code)
                .map(|r#match| {
                    let path = path.clone();

                    let r#match = r#match.unwrap();
                    let (line, _) = lc.get(r#match.start());

                    let captures = regex.captures(r#match.as_str()).unwrap().unwrap();
                    let capture = |name| captures.name(name).unwrap().as_str().trim().to_string();

                    let name = capture("name");
                    let code = capture("code");
                    let expected_output = capture("output");

                    Test {
                        name,
                        line,
                        run: Box::new(move || test(path.clone(), code, expected_output)),
                    }
                })
                .collect::<Vec<_>>();

            Some(TestFile {
                path,
                name: file_name,
                tests,
            })
        })
        .collect::<Vec<_>>();

    tests.sort_by(|a, b| a.name.cmp(&b.name));

    tests
}

fn test(file: PathBuf, code: String, expected_output: String) -> TestResult {
    let start = Instant::now();

    let output =
        run_and_collect_output(&code, Some(file.file_name().unwrap().into()), |_, _| Ok(()))
            .into_iter()
            .map(|o| o.to_string())
            .collect::<Vec<_>>()
            .join("\n");

    let duration = Instant::now().duration_since(start);

    if output == expected_output {
        TestResult::Passed { duration }
    } else {
        TestResult::Failed {
            duration,
            expected: expected_output,
            found: output,
        }
    }
}

fn is_hidden(entry: &fs::DirEntry) -> bool {
    entry
        .file_name()
        .to_str()
        .map(|s| s.starts_with('.'))
        .unwrap_or(true)
}
