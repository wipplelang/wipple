use std::{fmt::Write, path::Path, process};

datatest_stable::harness! {
    { test = test_file, root = "tests", pattern = r".*\.wipple$" },
}

fn test_file(path: &Path, source: String) -> datatest_stable::Result<()> {
    let file_name = path.file_name().unwrap().to_str().unwrap();

    let compiler_args = source
        .lines()
        .next()
        .and_then(|args| args.strip_prefix("-- wipple "))
        .expect("missing arguments comment");

    let compiler_args = compiler_args.replace("$FILE", &format!("compiler/tests/{file_name}"));

    let args = format!("cargo run --release --bin wipple --quiet -- {compiler_args}");

    let output = process::Command::new("/usr/bin/env")
        .args(args.split_ascii_whitespace())
        .stdin(process::Stdio::null())
        .stdout(process::Stdio::piped())
        .stderr(process::Stdio::piped())
        .env("CI", "1")
        .env("RUST_LIB_BACKTRACE", "0")
        .current_dir(Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap())
        .output()?;

    let status = output.status.code().unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();
    let stdout = String::from_utf8(output.stdout).unwrap();

    let mut output = String::new();
    writeln!(&mut output, "status: {}\n", status).unwrap();
    writeln!(&mut output, "stderr:\n{}", stderr).unwrap();
    writeln!(&mut output, "stdout:\n{}", stdout).unwrap();

    insta::with_settings!({
        snapshot_path => format!("{}/snapshots", env!("CARGO_MANIFEST_DIR")),
        prepend_module_to_snapshot => false,
    }, {
        insta::assert_snapshot!(file_name, output);
    });

    Ok(())
}
