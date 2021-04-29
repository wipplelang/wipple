use colored::Colorize;
use std::process::exit;
use wipple_projects::{set_dependency_path_in, ParsedProject};

fn main() {
    exit(run())
}

fn run() -> i32 {
    let current_exe =
        std::fs::File::open(std::env::current_exe().expect("Could not locate current executable"))
            .expect("Could not open current executable");

    let tempdir = tempfile::tempdir()
        .expect("Could not create temporary folder to extract bundled data into");

    let mut zip = zip::ZipArchive::new(current_exe).expect("Could not load bundle");

    zip.extract(&tempdir).expect("Could not extract bundle");

    let dependency_path = tempdir.as_ref().join("dependencies");

    let project_file = std::fs::File::open(tempdir.as_ref().join("project.json"))
        .expect("Could not open bundled project manifest");

    let mut project: ParsedProject =
        serde_json::from_reader(project_file).expect("Could not parse bundled project manifest");

    project.path = tempdir.as_ref().join(project.path.clone());
    project.change_dependency_paths(&|path| dependency_path.join(path));

    let load = || -> wipple::Result<()> {
        let mut stack = wipple_bundled_interpreter::setup()?;
        set_dependency_path_in(&mut stack, &dependency_path);
        project.register();
        project.run(&stack)?;
        Ok(())
    };

    if let Err(error) = load() {
        eprintln!("{}", error.as_error().to_string().red());
        return 1;
    }

    0
}
