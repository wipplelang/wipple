use colored::Colorize;
use std::process::exit;
use wipple::*;

fn main() {
    exit(run())
}

fn run() -> i32 {
    let current_exe =
        std::fs::File::open(std::env::current_exe().expect("Could not locate current executable"))
            .expect("Could not open current executable");

    let tempdir = tempfile::tempdir()
        .expect("Could not create temporary folder to extract bundled data into")
        .into_path();

    let mut zip = zip::ZipArchive::new(current_exe).expect("Could not load bundle");

    zip.extract(&tempdir).expect("Could not extract bundle");

    // SAFETY: This is the only location where this value is set, and it
    // always happens before it is read in 'wipple_projects::load_project'
    unsafe { wipple_projects::IS_RUNNING_BUNDLED = true };

    let load = || -> wipple::Result<()> {
        let stack = wipple_bundled_interpreter::setup()?;
        wipple_projects::load_project(&tempdir.join("project.wpl"), &stack)?;
        Ok(())
    };

    if let Err(error) = load() {
        eprintln!("{}", error.into_error(&Stack::new()).to_string().red());
        return 1;
    }

    0
}
