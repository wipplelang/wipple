use colored::Colorize;
use std::path::PathBuf;
use structopt::StructOpt;
use wipple_projects::*;
use wipple_stdlib::*;

/// Run a Wipple program
#[derive(StructOpt)]
pub struct Run {
    /// Evaluate a string instead of a file as input
    #[structopt(name = "code", short = "e")]
    pub evaluate_string: Option<String>,

    /// Hide messages not produced by your program (errors are always shown)
    #[structopt(short = "q")]
    pub quiet: bool,

    /// Path to the program
    pub path: Option<PathBuf>,
}

impl Run {
    pub fn run(self) -> wipple::Result<()> {
        match self.evaluate_string {
            Some(code) => {
                run(&code, None, |_, _| Ok(()), |output| println!("{}", output));
                Ok(())
            }
            None => {
                let (_, mut stack) = default_setup(wipple_bundled_interpreter::handle_output)?;

                let current_dir = self
                    .path
                    .clone()
                    .unwrap_or_else(|| std::env::current_dir().unwrap());

                match self.path {
                    Some(path) if !path.is_dir() => {
                        import_path(path, &stack)?;
                        Ok(())
                    }
                    _ => {
                        let project = Project::from_file(current_dir.join("project.wpl"), &stack)?;

                        let install_path = dirs::cache_dir()
                            .expect("Could not resolve cache directory")
                            .join("wipple");

                        let dependencies = project.update_dependencies(
                            &install_path,
                            &|| {
                                if !self.quiet {
                                    println!("{}", "Updating dependencies".bright_black())
                                }
                            },
                            &stack,
                        )?;

                        stack.set_dependency_path(install_path);

                        let parsed_project = project.parse(dependencies);
                        parsed_project.register();
                        parsed_project.run(&stack)?;

                        Ok(())
                    }
                }
            }
        }
    }
}
