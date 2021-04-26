use colored::Colorize;
use std::path::PathBuf;
use structopt::StructOpt;
use wipple::*;
use wipple_parser::*;
use wipple_projects::*;

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
    pub fn run(&self) -> std::result::Result<(), String> {
        let env = Environment::global();
        let mut stack = wipple_bundled_interpreter::setup()
            .map_err(|error| format!("Error initializing interpreter: {}", error))?;

        let convert_error = |error: Return| error.as_error().to_string();

        match &self.evaluate_string {
            Some(code) => {
                let (tokens, lookup) = lex(&code);

                let ast = parse_inline_program(&mut tokens.iter().peekable(), &lookup)
                    .map_err(|error| format!("Error parsing: {}", error.message))?;

                let program = convert(&ast, None);

                let result = program.evaluate(&env, &stack).map_err(convert_error)?;

                println!("{}", result.try_format(&env, &stack));
            }
            None => {
                let current_dir = self
                    .path
                    .clone()
                    .unwrap_or_else(|| std::env::current_dir().unwrap());

                match &self.path {
                    Some(path) if !path.is_dir() => {
                        import_path(path, &stack).map_err(convert_error)?;
                    }
                    _ => {
                        let project = Project::from_file(&current_dir.join("project.wpl"), &stack)
                            .map_err(convert_error)?;

                        let install_path = dirs::cache_dir()
                            .expect("Could not resolve cache directory")
                            .join("wipple");

                        let dependencies = project
                            .update_dependencies(
                                &install_path,
                                &|| {
                                    if !self.quiet {
                                        println!("{}", "Updating dependencies".bright_black())
                                    }
                                },
                                &stack,
                            )
                            .map_err(|error| format!("Error updating dependencies: {}", error))?;

                        set_dependency_path_in(&mut stack, &install_path);

                        let parsed_project = project.parse(dependencies);
                        parsed_project.register();
                        parsed_project.run(&stack).map_err(convert_error)?;
                    }
                };
            }
        }

        Ok(())
    }
}
