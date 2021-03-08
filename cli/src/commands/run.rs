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

    /// Path to the program
    #[structopt(default_value = "./")]
    pub path: String,
}

impl Run {
    pub fn run(&self) -> wipple::Result<()> {
        wipple::setup();
        wipple_projects::setup();

        let env = Environment::child_of(&Environment::global()).into_ref();
        let stack = Stack::new();

        match &self.evaluate_string {
            Some(code) => {
                let ast = parse_inline_program(&code).map_err(|error| {
                    wipple::ReturnState::Error(wipple::Error::new(
                        &format!("Error parsing: {}", error.message),
                        &stack,
                    ))
                })?;

                let program = convert(&ast, None);

                let result = program.evaluate(&env, &stack)?;

                println!("{}", result.try_format(&env, &stack));
            }
            None => {
                set_project_root(
                    &mut env.borrow_mut(),
                    Some(std::env::current_dir().unwrap()),
                );

                import(&self.path, &env, &stack)?;
            }
        }

        Ok(())
    }
}
