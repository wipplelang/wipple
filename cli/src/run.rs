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

    /// Path to the program
    pub path: Option<PathBuf>,
}

impl Run {
    pub fn run(&self) -> wipple::Result<()> {
        let env = Environment::global();
        let stack = Stack::empty();

        wipple::setup();
        wipple_projects::setup();

        // Load the standard library
        (*wipple_stdlib::_wipple_plugin(&env, stack.clone()))?;

        let stack = setup(stack);

        match &self.evaluate_string {
            Some(code) => {
                let (tokens, lookup) = lex(&code);

                let ast = parse_inline_program(&mut tokens.iter().peekable(), &lookup).map_err(
                    |error| {
                        wipple::ReturnState::Error(wipple::Error::new(
                            &format!("Error parsing: {}", error.message),
                            stack.clone(),
                        ))
                    },
                )?;

                let program = convert(&ast, None);

                let result = program.evaluate(&env, stack.clone())?;

                println!("{}", result.try_format(&env, stack));
            }
            None => {
                let current_dir = self
                    .path
                    .clone()
                    .unwrap_or_else(|| std::env::current_dir().unwrap());

                match &self.path {
                    Some(path) if !path.is_dir() => import_path(path, stack)?,
                    _ => load_project(&current_dir.join("project.wpl"), stack)?,
                };
            }
        }

        Ok(())
    }
}

fn setup(stack: Stack) -> Stack {
    stack.with_show(ShowFn::new(move |value, env, stack| {
        println!(
            "{}",
            value.evaluate(env, stack.clone())?.format(env, stack)?
        );

        Ok(())
    }))
}
