use std::{fs, path::PathBuf};
use structopt::StructOpt;
use wipple::*;
use wipple_parser::*;

/// Run a Wipple program
#[derive(StructOpt)]
pub struct Run {
    /// Show evaluated output after running program (for debugging)
    #[structopt(short = "S", long = "show")]
    pub show_output: bool,

    /// Evaluate a string instead of a file as input
    #[structopt(name = "code", short = "e")]
    pub evaluate_string: Option<String>,

    /// Path to the program
    #[structopt(parse(from_os_str), default_value = "./project.wpl")]
    pub path: PathBuf,
}

impl Run {
    pub fn run(&self) -> wipple::Result<()> {
        let env = prelude().into_ref();
        let stack = Stack::new();

        let program = match &self.evaluate_string {
            Some(code) => {
                let ast = parse(code, &stack)?;
                convert(&ast, None)
            }
            None => {
                let code = fs::read_to_string(&self.path).map_err(|error| {
                    wipple::Error::new(&format!("Error loading file: {}", error), &stack)
                })?;

                let ast = parse(&code, &stack)?;
                convert(&ast, Some(&self.path))
            }
        };

        let result = program.evaluate(&env, &stack)?;

        if self.show_output {
            println!("{}", result.format(&env, &stack));
        }

        Ok(())
    }
}

fn parse(code: &str, stack: &Stack) -> wipple::Result<wipple_parser::AST> {
    wipple_parser::parse(&code)
        .map_err(|error| wipple::Error::new(&format!("Error parsing: {}", error.message), &stack))
}
