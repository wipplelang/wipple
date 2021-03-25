mod new;
mod run;

use colored::Colorize;
use std::process::exit;
use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(name = "The Wipple interpreter", bin_name = "wipple", no_version)]
pub enum Args {
    Run(run::Run),
    New(new::New),
}

fn main() {
    exit(run())
}

fn run() -> i32 {
    let args = Args::from_args();

    match args {
        Args::Run(run) => {
            if let Err(state) = run.run() {
                eprintln!(
                    "{}",
                    state.into_error(wipple::Stack::empty()).to_string().red()
                );
                return 1;
            }
        }
        Args::New(new) => {
            if let Err(error) = new.run() {
                eprintln!("{}", error.red());
                return 1;
            }
        }
    }

    0
}
