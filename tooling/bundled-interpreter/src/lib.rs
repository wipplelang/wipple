use colored::Colorize;
use wipple_stdlib::*;

pub fn handle_output(output: RunOutput) {
    let (success, text) = output.into_components();

    if success {
        println!("{}", text);
    } else {
        eprintln!("{}", text.red());
    }
}
