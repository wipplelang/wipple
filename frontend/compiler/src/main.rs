use std::error::Error;
use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(about = "Compile standalone Wipple files into bytecode")]
struct Args {}

fn main() -> Result<(), Box<dyn Error>> {
    let _args = Args::from_args();

    println!("TODO");

    Ok(())
}
