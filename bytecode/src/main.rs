use std::{
    error::Error,
    io::{self, Read, Write},
};
use structopt::StructOpt;
use wipple_bytecode as bytecode;

#[derive(StructOpt)]
#[structopt(about = "Manage Wipple bytecode files")]
enum Args {
    #[structopt(about = "Compile textual bytecode into binary")]
    Compile,

    #[structopt(about = "Decompile binary bytecode into textual")]
    Decompile,

    #[structopt(about = "Print a debug representation of binary bytecode")]
    Dump {
        #[structopt(long = "pretty", help = "Pretty-print the output")]
        pretty: bool,
    },
}

type Result<T> = core::result::Result<T, Box<dyn Error>>;

fn main() -> Result<()> {
    match Args::from_args() {
        Args::Compile => compile(),
        Args::Decompile => decompile(),
        Args::Dump { pretty } => dump(pretty),
    }
}

fn read_string() -> io::Result<String> {
    let mut s = String::new();
    io::stdin().read_to_string(&mut s)?;
    Ok(s)
}

fn compile() -> Result<()> {
    let code = read_string()?;
    let source = bytecode::Source::new(None, &code);

    let (text_file, error) = bytecode::text::File::parse(source);
    if let Some(error) = error {
        // TODO: Better error message
        eprintln!("Parse error: {:?}", error);
    }

    let (bin_file, errors) = bytecode::binary::File::compile(text_file);
    for error in errors {
        eprintln!("{}", error);
    }

    if let Some(bin_file) = bin_file {
        let bin = bincode::serialize(&bin_file)?;
        io::stdout().write_all(&bin)?;
    }

    Ok(())
}

fn decompile() -> Result<()> {
    let bin_file: bytecode::BinFile = bincode::deserialize_from(io::stdin())?;

    let text_file = bin_file.decompile();
    print!("{}", text_file);

    Ok(())
}

fn dump(pretty: bool) -> Result<()> {
    let bin_file: bytecode::BinFile = bincode::deserialize_from(io::stdin())?;

    if pretty {
        println!("{:#?}", bin_file);
    } else {
        println!("{:?}", bin_file);
    }

    Ok(())
}
