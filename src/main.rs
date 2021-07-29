use std::{
    fs,
    path::{Path, PathBuf},
};
use structopt::StructOpt;
use wipple_bytecode as bytecode;

#[derive(StructOpt)]
#[structopt(about = "Manage Wipple bytecode files")]
enum Args {
    #[structopt(about = "Compile a .wplb textual bytecode file into a .wplc binary")]
    Compile {
        #[structopt(help = "Path to a .wplb file")]
        path: PathBuf,

        #[structopt(short = "o", help = "Path where the compiled output should be stored")]
        output: PathBuf,
    },

    #[structopt(about = "Decompile a .wplc binary into a .wplb textual bytecode file")]
    Decompile {
        #[structopt(help = "Path to a .wplc file")]
        path: PathBuf,

        #[structopt(
            short = "o",
            help = "Path where the decompiled output should be stored"
        )]
        output: PathBuf,
    },

    #[structopt(about = "Decode a .wplc binary and print a debug representation of its contents")]
    Dump {
        #[structopt(help = "Path to a .wplc file")]
        path: PathBuf,

        #[structopt(long = "pretty", help = "Pretty-print the output")]
        pretty: bool,
    },
}

fn main() {
    match Args::from_args() {
        Args::Compile { path, output } => compile(&path, &output),
        Args::Decompile { path, output } => decompile(&path, &output),
        Args::Dump { path, pretty } => dump(&path, pretty),
    }
}

fn compile(path: &Path, output: &Path) {
    let code = fs::read_to_string(path).unwrap();
    let source = bytecode::Source::new(Some(path), &code);

    match bytecode::text::File::parse(source).and_then(bytecode::binary::File::compile) {
        Ok(bin_file) => fs::write(output, bincode::serialize(&bin_file).unwrap()).unwrap(),
        Err(err) => eprintln!("{}", err),
    }
}

fn decompile(path: &Path, output: &Path) {
    let bin_file: bytecode::BinFile =
        bincode::deserialize_from(fs::File::open(path).unwrap()).unwrap();

    let text_file = bin_file.decompile();
    fs::write(output, text_file).unwrap();
}

fn dump(path: &Path, pretty: bool) {
    let bin_file: bytecode::BinFile =
        bincode::deserialize_from(fs::File::open(path).unwrap()).unwrap();

    if pretty {
        println!("{:#?}", bin_file);
    } else {
        println!("{:?}", bin_file);
    }
}
