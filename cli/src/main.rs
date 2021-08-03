use std::{error::Error, fs, path::PathBuf};
use structopt::StructOpt;
use wipple_bytecode::linker::link_single;
use wipple_compiler::{compile_file, expression::Expression};
use wipple_interpreter::Interpreter;
use wipple_parser::{lex, parse_file};

#[derive(StructOpt)]
struct Args {
    file: PathBuf,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::from_args();

    let code = fs::read_to_string(&args.file)?;

    let (tokens, lc) = lex(&code);
    let ast = parse_file(&mut tokens.iter().peekable(), &lc)?;

    let expr = Expression::from_parser(ast, Some(&args.file));

    let (module, codemap, diagnostics) = compile_file(expr, |_| None);

    codemap_diagnostic::Emitter::stderr(codemap_diagnostic::ColorConfig::Auto, Some(&codemap))
        .emit(&diagnostics);

    let module = module.ok_or(CompileError)?;

    let object = link_single(module)?;

    let interpreter = Interpreter::new();
    interpreter.execute(object)?;

    Ok(())
}

#[derive(Debug)]
struct CompileError;

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("Compile error")
    }
}

impl std::error::Error for CompileError {}
