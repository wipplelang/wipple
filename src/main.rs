use std::io::{self, Read};
use wipple_bytecode_parser::{
    parser::{self, Parse},
    Statement,
};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let expr = atoms::Parser::new(&input).parse().expect("Failed to parse");

    let value =
        Statement::parse(&mut parser::parse_list(expr).unwrap().into_iter().peekable()).unwrap();

    println!("{:?}", value);
}
