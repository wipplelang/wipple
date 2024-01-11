//! Use the Wipple compiler from the command line.

use std::{env, fs};

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let path = args[1].to_string();
    let code = fs::read_to_string(&path).expect("failed to open file");

    let driver = wipple_driver::Driver::new();
    let files = vec![wipple_driver::File { path, code }];
    let result = driver.compile(files, Vec::new()); // TODO: Dependencies

    dbg!(result);
}
