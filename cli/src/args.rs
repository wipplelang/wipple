use std::path::PathBuf;

use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "Wipple", bin_name = "wipple", no_version)]
pub enum Args {
    Run {
        #[structopt(long)]
        show_ast: bool,

        #[structopt(short = "S", long = "show")]
        show_output: bool,

        evaluate_string: Option<String>,

        #[structopt(parse(from_os_str), default_value = "./project.wpl")]
        path: PathBuf,
    },
}
