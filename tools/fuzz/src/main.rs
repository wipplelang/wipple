mod compiling;
mod parsing;

use clap::Parser;
use std::{mem, process::ExitCode};
use wipple_frontend::helpers::Shared;

#[derive(Debug, clap::Parser)]
struct Args {
    #[clap(long, arg_enum)]
    test: Test,

    #[clap(long)]
    limit: Option<usize>,
}

#[derive(Debug, Clone, clap::ArgEnum)]
enum Test {
    Parsing,
    Compiling,
}

fn main() -> ExitCode {
    let args = Args::parse();

    match args.test {
        Test::Parsing => fuzz(parsing::fuzz, args.limit),
        Test::Compiling => fuzz(compiling::fuzz, args.limit),
    }
}

fn fuzz(f: fn(), limit: Option<usize>) -> ExitCode {
    let backtrace = Shared::new(None);

    std::panic::set_hook({
        let backtrace = backtrace.clone();
        Box::new(move |_| {
            *backtrace.lock() = Some(backtrace::Backtrace::new());
        })
    });

    let mut iteration: usize = 1;

    loop {
        clearscreen::clear().ok();
        println!("Iteration #{}", iteration);

        if let Err(error) = std::panic::catch_unwind(f) {
            if let Some(msg) = error
                .downcast_ref::<&str>()
                .copied()
                .or_else(|| error.downcast_ref::<String>().map(|s| s.as_str()))
            {
                println!("Crash detected: {}", msg);
            } else {
                println!("Crash detected");
            }

            if let Some(backtrace) = mem::take(&mut *backtrace.lock()) {
                println!("{:?}", backtrace);
            }

            return ExitCode::FAILURE;
        }

        iteration += 1;

        if let Some(limit) = limit {
            if iteration > limit {
                break;
            }
        }
    }

    ExitCode::SUCCESS
}
