mod lowering;

use clap::Parser;
use std::mem;
use wipple_frontend::helpers::Shared;

#[derive(Debug, clap::Parser)]
struct Args {
    #[clap(long, arg_enum)]
    test: Test,
}

#[derive(Debug, Clone, clap::ArgEnum)]
enum Test {
    Parsing,
    Lowering,
}

fn main() {
    let args = Args::parse();

    match args.test {
        Test::Parsing => todo!(),
        Test::Lowering => fuzz(lowering::fuzz),
    }
}

fn fuzz(f: fn()) {
    let backtrace = Shared::new(None);

    std::panic::set_hook({
        let backtrace = backtrace.clone();
        Box::new(move |_| {
            *backtrace.lock() = Some(backtrace::Backtrace::new());
        })
    });

    let mut iteration = 0usize;

    loop {
        print!("\rIteration #{}", iteration);

        if let Err(error) = std::panic::catch_unwind(f) {
            println!();

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
        }

        iteration += 1;
    }
}
