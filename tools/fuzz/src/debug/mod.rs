mod compiling;
mod parsing;

use clap::Parser;
use std::{fmt::Debug, mem, panic::UnwindSafe};
use wipple_frontend::{diagnostics::FinalizedDiagnostics, helpers::Shared};

#[derive(Debug, Clone, Copy, clap::Parser)]
struct Args {
    #[clap(long, arg_enum)]
    test: Test,

    #[clap(long)]
    limit: Option<usize>,

    #[clap(long)]
    quiet: bool,

    #[clap(long)]
    interactive: bool,
}

#[derive(Debug, Clone, Copy, clap::ArgEnum)]
enum Test {
    Parsing,
    Compiling,
}

pub fn main() {
    let args = Args::parse();

    match args.test {
        Test::Parsing => fuzz(parsing::fuzz, args),
        Test::Compiling => fuzz(compiling::fuzz, args),
    }
}

fn fuzz<T: Debug + UnwindSafe + for<'b> arbitrary::Arbitrary<'b>>(
    f: fn(T, bool) -> FinalizedDiagnostics,
    args: Args,
) {
    let task = move || loop {
        let data = std::iter::repeat_with(rand::random)
            .take(1024)
            .collect::<Vec<u8>>();

        let mut u = arbitrary::Unstructured::new(&data);

        // Only proceed if the function was able to successfully generate an
        // `Arbitrary` value
        let input = match T::arbitrary(&mut u) {
            Ok(file) => file,
            Err(_) => continue,
        };

        break std::panic::catch_unwind(|| f(input, args.quiet));
    };

    let mut iteration = 1;

    loop {
        if !args.quiet {
            println!("Iteration #{}", iteration);
        }

        let backtrace = Shared::new(None);
        std::panic::set_hook({
            let backtrace = backtrace.clone();
            Box::new(move |_| {
                *backtrace.lock() = Some(backtrace::Backtrace::new());
            })
        });

        let result = task();

        let _ = std::panic::take_hook();

        let has_diagnostics = match result {
            Ok(diagnostics) => {
                let (files, diagnostics) = diagnostics.into_console_friendly(false);
                let has_diagnostics = !diagnostics.is_empty();

                if !args.quiet {
                    let writer = codespan_reporting::term::termcolor::StandardStream::stdout(
                        codespan_reporting::term::termcolor::ColorChoice::Auto,
                    );

                    let config = codespan_reporting::term::Config::default();

                    for diagnostic in diagnostics {
                        codespan_reporting::term::emit(
                            &mut writer.lock(),
                            &config,
                            &files,
                            &diagnostic,
                        )
                        .unwrap();
                    }
                }

                has_diagnostics
            }
            Err(error) => {
                let backtrace = match mem::take(&mut *backtrace.lock()) {
                    Some(trace) => trace,
                    None => panic!(),
                };

                if let Some(msg) = error
                    .downcast_ref::<&str>()
                    .copied()
                    .or_else(|| error.downcast_ref::<String>().map(|s| s.as_str()))
                {
                    println!("Crash detected: {}", msg);
                } else {
                    println!("Crash detected");
                }

                println!("{:?}", backtrace);

                std::process::exit(1);
            }
        };

        iteration += 1;

        if args.limit.map_or(false, |limit| iteration > limit) {
            break;
        }

        if args.interactive && has_diagnostics {
            println!("Press Enter to continue");
            std::io::stdin().read_line(&mut String::new()).ok();
        }

        if !args.quiet {
            clearscreen::clear().ok();
        }
    }
}
