mod compiling;
mod parsing;
mod typechecking;

use clap::Parser;
use std::{fmt::Debug, mem, panic::AssertUnwindSafe};
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

    #[clap(long)]
    show_diagnostics: bool,
}

#[derive(Debug, Clone, Copy, clap::ArgEnum)]
enum Test {
    Parsing,
    Compiling,
    Typechecking,
}

fn main() {
    let args = Args::parse();

    match args.test {
        Test::Parsing => fuzz(parsing::fuzz, args),
        Test::Compiling => fuzz(compiling::fuzz, args),
        Test::Typechecking => fuzz(typechecking::fuzz, args),
    }
}

const DATA_SIZE: usize = 65536;

fn fuzz<T: Debug + for<'b> arbitrary::Arbitrary<'b>>(
    f: fn(T, Args) -> FinalizedDiagnostics,
    args: Args,
) {
    let backtrace = Shared::new(None);

    let task = {
        let backtrace = backtrace.clone();

        move || loop {
            let data = std::iter::repeat_with(rand::random)
                .take(DATA_SIZE)
                .collect::<Vec<u8>>();

            let mut u = arbitrary::Unstructured::new(&data);

            // Only proceed if the function was able to successfully generate an
            // `Arbitrary` value
            let input = match T::arbitrary(&mut u) {
                Ok(file) => file,
                Err(_) => continue,
            };

            std::panic::set_hook({
                let backtrace = backtrace.clone();
                Box::new(move |_| {
                    *backtrace.lock() = Some(backtrace::Backtrace::new());
                })
            });

            // SAFETY: `AssertUnwindSafe` is OK here because `f` does not rely on
            // global mutable state -- there is none in the compiler
            let result = std::panic::catch_unwind(AssertUnwindSafe(|| f(input, args)));

            let _ = std::panic::take_hook();

            break result;
        }
    };

    let mut iteration = 1;

    loop {
        if !args.quiet {
            clearscreen::clear().ok();
            println!("Iteration #{}", iteration);
        }

        match task() {
            Ok(diagnostics) => {
                let (files, diagnostics) = diagnostics.into_console_friendly(
                    #[cfg(debug_assertions)]
                    false,
                );

                if !args.quiet && args.show_diagnostics {
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
            }
            Err(error) => {
                let backtrace = match mem::take(&mut *backtrace.lock()) {
                    Some(trace) => trace,
                    None => unreachable!(),
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

        if args.interactive {
            println!("Press Enter to continue");
            std::io::stdin().read_line(&mut String::new()).ok();
        }
    }
}
