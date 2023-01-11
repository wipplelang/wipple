#[cfg(not(debug_assertions))]
compile_error!("fuzzing requires debug_assertions to be enabled");

mod compiling;
mod parsing;

use clap::Parser;
use std::{
    borrow::Borrow,
    fmt::Debug,
    mem,
    panic::RefUnwindSafe,
    process::ExitCode,
    sync::{atomic::AtomicUsize, Arc},
    thread,
};
use wipple_frontend::{analysis::expand, helpers::Shared};

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
        Test::Parsing => fuzz::<String, _>(parsing::fuzz, args.limit),
        Test::Compiling => fuzz::<expand::File, _>(compiling::fuzz, args.limit),
    }
}

fn fuzz<T: Debug + RefUnwindSafe + Borrow<U> + for<'b> arbitrary::Arbitrary<'b>, U: ?Sized>(
    f: fn(&U),
    limit: Option<usize>,
) -> ExitCode {
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

        let result = std::panic::catch_unwind(|| {
            f(input.borrow());
        });

        break result.map_err(|e| (e, input));
    };

    let iteration = Arc::new(AtomicUsize::new(1));
    let record_iteration = move || {
        let iteration = iteration.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        clearscreen::clear().ok();
        println!("#{} iterations complete", iteration);

        limit.map_or(true, |limit| iteration < limit)
    };

    let backtrace = Shared::new(None);
    std::panic::set_hook({
        let backtrace = backtrace.clone();
        Box::new(move |_| {
            *backtrace.lock() = Some(backtrace::Backtrace::new());
        })
    });

    let task = Arc::new(move || loop {
        if backtrace.lock().is_some() {
            return Err(());
        }

        if let Err((error, input)) = task() {
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

            println!("Input:");
            println!("{:#?}", input);

            return Err(());
        }

        if !record_iteration() {
            return Ok(());
        }
    });

    let num_threads = thread::available_parallelism().map_or(1, usize::from);

    if num_threads <= 1 {
        match task() {
            Ok(()) => ExitCode::SUCCESS,
            Err(()) => ExitCode::FAILURE,
        }
    } else {
        thread::scope(|scope| {
            let threads = (0..num_threads)
                .map(|_| scope.spawn(task.as_ref()))
                .collect::<Vec<_>>();

            for thread in threads {
                let result = thread.join().expect("uncaught panic");
                if result.is_err() {
                    return ExitCode::FAILURE;
                }
            }

            ExitCode::SUCCESS
        })
    }
}
