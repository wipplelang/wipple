mod compiling;
mod parsing;

use clap::Parser;
use std::{
    borrow::Borrow,
    fmt::Debug,
    mem,
    panic::RefUnwindSafe,
    sync::{atomic::AtomicUsize, Arc},
    thread,
};
use wipple_frontend::{analysis::expand, helpers::Shared};

#[derive(Debug, Clone, Copy, clap::Parser)]
struct Args {
    #[clap(long, arg_enum)]
    test: Test,

    #[clap(long)]
    limit: Option<usize>,

    #[clap(long)]
    quiet: bool,
}

#[derive(Debug, Clone, Copy, clap::ArgEnum)]
enum Test {
    Parsing,
    Compiling,
}

pub fn main() {
    let args = Args::parse();

    match args.test {
        Test::Parsing => fuzz::<String, _>(parsing::fuzz, args),
        Test::Compiling => fuzz::<expand::File, _>(compiling::fuzz, args),
    }
}

fn fuzz<T: Debug + RefUnwindSafe + Borrow<U> + for<'b> arbitrary::Arbitrary<'b>, U: ?Sized>(
    f: fn(&U),
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

        let result = std::panic::catch_unwind(|| {
            f(input.borrow());
        });

        break result.map_err(|e| (e, input));
    };

    let iteration = Arc::new(AtomicUsize::new(1));
    let record_iteration = move || {
        let iteration = iteration.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        if !args.quiet {
            clearscreen::clear().ok();
            println!("#{} iterations complete", iteration);
        }

        args.limit.map_or(true, |limit| iteration < limit)
    };

    let backtrace = Shared::new(None);
    std::panic::set_hook({
        let backtrace = backtrace.clone();
        Box::new(move |_| {
            *backtrace.lock() = Some(backtrace::Backtrace::new());
        })
    });

    let task = Arc::new(move || loop {
        if let Err((error, input)) = task() {
            let backtrace = match mem::take(&mut *backtrace.lock()) {
                Some(trace) => trace,
                None => return,
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
            println!("Input: {:#?}", input);

            std::process::exit(1);
        }

        if !record_iteration() {
            return;
        }
    });

    let num_threads = thread::available_parallelism().map_or(1, usize::from);

    if num_threads <= 1 {
        task();
    } else {
        thread::scope(|scope| {
            let threads = (0..num_threads)
                .map(|_| scope.spawn(task.as_ref()))
                .collect::<Vec<_>>();

            for thread in threads {
                thread.join().expect("uncaught panic");
            }
        })
    }
}
