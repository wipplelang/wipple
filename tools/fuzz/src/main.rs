#[cfg_attr(debug_assertions, path = "./debug/mod.rs")]
#[cfg_attr(not(debug_assertions), path = "./release/mod.rs")]
mod main;

fn main() {
    main::main()
}
