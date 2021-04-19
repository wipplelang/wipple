#[macro_use]
mod macros;
pub use macros::*;

mod builtins;
mod fundamentals;
mod utils;

pub use builtins::*;
pub use fundamentals::*;
pub use utils::*;

pub use dynamic::{self, *};

pub fn setup() {
    let env = Environment::global();
    *env.borrow_mut() = Environment::blank();

    builtins::setup(&mut env.borrow_mut());
}
