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
pub use ref_thread_local::{self, *};

pub fn setup() {
    let env = env::global();
    *env.borrow_mut() = env::blank();

    builtins::setup(&mut env.borrow_mut());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn f() {
        let a_trait = Trait::new(Validation::of::<Number>());

        let a_value = Value::new(a_trait.clone(), Value::of(Number::new(42.0)));

        assert!(
            Validation::for_trait(a_trait)(a_value, &env::global(), &Stack::new())
                .unwrap()
                .is_valid()
        );
    }
}
