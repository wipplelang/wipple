pub mod block;
pub mod empty;
pub mod evaluate;
pub mod function;
pub mod list;
pub mod r#macro;
pub mod name;
pub mod number;
pub mod operator;
pub mod quoted;
pub mod text;
pub mod trait_constructor;
pub mod validation_container;

pub use block::*;
pub use evaluate::*;
pub use function::*;
pub use list::*;
pub use name::*;
pub use number::*;
pub use operator::*;
pub use r#macro::*;
pub use text::*;
pub use trait_constructor::*;
pub use validation_container::*;

pub(crate) fn init(env: &mut crate::fundamentals::Environment) {
    block::init(env);
    empty::init(env);
    evaluate::init(env);
    list::init(env);
    r#macro::init(env);
    name::init(env);
    number::init(env);
    quoted::init(env);
    trait_constructor::init(env);
}
