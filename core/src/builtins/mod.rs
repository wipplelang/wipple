mod block;
mod empty;
mod evaluate;
mod function;
mod list;
mod r#macro;
mod module;
mod name;
mod number;
mod operator;
mod quoted;
mod text;
mod r#trait;
mod validation;

pub use block::*;
pub use evaluate::*;
pub use function::*;
pub use list::*;
pub use module::*;
pub use name::*;
pub use number::*;
pub use operator::*;
pub use quoted::*;
pub use r#macro::*;
pub use r#trait::*;
pub use text::*;
pub use validation::*;

use crate::*;

pub(crate) fn setup(env: &mut Environment) {
    block::setup(env);
    evaluate::setup(env);
    empty::setup(env);
    list::setup(env);
    r#macro::setup(env);
    module::setup(env);
    name::setup(env);
    number::setup(env);
    operator::setup(env);
    quoted::setup(env);
    r#trait::setup(env);
    validation::setup(env);
}
