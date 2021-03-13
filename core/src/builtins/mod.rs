mod block;
mod empty;
mod evaluate;
mod functions;
mod list;
mod module;
mod name;
mod number;
mod operator;
mod quoted;
mod show;
mod text;
mod r#trait;
mod validation;
mod variants;

pub use block::*;
pub use evaluate::*;
pub use functions::*;
pub use list::*;
pub use module::*;
pub use name::*;
pub use number::*;
pub use operator::*;
pub use quoted::*;
pub use r#trait::*;
pub use show::*;
pub use text::*;
pub use validation::*;
pub use variants::*;

use crate::*;

pub(crate) fn setup(env: &mut Environment) {
    block::setup(env);
    empty::setup(env);
    evaluate::setup(env);
    functions::setup(env);
    list::setup(env);
    module::setup(env);
    name::setup(env);
    number::setup(env);
    operator::setup(env);
    quoted::setup(env);
    show::setup(env);
    r#trait::setup(env);
    validation::setup(env);
    variants::setup(env);
}
