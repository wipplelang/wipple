mod block;
mod empty;
mod evaluate;
mod functions;
mod list;
mod literal;
mod module;
mod name;
mod number;
mod operator;
mod pattern;
mod text;
mod r#trait;
mod variants;

pub use block::*;
pub use evaluate::*;
pub use functions::*;
pub use list::*;
pub use literal::*;
pub use module::*;
pub use name::*;
pub use number::*;
pub use operator::*;
pub use text::*;
pub use variants::*;

use crate::*;

pub(crate) fn setup(env: &mut EnvironmentInner) {
    block::setup(env);
    empty::setup(env);
    evaluate::setup(env);
    functions::setup(env);
    list::setup(env);
    module::setup(env);
    name::setup(env);
    number::setup(env);
    operator::setup(env);
    literal::setup(env);
    text::setup(env);
    r#trait::setup(env);
    pattern::setup(env);
    variants::setup(env);
}
