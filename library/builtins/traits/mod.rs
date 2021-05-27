mod block;
mod comparison;
mod empty;
mod evaluate;
mod functions;
mod list;
mod literal;
mod math;
mod module;
mod name;
mod number;
mod operator;
mod pattern;
mod reference;
mod text;
mod r#trait;
mod variant;

pub use block::*;
pub use comparison::*;
pub use empty::*;
pub use evaluate::*;
pub use functions::*;
pub use list::*;
pub use literal::*;
pub use math::*;
pub use module::*;
pub use name::*;
pub use number::*;
pub use operator::*;
pub use reference::*;
pub use text::*;
pub use variant::*;

use wipple::*;

#[ext(pub, name = EnvBuiltinsExt)]
impl Env {
    fn builtins(stack: &Stack) -> Result<Env> {
        Env::try_with(|env| {
            block::setup(env, stack)?;
            comparison::setup(env, stack)?;
            empty::setup(env, stack)?;
            evaluate::setup(env, stack)?;
            functions::setup(env, stack)?;
            list::setup(env, stack)?;
            math::setup(env, stack)?;
            module::setup(env, stack)?;
            name::setup(env, stack)?;
            number::setup(env, stack)?;
            operator::setup(env, stack)?;
            literal::setup(env, stack)?;
            text::setup(env, stack)?;
            r#trait::setup(env, stack)?;
            pattern::setup(env, stack)?;
            reference::setup(env, stack)?;
            variant::setup(env, stack)?;

            Ok(())
        })
    }
}
