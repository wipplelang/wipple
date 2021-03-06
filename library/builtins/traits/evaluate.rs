use crate::*;
use wipple::*;

stored_closure!(pub struct EvaluateFn(&Env, &Stack) -> Result<Value>);

impl Primitive for EvaluateFn {}

#[ext(pub, name = ValueEvaluateExt)]
impl Value {
    fn evaluate(&self, env: &Env, stack: &Stack) -> Result<Cow<Value>> {
        Ok(match self.get_if_present::<EvaluateFn>(env, stack)? {
            Some(evaluate) => Cow::Owned(evaluate.0(env, stack)?),
            None => Cow::Borrowed(self),
        })
    }
}

#[allow(clippy::unnecessary_wraps)]
pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    env.set_variable(stack, "Evaluate", Value::of(Trait::of::<EvaluateFn>()))?;

    env.set_variable(
        stack,
        "evaluate",
        Value::of(Function::new(|value, env, stack| {
            Ok(value
                .evaluate(env, stack)?
                .evaluate(env, stack)?
                .into_owned())
        })),
    )?;

    Ok(())
}
