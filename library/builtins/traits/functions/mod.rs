mod closure;
mod template;

pub use closure::*;
pub use template::*;

use crate::*;
use wipple::*;

stored_closure!(struct FunctionCallFn(Value, &Env, &Stack) -> Result<Value>);

#[derive(TypeInfo, Debug, Clone)]
pub struct Function {
    call: FunctionCallFn,
    pub transparent: bool,
}

impl Primitive for Function {}

impl Function {
    pub fn new(call: impl Fn(Value, &Env, &Stack) -> Result<Value> + 'static) -> Self {
        Function {
            call: FunctionCallFn::new(call),
            transparent: false,
        }
    }

    /// Create a function that doesn't catch any 'return's inside it
    pub fn transparent(call: impl Fn(Value, &Env, &Stack) -> Result<Value> + 'static) -> Self {
        Function {
            call: FunctionCallFn::new(call),
            transparent: true,
        }
    }
}

impl std::ops::Deref for Function {
    type Target = dyn Fn(Value, &Env, &Stack) -> Result<Value>;

    fn deref(&self) -> &Self::Target {
        &*self.call
    }
}

#[ext(pub, name = ValueCallExt)]
impl Value {
    fn call_with(&self, parameter: Value, env: &Env, stack_: &Stack) -> Result<Value> {
        let mut stack = stack_.clone();
        stack
            .diagnostics_mut()
            .add(|| format!("Calling '{}'", self.format_with_fallback(env, &stack_)));

        let function = self.get_or::<Function>(
            "Cannot call this value because it does not have the Function trait",
            env,
            &stack,
        )?;

        if function.transparent {
            function(parameter, env, &stack)
        } else {
            catch!(Return in function(parameter, env, &stack))
        }
    }
}

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    closure::setup(env, stack)?;
    template::setup(env, stack)?;

    env.set_variable(stack, "Function", Value::of(Trait::of::<Function>()))?;

    Ok(())
}
