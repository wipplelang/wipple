use crate::*;
use wipple::*;

#[derive(TypeInfo, Clone)]
pub struct Reference(pub Rc<RefCell<Value>>);

impl Primitive for Reference {}

impl Reference {
    pub fn new(value: Value) -> Self {
        Reference(Rc::new(RefCell::new(value)))
    }
}

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    env.set_variable("Ref", Value::of(Trait::of::<Reference>()));

    env.set_variable(
        "ref",
        Value::of(Function::new(|value, env, stack| {
            let value = value.evaluate(env, stack)?.into_owned();
            Ok(Value::of(Reference::new(value)))
        })),
    );

    env.set_variable(
        "get",
        Value::of(Function::new(|value, env, stack| {
            Ok(value
                .evaluate(env, stack)?
                .get_or::<Reference>("Expected reference", env, stack)?
                .0
                .borrow()
                .clone())
        })),
    );

    env.set_variable(
        "set!",
        Value::of(Function::new(|value, env, stack| {
            let set_value = value.evaluate(env, stack)?.into_owned();

            Ok(Value::of(Function::new(move |value, env, stack| {
                let value = value.evaluate(env, stack)?;
                let reference = value.get_or::<Reference>("Expected reference", env, stack)?;

                reference.0.replace(set_value.clone());

                Ok(Value::empty())
            })))
        })),
    );

    // Empty == Text
    env.add_text_relation::<Reference>("reference", stack)?;

    Ok(())
}
