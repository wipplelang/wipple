use crate::*;
use wipple::*;

macro_rules! define {
    ($name:ident) => {
        #[derive(TypeInfo, Debug, Clone)]
        pub struct $name {
            pub value: Value,
            pub location: Option<SourceLocation>,
        }

        impl Primitive for $name {}

        impl $name {
            pub fn new(value: Value) -> Self {
                $name::new_located(value, None)
            }

            pub fn new_located(value: Value, location: Option<SourceLocation>) -> Self {
                $name { value, location }
            }
        }
    };
}

define!(Literal);
define!(Escaped);

stored_closure!(pub struct InterpolateFn(bool, &Env, &Stack) -> Result<Value>);

impl Primitive for InterpolateFn {}

#[ext(pub, name = ValueInterpolateExt)]
impl Value {
    // FIXME: Split into two different traits
    fn interpolate(&self, direct: bool, env: &Env, stack: &Stack) -> Result<Cow<Value>> {
        match self.get_if_present::<InterpolateFn>(env, stack)? {
            Some(interpolate) => interpolate(direct, env, stack).map(Cow::Owned),
            None if direct => self.evaluate(env, stack),
            None => Ok(Cow::Borrowed(self)),
        }
    }
}

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    env.set_variable(stack, "Literal", Value::of(Trait::of::<Literal>()))?;

    // Literal == Evaluate
    env.add_relation_between(stack, |literal: Literal| {
        EvaluateFn::new(move |env, stack| {
            literal
                .value
                .interpolate(false, env, stack)
                .map(Cow::into_owned)
        })
    })?;

    // Literal == Text
    env.add_relation_between_with(stack, {
        let env = env.clone();

        move |literal: Literal, stack| {
            let text = literal.value.format(&env, stack)?;
            Ok(Text::new(format!("'{}", text)))
        }
    })?;

    env.set_variable(
        stack,
        "literal",
        Value::of(Function::new(|value, _, _| Ok(value))),
    )?;

    env.set_variable(stack, "Escaped", Value::of(Trait::of::<Escaped>()))?;

    // Escaped == Evaluate
    env.add_relation_between(stack, |escaped: Escaped| {
        EvaluateFn::new(move |env, stack| {
            escaped
                .value
                .interpolate(true, env, stack)
                .map(Cow::into_owned)
        })
    })?;

    // Escaped == Interpolate
    env.add_relation_between(stack, |escaped: Escaped| {
        InterpolateFn::new(move |_, env, stack| {
            escaped.value.evaluate(env, stack).map(Cow::into_owned)
        })
    })?;

    // Escaped == Text
    env.add_relation_between_with(stack, {
        let env = env.clone();

        move |escaped: Escaped, stack| {
            let text = escaped.value.format(&env, stack)?;
            Ok(Text::new(format!("\\{}", text)))
        }
    })?;

    env.set_variable(
        stack,
        "Interpolate",
        Value::of(Trait::of::<InterpolateFn>()),
    )?;

    Ok(())
}
