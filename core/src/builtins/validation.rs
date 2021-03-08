use crate::*;
use std::rc::Rc;

#[derive(Clone)]
pub struct Validation(
    #[allow(clippy::type_complexity)]
    pub  Rc<dyn Fn(&Value, &EnvironmentRef, &Stack) -> Result<Validated<Value>>>,
);

impl Validation {
    pub fn new(
        validation: impl Fn(&Value, &EnvironmentRef, &Stack) -> Result<Validated<Value>> + 'static,
    ) -> Self {
        Validation(Rc::new(validation))
    }

    pub fn any() -> Self {
        Validation::new(|value, _, _| Ok(Validated::Valid(value.clone())))
    }

    pub fn for_primitive<T: Primitive>() -> Self {
        Validation::new(move |value, env, stack| {
            Ok(match value.get_primitive_if_present::<T>(env, stack)? {
                Some(primitive) => Validated::Valid(Value::of(primitive)),
                None => Validated::Invalid,
            })
        })
    }

    pub fn for_primitive_with<T: Primitive>(
        validation: impl Fn(T, &EnvironmentRef, &Stack) -> Result<Validated<Value>> + 'static,
    ) -> Self {
        Validation::new(move |value, env, stack| {
            let primitive = match value.get_primitive_if_present::<T>(env, stack)? {
                Some(primitive) => primitive,
                None => return Ok(Validated::Invalid),
            };

            validation(primitive, env, stack)
        })
    }
}

pub enum Validated<T> {
    Valid(T),
    Invalid,
}

impl<T> Validated<T> {
    pub fn map<U>(self, transform: impl FnOnce(T) -> U) -> Validated<U> {
        use Validated::*;

        match self {
            Valid(value) => Valid(transform(value)),
            Invalid => Invalid,
        }
    }

    pub fn flat_map<U>(self, transform: impl FnOnce(T) -> Validated<U>) -> Validated<U> {
        use Validated::*;

        match self {
            Valid(value) => transform(value),
            Invalid => Invalid,
        }
    }
}

impl<T> From<Validated<T>> for Option<T> {
    fn from(validated: Validated<T>) -> Self {
        use Validated::*;

        match validated {
            Valid(value) => Some(value),
            Invalid => None,
        }
    }
}

fundamental_primitive!(validation for Validation);

pub(crate) fn setup(env: &mut Environment) {
    env.add_primitive_conformance("builtin 'Validation ::= Text'", |_: Validation| Text {
        text: String::from("<validation>"),
        location: None,
    });
}
