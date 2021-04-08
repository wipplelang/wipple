use crate::*;

fn_wrapper_struct! {
    pub type Validation(&Value, &EnvironmentRef, Stack) -> Result<Validated<Value>>;
}

impl Validation {
    pub fn any() -> Self {
        Validation::new(|value, _, _| Ok(Validated::Valid(value.clone())))
    }

    pub fn of<T: Primitive>() -> Self {
        // We can't use Validation::for_trait here because it leads to infinite
        // recursion, as Trait::of calls Validation::of
        Validation::new(move |value, env, stack| {
            Ok(match value.get_primitive_if_present::<T>(env, stack)? {
                Some(primitive) => Validated::Valid(Value::of(primitive)),
                None => Validated::Invalid,
            })
        })
    }

    pub fn for_trait(r#trait: Trait) -> Self {
        Validation::new(move |value, env, stack| {
            Ok(match value.get_trait_if_present(&r#trait, env, stack)? {
                Some(value) => Validated::Valid(value),
                None => Validated::Invalid,
            })
        })
    }
}

#[derive(Debug)]
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
