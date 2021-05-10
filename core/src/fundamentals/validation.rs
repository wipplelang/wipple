use crate::*;

fn_wrapper! {
    #[derive(TypeInfo)]
    pub struct Validation(Value, &Environment, &Stack) -> Result<Validated<Value>>;
}

impl Validation {
    pub fn any() -> Self {
        Validation::new(|value, _, _| Ok(Validated::Valid(value)))
    }

    pub fn for_empty_value() -> Self {
        Validation::new(|value, _, _| {
            Ok(if value.is_empty() {
                Validated::Valid(value)
            } else {
                Validated::Invalid
            })
        })
    }

    pub fn of<T: TypeInfo>() -> Self {
        Validation::new(move |value, env, stack| {
            // Defer the call to Trait::of to prevent infinite recursion when
            // creating the validation -- Trait::of calls Validation:of
            Ok(match value.get_if_present::<T>(env, stack)? {
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

    pub fn is(validation: Validation) -> Self {
        Validation::new(move |value, env, stack| {
            validation(value.clone(), env, stack).map(|result| match result {
                Validated::Valid(_) => Validated::Valid(value),
                Validated::Invalid => Validated::Invalid,
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
    pub fn is_valid(&self) -> bool {
        matches!(self, Validated::Valid(_))
    }

    pub fn as_valid(&self) -> Option<&T> {
        match self {
            Validated::Valid(value) => Some(value),
            Validated::Invalid => None,
        }
    }

    pub fn into_valid(self) -> Option<T> {
        match self {
            Validated::Valid(value) => Some(value),
            Validated::Invalid => None,
        }
    }
}

impl<T> From<Option<T>> for Validated<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(value) => Validated::Valid(value),
            None => Validated::Invalid,
        }
    }
}
