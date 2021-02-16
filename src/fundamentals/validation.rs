use crate::*;
use std::rc::Rc;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum ValidationResult<T> {
    Valid(T),
    Invalid,
}

pub use ValidationResult::*;

pub type AnyValidationResult = ValidationResult<Any>;

#[derive(Clone)]
pub struct Validation<A: 'static + Clone, B: 'static>(
    Rc<dyn Fn(A, &mut Environment, &ProgramStack) -> Result<ValidationResult<B>>>,
);

impl<A: Clone, B> Validation<A, B> {
    pub fn new(
        validate: impl Fn(A, &mut Environment, &ProgramStack) -> Result<ValidationResult<B>> + 'static,
    ) -> Validation<A, B> {
        Validation(Rc::new(validate))
    }

    pub fn validate(
        &self,
        value: A,
        env: &mut Environment,
        stack: &ProgramStack,
    ) -> Result<ValidationResult<B>> {
        (self.0)(value, env, stack)
    }

    pub fn or(self, other: Validation<A, B>) -> Validation<A, B> {
        Validation::new(move |input: A, env, stack| {
            match self.validate(input.clone(), env, stack)? {
                Valid(new_value) => Ok(Valid(new_value)),
                Invalid => other.validate(input, env, stack),
            }
        })
    }
}

impl<A: Clone, B: Clone> Validation<A, B> {
    pub fn and<C>(self, other: Validation<B, C>) -> Validation<A, C> {
        Validation::new(
            move |input, env, stack| match self.validate(input, env, stack)? {
                Valid(new_value) => other.validate(new_value, env, stack),
                Invalid => Ok(Invalid),
            },
        )
    }

    pub fn join<C>(self, other: Validation<B, C>) -> Validation<A, (B, C)> {
        Validation::new(
            move |input, env, stack| match self.validate(input, env, stack)? {
                Valid(new_value) => match other.validate(new_value.clone(), env, stack)? {
                    Valid(result) => Ok(Valid((new_value, result))),
                    Invalid => Ok(Invalid),
                },
                Invalid => Ok(Invalid),
            },
        )
    }
}

pub type AnyValidation = Validation<Any, Any>;

impl AnyValidation {
    pub fn from<A: Clone, B: Clone>(validation: Validation<A, B>) -> AnyValidation {
        AnyValidation::new(move |value, env, stack| {
            let value = value.cast::<A>().clone();

            let result = validation.validate(value, env, stack)?;

            Ok(match result {
                Valid(v) => Valid(Any::from(v)),
                Invalid => Invalid,
            })
        })
    }
}

impl<A: Clone, B: Clone> Validation<A, B> {
    pub fn from_any(validation: AnyValidation) -> Validation<A, B> {
        Validation::new(move |value, env, stack| {
            let erased_result = validation.validate(Any::from(value), env, stack)?;

            let result = match erased_result {
                Valid(erased_value) => {
                    let value = erased_value.cast::<B>().clone();
                    Valid(value)
                }
                Invalid => Invalid,
            };

            Ok(result)
        })
    }
}
