use crate::fundamentals::*;
use std::any::Any;
use std::rc::Rc;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum ValidationResult<T> {
    Valid(T),
    Invalid,
}

pub use ValidationResult::*;

pub type AnyValidationResult = ValidationResult<Rc<dyn Any>>;

#[derive(Clone)]
pub struct Validation<A: 'static + Clone, B: 'static>(
    Rc<dyn Fn(A, &mut Environment) -> Result<ValidationResult<B>>>,
);

impl<A: Clone, B> Validation<A, B> {
    pub fn new(
        validate: impl Fn(A, &mut Environment) -> Result<ValidationResult<B>> + 'static,
    ) -> Validation<A, B> {
        Validation(Rc::new(validate))
    }

    pub fn validate(&self, value: A, env: &mut Environment) -> Result<ValidationResult<B>> {
        (self.0)(value, env)
    }

    pub fn or(self, other: Validation<A, B>) -> Validation<A, B> {
        Validation::new(
            move |input: A, env| match self.validate(input.clone(), env)? {
                Valid(new_value) => Ok(Valid(new_value)),
                Invalid => other.validate(input, env),
            },
        )
    }
}

impl<A: Clone, B: Clone> Validation<A, B> {
    pub fn and<C>(self, other: Validation<B, C>) -> Validation<A, C> {
        Validation::new(move |input, env| match self.validate(input, env)? {
            Valid(new_value) => other.validate(new_value, env),
            Invalid => Ok(Invalid),
        })
    }
}

#[derive(Clone)]
pub struct AnyValidation(
    Rc<dyn Fn(&dyn Any, &mut Environment) -> Result<Option<AnyValidationResult>>>,
);

impl AnyValidation {
    pub fn new(
        validate: impl Fn(&dyn Any, &mut Environment) -> Result<Option<AnyValidationResult>> + 'static,
    ) -> AnyValidation {
        AnyValidation(Rc::new(validate))
    }

    pub fn from<A: Clone, B>(validation: Validation<A, B>) -> AnyValidation {
        AnyValidation::new(move |value, env| {
            let value = match value.downcast_ref::<A>() {
                Some(value) => value.clone(),
                None => return Ok(None),
            };

            let result = validation.validate(value, env)?;

            Ok(Some(match result {
                Valid(v) => Valid(Rc::new(v)),
                Invalid => Invalid,
            }))
        })
    }

    pub fn validate(
        &self,
        value: &dyn Any,
        env: &mut Environment,
    ) -> Result<Option<ValidationResult<Rc<dyn Any>>>> {
        (self.0)(value, env)
    }
}

impl<A: Clone, B: Clone> Validation<A, B> {
    pub fn from_any(validation: AnyValidation) -> Validation<A, B> {
        Validation::new(move |value, env| {
            let erased_result = validation.validate(&value, env)?.unwrap();

            let result = match erased_result {
                Valid(erased_value) => {
                    let value = erased_value.downcast_ref::<B>().unwrap().clone();
                    Valid(value)
                }
                Invalid => Invalid,
            };

            Ok(result)
        })
    }
}
