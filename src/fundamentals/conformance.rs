use crate::*;
use std::rc::Rc;

#[derive(Clone)]
pub struct Conformance<A: 'static + Clone, B: 'static + Clone> {
    pub derived_trait_id: TraitID<B>,
    pub validation: Validation<Value, A>,
    pub derive_trait_value: Rc<dyn Fn(A, &mut Environment, &ProgramStack) -> Result<B>>,
}

impl<A: Clone, B: Clone> Conformance<A, B> {
    pub fn new(
        derived_trait_id: TraitID<B>,
        validation: Validation<Value, A>,
        derive_trait_value: impl Fn(A, &mut Environment, &ProgramStack) -> Result<B> + 'static,
    ) -> Conformance<A, B> {
        Conformance {
            derived_trait_id,
            validation,
            derive_trait_value: Rc::new(derive_trait_value),
        }
    }
}

pub type AnyConformance = Conformance<Any, Any>;

impl AnyConformance {
    fn from<A: Clone, B: Clone>(conformance: Conformance<A, B>) -> AnyConformance {
        AnyConformance {
            derived_trait_id: AnyTraitID::from(conformance.derived_trait_id.clone()),
            validation: Validation::new({
                let conformance = conformance.clone();

                move |value, env, stack| {
                    let erased_result: ValidationResult<Any> =
                        match conformance.validation.validate(value, env, stack)? {
                            Valid(value) => Valid(Any::from(value)),
                            Invalid => Invalid,
                        };

                    Ok(erased_result)
                }
            }),
            derive_trait_value: Rc::new(move |value, env, stack| {
                (conformance.derive_trait_value)(value.cast::<A>().clone(), env, &stack)
                    .map(|v| Any::from(v))
            }),
        }
    }
}

impl<A: Clone, B: Clone> Conformance<A, B> {
    pub fn from_any(conformance: AnyConformance) -> Conformance<A, B> {
        Conformance::new(
            TraitID::from_any(conformance.clone().derived_trait_id),
            Validation::new({
                let conformance = conformance.clone();

                move |value, env, stack| {
                    let value = match conformance.validation.validate(value, env, stack)? {
                        Valid(erased_value) => {
                            let value = erased_value.cast::<A>().clone();
                            Valid(value)
                        }
                        Invalid => Invalid,
                    };

                    Ok(value)
                }
            }),
            move |value, env, stack| {
                let erased_value =
                    (conformance.derive_trait_value)(Any::from(value.clone()), env, stack)?;
                let value = erased_value.cast::<B>().clone();
                Ok(value)
            },
        )
    }
}

impl Environment {
    pub fn add_conformance<A: Clone, B: Clone>(&mut self, conformance: Conformance<A, B>) {
        self.conformances.push(AnyConformance::from(conformance));
    }
}
