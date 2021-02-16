use crate::fundamentals::*;
use std::any::Any;
use std::rc::Rc;

#[derive(Clone)]
pub struct Conformance<A: 'static + Clone, B: 'static + Clone> {
    pub derived_trait_id: TraitID<B>,
    pub validation: Validation<Value, A>,
    pub derive_trait_value: Rc<dyn Fn(&A, &mut Environment, &ProgramStack) -> Result<B>>,
}

impl<A: Clone, B: Clone> Conformance<A, B> {
    pub fn new(
        derived_trait_id: TraitID<B>,
        validation: Validation<Value, A>,
        derive_trait_value: impl Fn(&A, &mut Environment, &ProgramStack) -> Result<B> + 'static,
    ) -> Conformance<A, B> {
        Conformance {
            derived_trait_id,
            validation,
            derive_trait_value: Rc::new(derive_trait_value),
        }
    }
}

#[derive(Clone)]
pub struct AnyConformance {
    pub derived_trait_id: AnyTraitID,
    pub validation: Validation<Value, Rc<dyn Any>>,
    pub derive_trait_value:
        Rc<dyn Fn(&dyn Any, &mut Environment, &ProgramStack) -> Result<Option<Rc<dyn Any>>>>,
}

impl AnyConformance {
    fn from<A: Clone, B: Clone>(conformance: Conformance<A, B>) -> AnyConformance {
        AnyConformance {
            derived_trait_id: AnyTraitID::from(conformance.derived_trait_id.clone()),
            validation: Validation::new({
                let conformance = conformance.clone();

                move |value, env, stack| {
                    let erased_result: ValidationResult<Rc<dyn Any>> =
                        match conformance.validation.validate(value, env, stack)? {
                            Valid(value) => Valid(Rc::new(value)),
                            Invalid => Invalid,
                        };

                    Ok(erased_result)
                }
            }),
            derive_trait_value: Rc::new(move |value, env, stack| {
                let value = match value.downcast_ref::<A>() {
                    Some(value) => value,
                    None => return Ok(None),
                };

                (conformance.derive_trait_value)(&value, env, &stack).map(|v| {
                    Some({
                        let v: Rc<dyn Any> = Rc::new(v);
                        v
                    })
                })
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
                            let value = erased_value.downcast_ref::<A>().unwrap().clone();
                            Valid(value)
                        }
                        Invalid => Invalid,
                    };

                    Ok(value)
                }
            }),
            move |value, env, stack| {
                let erased_value = (conformance.derive_trait_value)(value, env, stack)?.unwrap();
                let value = erased_value.downcast_ref::<B>().unwrap().clone();
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
