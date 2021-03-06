use crate::*;
use std::rc::Rc;

pub type Conformances = Vec<Conformance>;

fundamental_env_key!(conformances for Conformances {
    EnvironmentKey::new(
        UseFn::new(|parent: &Conformances, new| {
            parent.clone().into_iter().chain(new.clone()).collect()
        }),
        true,
    )
});

#[derive(Clone)]
pub struct Conformance {
    pub derived_trait_id: TraitID,

    #[allow(clippy::type_complexity)]
    pub derive_trait_value: Rc<dyn Fn(&Value, &EnvironmentRef, &Stack) -> Result<Option<Value>>>,
}

impl Environment {
    pub fn add_conformance(
        &mut self,
        derived_trait_id: TraitID,
        derive_trait_value: impl Fn(&Value, &EnvironmentRef, &Stack) -> Result<Option<Value>> + 'static,
    ) {
        self.conformances().push(Conformance {
            derived_trait_id,
            derive_trait_value: Rc::new(derive_trait_value),
        })
    }

    pub fn add_conformance_for_primitive<T: Primitive>(
        &mut self,
        derived_trait_id: TraitID,
        derive_trait_value: impl Fn(T, &EnvironmentRef, &Stack) -> Result<Option<Value>> + 'static,
    ) {
        self.add_conformance(derived_trait_id, move |value, env, stack| {
            let primitive = match value.get_primitive_if_present::<T>(env, stack)? {
                Some(primitive) => primitive,
                None => return Ok(None),
            };

            derive_trait_value(primitive, env, stack)
        })
    }
}
