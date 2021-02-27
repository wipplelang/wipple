use crate::*;
use std::any::TypeId;
use uuid::Uuid;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TraitID {
    Primitive(TypeId),
    Runtime(Uuid),
}

impl TraitID {
    pub fn new_primitive<T: Primitive>() -> Self {
        TraitID::Primitive(TypeId::of::<T>())
    }

    pub fn new_runtime() -> Self {
        TraitID::Runtime(Uuid::new_v4())
    }
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub id: TraitID,
    pub value: Value,
}

impl Value {
    pub fn get_trait(&self, id: TraitID, env: &mut Environment, stack: &Stack) -> Result<Trait> {
        self.get_trait_or(id, "Cannot find trait", env, stack)
    }

    pub fn has_trait(&self, id: TraitID, env: &mut Environment, stack: &Stack) -> Result<bool> {
        self.get_trait_if_present(id, env, stack)
            .map(|t| t.is_some())
    }

    pub fn get_trait_or(
        &self,
        id: TraitID,
        message: &str,
        env: &mut Environment,
        stack: &Stack,
    ) -> Result<Trait> {
        self.get_trait_if_present(id, env, stack)?
            .ok_or_else(|| Error::new(message, stack))
    }

    pub fn get_trait_if_present(
        &self,
        id: TraitID,
        env: &mut Environment,
        stack: &Stack,
    ) -> Result<Option<Trait>> {
        let traits = self.traits();

        // Always use traits directly defined on the value if they exist instead
        // of deriving them
        for r#trait in traits {
            if r#trait.id == id {
                return Ok(Some(r#trait));
            }
        }

        // Attempt to derive the trait via a conformance
        for conformance in env.conformances.clone() {
            if conformance.derived_trait_id != id {
                continue;
            }

            if let Some(value) = (conformance.derive_trait_value)(self, env, stack)? {
                return Ok(Some(Trait { id, value }));
            }
        }

        Ok(None)
    }
}

impl Value {
    pub fn get_primitive<T: Primitive>(&self, env: &mut Environment, stack: &Stack) -> Result<T> {
        self.get_primitive_or("Cannot find trait", env, stack)
    }

    pub fn get_primitive_or<T: Primitive>(
        &self,
        message: &str,
        env: &mut Environment,
        stack: &Stack,
    ) -> Result<T> {
        self.get_primitive_if_present(env, stack)?
            .ok_or_else(|| Error::new(message, stack))
    }

    pub fn get_primitive_if_present<T: Primitive>(
        &self,
        env: &mut Environment,
        stack: &Stack,
    ) -> Result<Option<T>> {
        Ok(self
            .get_trait_if_present(TraitID::new_primitive::<T>(), env, stack)?
            .and_then(|r#trait| match r#trait.value {
                Value::Primitive(_, value) => value.downcast_ref::<T>().cloned(),
                _ => None,
            }))
    }
}
