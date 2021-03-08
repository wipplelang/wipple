use std::hash::{Hash, Hasher};

use crate::*;
use uuid::Uuid;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TraitID {
    Primitive(TypeInfo),
    Runtime(Uuid),
}

impl TraitID {
    pub fn new_primitive<T: Primitive>() -> Self {
        TraitID::Primitive(TypeInfo::of::<T>())
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

impl Trait {
    pub fn of<T: Primitive>(value: T) -> Trait {
        Trait {
            id: TraitID::Primitive(TypeInfo::of::<T>()),
            value: Value::of(value),
        }
    }
}

impl PartialEq for Trait {
    fn eq(&self, other: &Trait) -> bool {
        self.id == other.id
    }
}

impl Eq for Trait {}

impl Hash for Trait {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

fundamental_env_key!(is_deriving_from_conformance for bool {
    EnvironmentKey::new(
        UseFn::take_parent(),
        true,
    )
});

impl Value {
    pub fn get_trait(&self, id: TraitID, env: &EnvironmentRef, stack: &Stack) -> Result<Trait> {
        self.get_trait_or(id, "Cannot find trait", env, stack)
    }

    pub fn has_trait(&self, id: TraitID, env: &EnvironmentRef, stack: &Stack) -> Result<bool> {
        self.get_trait_if_present(id, env, stack)
            .map(|t| t.is_some())
    }

    pub fn get_trait_or(
        &self,
        id: TraitID,
        message: &str,
        env: &EnvironmentRef,
        stack: &Stack,
    ) -> Result<Trait> {
        self.get_trait_if_present(id, env, stack)?
            .ok_or_else(|| ReturnState::Error(Error::new(message, stack)))
    }

    pub fn get_trait_if_present(
        &self,
        id: TraitID,
        env: &EnvironmentRef,
        stack: &Stack,
    ) -> Result<Option<Trait>> {
        // Always use traits directly defined on the value if they exist instead
        // of deriving them
        if let Some(r#trait) = self.traits().into_iter().find(|t| t.id == id) {
            return Ok(Some(r#trait));
        }

        // Don't derive traits from conformances if we're already deriving
        // another trait — this can cause ambiguity (eg. A -> B -> C vs. A -> C)

        if *env.borrow_mut().is_deriving_from_conformance() {
            return Ok(None);
        }

        *env.borrow_mut().is_deriving_from_conformance() = true;

        // Attempt to derive the trait via a conformance

        let mut env = env.clone();

        loop {
            let conformances = env
                .borrow_mut()
                .conformances()
                .clone()
                .into_iter()
                .filter(|c| c.derived_trait_id == id);

            let mut derived_trait = None;

            for conformance in conformances {
                if conformance.derived_trait_id != id {
                    continue;
                }

                if let Some(derived_value) = (conformance.derive_trait_value)(self, &env, stack)? {
                    if derived_trait.is_some() {
                        return Err(ReturnState::Error(Error::new(
                            "Value satisfies multiple conformances, so the conformance to use is ambiguous",
                            stack
                        )));
                    }

                    derived_trait = Some(Trait {
                        id,
                        value: derived_value,
                    });
                }
            }

            *env.borrow_mut().is_deriving_from_conformance() = false;

            if let Some(derived_trait) = derived_trait {
                return Ok(Some(derived_trait));
            }

            let parent = env.borrow_mut().parent.clone();
            if let Some(parent) = parent {
                env = parent.clone();
            } else {
                return Ok(None);
            }
        }
    }
}

impl Value {
    pub fn get_primitive<T: Primitive>(&self, env: &EnvironmentRef, stack: &Stack) -> Result<T> {
        self.get_primitive_or("Cannot find trait", env, stack)
    }

    pub fn get_primitive_or<T: Primitive>(
        &self,
        message: &str,
        env: &EnvironmentRef,
        stack: &Stack,
    ) -> Result<T> {
        self.get_primitive_if_present(env, stack)?
            .ok_or_else(|| ReturnState::Error(Error::new(message, stack)))
    }

    pub fn get_primitive_if_present<T: Primitive>(
        &self,
        env: &EnvironmentRef,
        stack: &Stack,
    ) -> Result<Option<T>> {
        Ok(self
            .get_trait_if_present(TraitID::new_primitive::<T>(), env, stack)?
            .map(|r#trait| r#trait.value.cast_primitive::<T>()))
    }
}
