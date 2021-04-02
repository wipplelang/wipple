use crate::*;

#[derive(Clone)]
pub struct Trait {
    pub id: Id,
    pub validation: Validation,
}

impl Trait {
    pub fn new(validation: Validation) -> Self {
        Trait {
            id: Id::new(),
            validation,
        }
    }

    pub fn of<T: Primitive>() -> Self {
        Trait {
            id: Id::of::<T>(),
            validation: Validation::of::<T>(),
        }
    }
}

impl PartialEq for Trait {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Trait {}

impl std::fmt::Debug for Trait {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "(Trait {:?})", self.id)
    }
}

impl Value {
    pub fn get_trait(&self, r#trait: &Trait, env: &EnvironmentRef, stack: Stack) -> Result {
        self.get_trait_or(r#trait, "Value does not have this trait", env, stack)
    }

    pub fn has_trait(&self, r#trait: &Trait, env: &EnvironmentRef, stack: Stack) -> Result<bool> {
        self.get_trait_if_present(r#trait, env, stack)
            .map(|t| t.is_some())
    }

    pub fn has_trait_directly(&self, r#trait: &Trait) -> bool {
        &self.r#trait == r#trait
    }

    pub fn get_trait_or(
        &self,
        r#trait: &Trait,
        message: &str,
        env: &EnvironmentRef,
        stack: Stack,
    ) -> Result {
        self.get_trait_if_present(r#trait, env, stack.clone())?
            .ok_or_else(|| ReturnState::Error(Error::new(message, stack)))
    }

    pub fn get_trait_if_present(
        &self,
        r#trait: &Trait,
        env: &EnvironmentRef,
        stack: Stack,
    ) -> Result<Option<Value>> {
        if self.has_trait_directly(r#trait) {
            Ok(Some(self.contained_value().clone()))
        } else {
            fn derive(
                r#trait: &Trait,
                value: &Value,
                search_env: &EnvironmentRef,
                eval_env: &EnvironmentRef,
                stack: Stack,
            ) -> Result<Option<Value>> {
                let conformances = search_env
                    .borrow_mut()
                    .conformances()
                    .clone()
                    .into_iter()
                    .filter(|c| c.matches(value) && &c.derived_trait == r#trait)
                    .collect::<Vec<_>>();

                match conformances.len() {
                    0 => {
                        let parent = match &search_env.borrow().parent {
                            Some(parent) => parent.clone(),
                            None => return Ok(None),
                        };

                        derive(r#trait, value, &parent, eval_env, stack)
                    },
                    1 => (conformances.first().unwrap().derive_value)(value.contained_value(), eval_env, stack).map(Some),
                    _ => Err(ReturnState::Error(Error::new(
                        // TODO: Better diagnostics -- list out all of the matching conformances
                        "Multiple conformances match this value, so the conformance to use is ambiguous",
                        stack,
                    ))),
                }
            }

            derive(r#trait, self, env, env, stack)
        }
    }
}

impl Value {
    pub fn get_primitive<T: Primitive>(&self, env: &EnvironmentRef, stack: Stack) -> Result<T> {
        self.get_primitive_or("Cannot find trait", env, stack)
    }

    pub fn get_primitive_or<T: Primitive>(
        &self,
        message: &str,
        env: &EnvironmentRef,
        stack: Stack,
    ) -> Result<T> {
        self.get_primitive_if_present(env, stack.clone())?
            .ok_or_else(|| ReturnState::Error(Error::new(message, stack)))
    }

    pub fn get_primitive_if_present<T: Primitive>(
        &self,
        env: &EnvironmentRef,
        stack: Stack,
    ) -> Result<Option<T>> {
        Ok(self
            .get_trait_if_present(&Trait::of::<T>(), env, stack)?
            .map(|value| value.into_primitive::<T>()))
    }
}
