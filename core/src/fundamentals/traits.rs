use crate::*;

#[derive(TypeInfo, Clone)]
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
        write!(f, "Trait({:?})", self.id)
    }
}

impl Value {
    pub fn get_trait(&self, r#trait: &Trait, env: &EnvironmentRef, stack: &Stack) -> Result {
        self.get_trait_or(r#trait, "Value does not have this trait", env, stack)
    }

    pub fn is_trait(&self, r#trait: &Trait, env: &EnvironmentRef, stack: &Stack) -> Result<bool> {
        self.get_trait_if_present(r#trait, env, stack)
            .map(|t| t.is_some())
    }

    pub fn is_trait_directly(&self, r#trait: &Trait) -> bool {
        &self.r#trait == r#trait
    }

    pub fn get_trait_or(
        &self,
        r#trait: &Trait,
        message: &str,
        env: &EnvironmentRef,
        stack: &Stack,
    ) -> Result {
        self.get_trait_if_present(r#trait, env, stack)?
            .ok_or_else(|| Return::error(message, stack))
    }

    pub fn get_trait_if_present(
        &self,
        r#trait: &Trait,
        env: &EnvironmentRef,
        stack: &Stack,
    ) -> Result<Option<Value>> {
        if self.is_trait_directly(r#trait) {
            Ok(Some(self.contained_value().clone()))
        } else {
            fn derive(
                r#trait: &Trait,
                value: &Value,
                search_env: &EnvironmentRef,
                eval_env: &EnvironmentRef,
                stack: &Stack,
            ) -> Result<Option<Value>> {
                let conformances = search_env
                    .borrow_mut()
                    .conformances()
                    .clone()
                    .0
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
                    }
                    1 => {
                        let value = value.contained_value();

                        let derived_value = (conformances.first().unwrap().derive_value)(value, eval_env, stack)?;

                        let contained_value = match (r#trait.validation)(&derived_value, eval_env, stack)? {
                            Validated::Valid(value) => value,
                            Validated::Invalid => {
                                return Err(Return::error(
                                    "Cannot use this value to derive this trait",
                                    stack
                                ))
                            },
                        };

                        Ok(Some(contained_value))
                    }
                    _ => Err(Return::error(
                        // TODO: Better diagnostics -- list out all of the matching conformances
                        "Multiple conformances match this value, so the conformance to use is ambiguous",
                        stack,
                    )),
                }
            }

            derive(r#trait, self, env, env, stack)
        }
    }
}

impl Value {
    pub fn get<T: Primitive>(&self, env: &EnvironmentRef, stack: &Stack) -> Result<T> {
        self.get_or("Cannot find trait", env, stack)
    }

    pub fn get_or<T: Primitive>(
        &self,
        message: &str,
        env: &EnvironmentRef,
        stack: &Stack,
    ) -> Result<T> {
        self.get_if_present(env, stack)?
            .ok_or_else(|| Return::error(message, stack))
    }

    pub fn get_if_present<T: Primitive>(
        &self,
        env: &EnvironmentRef,
        stack: &Stack,
    ) -> Result<Option<T>> {
        Ok(self
            .get_trait_if_present(&Trait::of::<T>(), env, stack)?
            .map(|value| value.into_primitive::<T>()))
    }
}
