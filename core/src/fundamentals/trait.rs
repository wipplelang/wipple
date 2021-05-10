use crate::*;

#[derive(TypeInfo, Clone)]
pub struct Trait {
    pub id: Id,
    pub pattern: Pattern,
}

impl Trait {
    pub fn new(pattern: Pattern) -> Self {
        Trait {
            id: Id::new(),
            pattern,
        }
    }

    pub fn of<T: TypeInfo>() -> Self {
        Trait {
            id: Id::of::<T>(),
            pattern: Pattern::of::<T>(),
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
    pub fn get_trait(&self, r#trait: &Trait, env: &Environment, stack: &Stack) -> Result {
        self.get_trait_or(r#trait, "Value does not have this trait", env, stack)
    }

    pub fn is_trait(&self, r#trait: &Trait, env: &Environment, stack: &Stack) -> Result<bool> {
        self.get_trait_if_present(r#trait, env, stack)
            .map(|t| t.is_some())
    }

    pub fn get_trait_or(
        &self,
        r#trait: &Trait,
        message: &str,
        env: &Environment,
        stack: &Stack,
    ) -> Result {
        self.get_trait_if_present(r#trait, env, stack)?
            .ok_or_else(|| Return::error(message, stack))
    }

    pub fn get_trait_if_present(
        &self,
        r#trait: &Trait,
        env: &Environment,
        stack: &Stack,
    ) -> Result<Option<Value>> {
        if let Some(value) = self.direct_value_for_trait(r#trait) {
            return Ok(Some(value.clone()));
        }

        fn derive(
            r#trait: &Trait,
            value: &Value,
            conformances_env: &Environment,
            env: &Environment,
            stack: &Stack,
        ) -> Result<Option<Value>> {
            // Conformances declared in parent environments take precedence
            let parent_env = conformances_env.borrow().parent.clone();
            if let Some(parent_env) = parent_env {
                if let Some(derived_value) = derive(r#trait, value, &parent_env, env, stack)? {
                    return Ok(Some(derived_value));
                };
            }

            let conformances = conformances_env.borrow_mut().conformances().0.clone();

            // Conformances declared first have a higher precedence
            for conformance in conformances {
                if &conformance.derived_trait != r#trait {
                    continue;
                }

                if let Some(value) = (conformance.pattern)(value.clone(), env, stack)?.as_valid() {
                    let derived_value = (conformance.derive_value)(value.clone(), env, stack)?;

                    let trait_value = (r#trait.pattern)(derived_value, env, stack)?
                        .into_valid()
                        .ok_or_else(|| {
                            Return::error(
                                "Value derived from conformance cannot be used to represent the derived trait",
                                stack,
                            )
                        })?;

                    return Ok(Some(trait_value));
                }
            }

            Ok(None)
        }

        derive(r#trait, self, env, env, stack)
    }
}

impl Value {
    pub fn get<T: TypeInfo>(&self, env: &Environment, stack: &Stack) -> Result<T> {
        self.get_or("Cannot find trait", env, stack)
    }

    pub fn get_or<T: TypeInfo>(
        &self,
        message: &str,
        env: &Environment,
        stack: &Stack,
    ) -> Result<T> {
        self.get_if_present(env, stack)?
            .ok_or_else(|| Return::error(message, stack))
    }

    pub fn get_if_present<T: TypeInfo>(
        &self,
        env: &Environment,
        stack: &Stack,
    ) -> Result<Option<T>> {
        Ok(self
            .get_trait_if_present(&Trait::of::<T>(), env, stack)?
            .map(|value| value.into_primitive::<T>().unwrap()))
    }
}
