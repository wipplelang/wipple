use crate::*;

#[derive(TypeInfo, Clone)]
pub enum Trait {
    Primitive(DynamicType),
    Runtime(Id, Pattern),
}

impl Trait {
    pub fn new(pattern: Pattern) -> Self {
        Trait::Runtime(Id::new(), pattern)
    }

    pub fn of<T: TypeInfo>() -> Self {
        Trait::Primitive(DynamicType::of::<T>())
    }
}

impl Trait {
    pub fn id(&self) -> Id {
        match self {
            Trait::Primitive(r#type) => Id::Primitive(*r#type),
            Trait::Runtime(id, _) => *id,
        }
    }

    pub fn pattern(&self) -> Pattern {
        match self {
            Trait::Primitive(_) => Pattern::for_trait(self.clone()),
            Trait::Runtime(_, pattern) => pattern.clone(),
        }
    }
}

impl PartialEq for Trait {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Trait::Primitive(a), Trait::Primitive(b)) => a == b,
            (Trait::Runtime(a, _), Trait::Runtime(b, _)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Trait {}

impl std::fmt::Debug for Trait {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Trait({:?})", self.id())
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

        self.derive(r#trait, env, stack)
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
