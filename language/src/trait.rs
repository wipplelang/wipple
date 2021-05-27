use crate::*;
use derivative::*;

#[derive(Derivative, TypeInfo, Debug, Clone)]
#[derivative(PartialEq, Eq)]
pub enum Trait {
    Primitive(DynamicType),
    Runtime(Id, #[derivative(PartialEq = "ignore")] Pattern),
}

impl Primitive for Trait {}

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
            Trait::Primitive(r#type) => Id::Type(*r#type),
            Trait::Runtime(id, _) => *id,
        }
    }

    pub fn pattern(&self) -> Cow<Pattern> {
        match self {
            Trait::Primitive(_) => Cow::Owned(Pattern::for_trait(self.clone())),
            Trait::Runtime(_, pattern) => Cow::Borrowed(pattern),
        }
    }
}

impl Value {
    pub fn get_trait(&self, r#trait: &Trait, env: &Env, stack: &Stack) -> Result<Cow<Value>> {
        self.get_trait_or(r#trait, "Value does not have this trait", env, stack)
    }

    pub fn is_trait(&self, r#trait: &Trait, env: &Env, stack: &Stack) -> Result<bool> {
        self.get_trait_if_present(r#trait, env, stack)
            .map(|t| t.is_some())
    }

    pub fn get_trait_or(
        &self,
        r#trait: &Trait,
        message: &str,
        env: &Env,
        stack: &Stack,
    ) -> Result<Cow<Value>> {
        self.get_trait_if_present(r#trait, env, stack)?
            .ok_or_else(|| error(message, stack))
    }

    pub fn get_trait_if_present(
        &self,
        r#trait: &Trait,
        env: &Env,
        stack: &Stack,
    ) -> Result<Option<Cow<Value>>> {
        if let Some(value) = self.direct_value_for(r#trait) {
            return Ok(Some(Cow::Borrowed(value)));
        }

        Ok(self.derive(r#trait, env, stack)?.map(Cow::Owned))
    }
}

impl Value {
    pub fn get<T: TypeInfo>(&self, env: &Env, stack: &Stack) -> Result<Cow<T>> {
        self.get_or("Value does not have this trait", env, stack)
    }

    pub fn get_or<T: TypeInfo>(&self, message: &str, env: &Env, stack: &Stack) -> Result<Cow<T>> {
        self.get_if_present(env, stack)?
            .ok_or_else(|| error(message, stack))
    }

    pub fn get_if_present<T: TypeInfo>(&self, env: &Env, stack: &Stack) -> Result<Option<Cow<T>>> {
        Ok(self
            .get_trait_if_present(&Trait::of::<T>(), env, stack)?
            .map(|value| match value {
                Cow::Owned(value) => Cow::Owned(value.into_primitive().into_cast()),
                Cow::Borrowed(value) => Cow::Borrowed(value.primitive().cast()),
            }))
    }
}
