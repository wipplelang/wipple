use crate::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kind {
    Any,
    Child { id: Id, parent: Box<Kind> },
}

impl Kind {
    pub fn new_with_parent(parent: &Kind) -> Self {
        Kind::Child {
            id: Id::new(),
            parent: Box::new(parent.clone()),
        }
    }

    pub fn of_with_parent<T: 'static>(parent: &Kind) -> Self {
        Kind::Child {
            id: Id::of::<T>(),
            parent: Box::new(parent.clone()),
        }
    }
}

impl Kind {
    pub fn is_subset_of(&self, other: &Self) -> bool {
        use Kind::*;

        match (self, other) {
            (Any, Any) => true,
            (Any, Child { .. }) => true,
            (Child { .. }, Any) => false,
            (
                Child {
                    id: a_id,
                    parent: a_parent,
                },
                Child {
                    id: b_id,
                    parent: b_parent,
                },
            ) => a_id == b_id || a_parent.is_subset_of(b_parent),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub id: Id,
    pub kind: Kind,
}

impl PartialEq for Trait {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Trait {}

impl Trait {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Trait::new_with_kind(&Kind::Any)
    }

    pub fn new_with_kind(kind: &Kind) -> Self {
        Trait {
            id: Id::new(),
            kind: kind.clone(),
        }
    }

    pub fn of<T: 'static>() -> Self {
        Trait::of_with_kind::<T>(&Kind::Any)
    }

    pub fn of_with_kind<T: 'static>(kind: &Kind) -> Self {
        Trait {
            id: Id::of::<T>(),
            kind: kind.clone(),
        }
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
            Ok(Some(self.clone()))
        } else {
            self.derive(|c| &c.derived_trait == r#trait, env, stack)
        }
    }
}

impl Value {
    pub fn get_kind(&self, kind: &Kind, env: &EnvironmentRef, stack: Stack) -> Result {
        self.get_kind_or(kind, "Value is not of this kind", env, stack)
    }

    pub fn is_kind(&self, kind: &Kind, env: &EnvironmentRef, stack: Stack) -> Result<bool> {
        self.get_kind_if_present(kind, env, stack)
            .map(|t| t.is_some())
    }

    pub fn has_kind_directly(&self, kind: &Kind) -> bool {
        self.r#trait.kind.is_subset_of(kind)
    }

    pub fn get_kind_or(
        &self,
        kind: &Kind,
        message: &str,
        env: &EnvironmentRef,
        stack: Stack,
    ) -> Result {
        self.get_kind_if_present(kind, env, stack.clone())?
            .ok_or_else(|| ReturnState::Error(Error::new(message, stack)))
    }

    pub fn get_kind_if_present(
        &self,
        kind: &Kind,
        env: &EnvironmentRef,
        stack: Stack,
    ) -> Result<Option<Value>> {
        if self.has_kind_directly(kind) {
            Ok(Some(self.clone()))
        } else {
            self.derive(|c| c.derived_trait.kind.is_subset_of(kind), env, stack)
        }
    }
}

impl Value {
    fn derive(
        &self,
        predicate: impl Fn(&Conformance) -> bool,
        env: &EnvironmentRef,
        stack: Stack,
    ) -> Result<Option<Value>> {
        fn derive(
            value: &Value,
            predicate: impl Fn(&Conformance) -> bool,
            search_env: &EnvironmentRef,
            eval_env: &EnvironmentRef,
            stack: Stack,
        ) -> Result<Option<Value>> {
            let conformances = search_env
                .borrow_mut()
                .conformances()
                .clone()
                .into_iter()
                .filter(|c| c.matches(value) && predicate(c))
                .collect::<Vec<_>>();

            match conformances.len() {
                0 => {
                    let parent = match &search_env.borrow().parent {
                        Some(parent) => parent.clone(),
                        None => return Ok(None),
                    };

                    derive(value, predicate, &parent, eval_env, stack)
                },
                1 => (conformances.first().unwrap().derive_value)(value, eval_env, stack).map(Some),
                _ => Err(ReturnState::Error(Error::new(
                    // TODO: Better diagnostics -- list out all of the matching conformances
                    "Multiple conformances match this value, so the conformance to use is ambiguous",
                    stack,
                ))),
            }
        }

        derive(self, predicate, env, env, stack)
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
