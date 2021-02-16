use crate::*;
use std::any::Any;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::rc::Rc;
use uuid::Uuid;

#[derive(Debug, Clone)]
pub enum TraitID<T> {
    Builtin(&'static str, PhantomData<T>),
    Runtime(Uuid, PhantomData<T>),
}

impl<T> TraitID<T> {
    fn eq_erased<U>(&self, other: &TraitID<U>) -> bool {
        use TraitID::*;

        match (self, other) {
            (Builtin(a, _), Builtin(b, _)) => a == b,
            (Runtime(a, _), Runtime(b, _)) => a == b,
            _ => false,
        }
    }
}

impl<T> PartialEq for TraitID<T> {
    fn eq(&self, other: &Self) -> bool {
        self.eq_erased(other)
    }
}

impl<T> Eq for TraitID<T> {}

impl<T> Hash for TraitID<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use TraitID::*;

        match self {
            Builtin(label, _) => label.hash(state),
            Runtime(id, _) => id.hash(state),
        }
    }
}

impl<T> TraitID<T> {
    pub const fn builtin(debug_label: &'static str) -> TraitID<T> {
        TraitID::Builtin(debug_label, PhantomData)
    }

    pub fn new() -> TraitID<T> {
        TraitID::Runtime(Uuid::new_v4(), PhantomData)
    }
}

pub type AnyTraitID = TraitID<Rc<dyn Any>>;

impl AnyTraitID {
    pub fn from<T>(id: TraitID<T>) -> AnyTraitID {
        use TraitID::*;

        match id {
            Builtin(label, _) => Builtin(label, PhantomData),
            Runtime(id, _) => Runtime(id, PhantomData),
        }
    }
}

impl<T> TraitID<T> {
    pub fn from_any(id: AnyTraitID) -> TraitID<T> {
        use TraitID::*;

        match id {
            Builtin(label, _) => Builtin(label, PhantomData),
            Runtime(id, _) => Runtime(id, PhantomData),
        }
    }
}

#[derive(Clone)]
pub struct Trait<T> {
    pub id: TraitID<T>,
    pub value: Rc<dyn Fn(&mut Environment, &ProgramStack) -> Result<T>>,
}

impl<T> Trait<T> {
    pub fn new(
        id: TraitID<T>,
        value: impl Fn(&mut Environment, &ProgramStack) -> Result<T> + 'static,
    ) -> Trait<T> {
        Trait {
            id,
            value: Rc::new(value),
        }
    }

    pub fn value(&self, env: &mut Environment, stack: &ProgramStack) -> Result<T> {
        (self.value)(env, stack)
    }

    fn eq_erased<U>(&self, other: &Trait<U>) -> bool {
        self.id.eq_erased(&other.id)
    }
}

impl<T> PartialEq for Trait<T> {
    fn eq(&self, other: &Self) -> bool {
        self.eq_erased(other)
    }
}

impl<T> Eq for Trait<T> {}

impl<T> Hash for Trait<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

pub type AnyTrait = Trait<Rc<dyn Any>>;

impl AnyTrait {
    pub fn from<T: 'static + Clone>(t: Trait<T>) -> AnyTrait {
        AnyTrait {
            id: AnyTraitID::from(t.id.clone()),
            value: Rc::new(move |env, stack| {
                let value = (t.value)(env, stack)?;
                Ok(Rc::new(value.clone()))
            }),
        }
    }
}

impl<T: 'static + Clone> Trait<T> {
    pub fn from_any(t: AnyTrait) -> Trait<T> {
        Trait {
            id: TraitID::from_any(t.clone().id),
            value: Rc::new(move |env, stack| {
                let erased_value = match (t.value)(env, stack) {
                    Ok(erased_value) => erased_value,
                    Err(error) => return Err(error),
                };

                let value = erased_value.downcast_ref::<T>().unwrap().clone();

                Ok(value)
            }),
        }
    }
}

impl Value {
    pub fn new<T: 'static + Clone>(t: Trait<T>) -> Value {
        Value::empty().add(t)
    }

    pub fn add<T: 'static + Clone>(self, t: Trait<T>) -> Value {
        let mut value = self;
        value.traits.insert(AnyTrait::from(t));
        value
    }
}

impl<T: Clone> TraitID<T> {
    pub fn validation(self) -> Validation<Value, T> {
        let id = self.clone();

        Validation::new(move |value: Value, env, stack| {
            let stack = stack.add(|| format!("Validating '{}'", value.format(env, stack)));

            let result = match value.get_trait_if_present(id.clone(), env, &stack)? {
                Some(t) => Valid(t),
                None => Invalid,
            };

            Ok(result)
        })
    }
}

macro_rules! diagnostic {
    ($self:ident, $env:ident, $stack:ident) => {
        let $stack = $stack.add(|| format!("Finding trait for '{}'", $self.format($env, $stack)));
    };
}

impl Value {
    pub fn get_trait<T: 'static + Clone>(
        &self,
        id: TraitID<T>,
        env: &mut Environment,
        stack: &ProgramStack,
    ) -> Result<T> {
        self.get_trait_or(id, "Cannot find trait", env, stack)
    }

    pub fn get_trait_or<T: 'static + Clone>(
        &self,
        id: TraitID<T>,
        error_message: &str,
        env: &mut Environment,
        stack: &ProgramStack,
    ) -> Result<T> {
        diagnostic!(self, env, stack);

        match self.find_trait(id, env, &stack)? {
            Some(t) => t.value(env, &stack),
            None => Err(ProgramError::new(error_message, &stack)),
        }
    }

    pub fn get_trait_if_present<T: 'static + Clone>(
        &self,
        id: TraitID<T>,
        env: &mut Environment,
        stack: &ProgramStack,
    ) -> Result<Option<T>> {
        diagnostic!(self, env, stack);

        match self.find_trait(id, env, &stack)? {
            Some(t) => t.value(env, &stack).map(Some),
            None => Ok(None),
        }
    }

    pub fn has_trait<T: 'static + Clone>(
        &self,
        id: TraitID<T>,
        env: &mut Environment,
        stack: &ProgramStack,
    ) -> Result<bool> {
        diagnostic!(self, env, stack);

        self.find_trait(id, env, &stack).map(|t| t.is_some())
    }

    fn find_trait<T: 'static + Clone>(
        &self,
        id: TraitID<T>,
        env: &mut Environment,
        stack: &ProgramStack,
    ) -> Result<Option<Trait<T>>> {
        if let Some(t) = self.traits.iter().find(|t| t.id.eq_erased(&id)) {
            return Ok(Some(Trait::from_any(t.clone())));
        }

        let mut derived_trait = None;

        for conformance in env.conformances.clone() {
            if !conformance.derived_trait_id.eq_erased(&id) {
                continue;
            }

            let erased_result = conformance.validation.validate(self.clone(), env, &stack)?;

            let validated_value = match erased_result {
                Valid(value) => value.clone(),
                Invalid => continue,
            };

            if derived_trait.is_some() {
                return Err(ProgramError::new("Value satisfies multiple conformances deriving this trait, so the trait to derive is ambiguous", &stack));
            }

            let captured_env = env.clone();

            derived_trait = Some(Trait::new(id.clone(), move |_, stack| {
                let mut captured_env = captured_env.clone();

                let erased_value =
                    (conformance.derive_trait_value)(&*validated_value, &mut captured_env, stack)?
                        .unwrap();

                let value = erased_value.downcast_ref::<T>().unwrap().clone();

                Ok(value)
            }));
        }

        Ok(derived_trait)
    }
}

#[macro_export]
macro_rules! simple_trait {
    { name: $name:ident, type: $type:ident, label: $label:expr, } => {
        impl TraitID<$type> {
            #[allow(non_upper_case_globals)]
            pub const $name: TraitID<$type> = TraitID::builtin($label);
        }

        impl<'a> Trait<$type> {
            pub fn $name(x: $type) -> Trait<$type> {
                Trait::new(TraitID::$name, move |_, _| Ok(x.clone()))
            }
        }
    };
}
