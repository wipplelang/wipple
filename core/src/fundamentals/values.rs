use crate::*;
use std::collections::HashSet;

pub trait Primitive: Clone + 'static {}

#[derive(Clone)]
pub enum Value {
    Primitive(Dynamic),
    Composite(HashSet<Trait>),
}

impl Value {
    pub fn empty() -> Self {
        Value::Composite(HashSet::new())
    }

    pub fn traits(&self) -> HashSet<Trait> {
        match &self {
            Value::Primitive(primitive) => {
                let mut traits = HashSet::new();

                let value = self.clone();

                traits.insert(Trait::new(
                    TraitID::Primitive(primitive.type_info),
                    move |_, _| Ok(value.clone()),
                ));

                traits
            }
            Value::Composite(traits) => traits.clone(),
        }
    }

    pub fn add(&self, r#trait: &Trait) -> Self {
        let mut traits = self.traits();
        traits.remove(r#trait);
        traits.insert(r#trait.clone());
        Value::Composite(traits)
    }

    pub fn new(r#trait: &Trait) -> Self {
        Value::empty().add(r#trait)
    }

    pub fn of(primitive: impl Primitive) -> Self {
        Value::Primitive(Dynamic::new(primitive))
    }

    pub fn is_empty(&self) -> bool {
        match &self {
            Value::Primitive(_) => false,
            Value::Composite(traits) => traits.is_empty(),
        }
    }

    pub fn cast_primitive<T: Primitive>(self) -> T {
        match self {
            Value::Primitive(value) => value.cast::<T>().clone(),
            _ => panic!("Value is not a primitive"),
        }
    }
}
