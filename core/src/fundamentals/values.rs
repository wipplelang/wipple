use crate::*;

pub trait Primitive: Clone + 'static {}

#[derive(Debug, Clone)]
pub enum Value {
    Primitive(Dynamic),
    Composite(Vec<Trait>),
}

impl Value {
    pub fn empty() -> Self {
        Value::Composite(vec![])
    }

    pub fn traits(&self) -> Vec<Trait> {
        match &self {
            Value::Primitive(value) => vec![Trait {
                id: TraitID::Primitive(value.type_info),
                value: self.clone(),
            }],
            Value::Composite(traits) => traits.clone(),
        }
    }

    pub fn add(&self, r#trait: &Trait) -> Self {
        let mut traits = self.traits();
        traits.push(r#trait.clone());
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
