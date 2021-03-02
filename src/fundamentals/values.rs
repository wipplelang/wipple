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
        match self {
            Value::Primitive(value) => vec![Trait {
                id: TraitID::Primitive(value.type_id),
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

    pub fn of<T: Primitive>(primitive: T) -> Self {
        Value::Primitive(Dynamic::new(primitive))
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Value::Primitive(_) => false,
            Value::Composite(traits) => traits.is_empty(),
        }
    }
}
