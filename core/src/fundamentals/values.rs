use crate::*;

pub trait Primitive: TypeInfo {}

#[derive(Debug, Clone)]
pub struct Value {
    pub r#trait: Trait,
    contained: Contained,
}

#[derive(Debug, Clone)]
enum Contained {
    Primitive(Dynamic),
    Value(Box<Value>),
}

impl Value {
    pub fn new_validated(
        r#trait: Trait,
        value: Value,
        env: &EnvironmentRef,
        stack: &Stack,
    ) -> Result {
        let value = match (r#trait.validation)(&value, env, stack)? {
            Validated::Valid(value) => value,
            Validated::Invalid => {
                return Err(Return::error(
                    "Cannot use this value to represent this trait",
                    stack,
                ))
            }
        };

        Ok(Value::new_unvalidated(r#trait, value))
    }

    pub fn new_unvalidated(r#trait: Trait, value: Value) -> Self {
        Value {
            r#trait,
            contained: Contained::Value(Box::new(value)),
        }
    }

    pub fn primitive(r#trait: Trait, primitive: Dynamic) -> Self {
        Value {
            r#trait,
            contained: Contained::Primitive(primitive),
        }
    }

    pub fn of<T: Primitive>(primitive: T) -> Self {
        Value::primitive(Trait::of::<T>(), Dynamic::new(primitive))
    }

    pub fn into_primitive<T: Primitive>(self) -> T {
        match self.contained {
            Contained::Primitive(primitive) => primitive.into_cast::<T>(),
            Contained::Value(value) => value.into_primitive::<T>(), // handles nested primitives
        }
    }

    pub fn contained_value(&self) -> &Value {
        match &self.contained {
            Contained::Primitive(_) => self,
            Contained::Value(value) => value,
        }
    }
}
