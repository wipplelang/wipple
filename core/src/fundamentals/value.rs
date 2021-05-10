use crate::*;

#[derive(Debug, Clone)]
pub enum Value {
    Primitive(Dynamic),
    Runtime(Id, Box<Value>),
}

impl Value {
    pub fn new(r#trait: Trait, value: Value) -> Self {
        // Flatten primitives
        if let Id::Primitive(dt) = r#trait.id {
            if value.is_primitive_of_type(dt) {
                return value;
            }
        }

        Value::Runtime(r#trait.id, Box::new(value))
    }

    pub fn of<T: TypeInfo>(primitive: T) -> Self {
        Value::Primitive(Dynamic::new(primitive))
    }
}

impl Value {
    pub fn is_trait_directly(&self, r#trait: &Trait) -> bool {
        self.direct_value_for_trait(r#trait).is_some()
    }

    pub fn direct_value_for_trait(&self, r#trait: &Trait) -> Option<&Value> {
        match self {
            Value::Primitive(primitive) => match r#trait.id {
                Id::Primitive(dt) => {
                    if primitive.r#type == dt {
                        Some(self)
                    } else {
                        None
                    }
                }
                _ => None,
            },
            Value::Runtime(id, value) => {
                if id == &r#trait.id {
                    Some(value)
                } else {
                    None
                }
            }
        }
    }

    pub fn into_primitive<T: TypeInfo>(self) -> Option<T> {
        match self {
            Value::Primitive(primitive) => primitive.try_into_cast::<T>(),
            _ => None,
        }
    }
}

impl Value {
    fn is_primitive_of_type(&self, dt: DynamicType) -> bool {
        match self {
            Value::Primitive(primitive) => primitive.r#type == dt,
            _ => false,
        }
    }
}
