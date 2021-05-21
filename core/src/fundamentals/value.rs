use crate::*;

#[derive(Debug, Clone)]
pub enum Value {
    Primitive(Dynamic),
    Runtime(Trait, Box<Value>),
}

impl Value {
    pub fn new(r#trait: Trait, value: Value) -> Self {
        // Flatten primitives
        if let Id::Primitive(dt) = r#trait.id() {
            if value.is_primitive_of_type(dt) {
                return value;
            }
        }

        Value::Runtime(r#trait, Box::new(value))
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
            Value::Primitive(primitive) => match r#trait.id() {
                Id::Primitive(dt) => {
                    if primitive.r#type == dt {
                        Some(self)
                    } else {
                        None
                    }
                }
                _ => None,
            },
            Value::Runtime(stored_trait, value) => {
                if stored_trait == r#trait {
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
    pub(crate) fn r#trait(&self) -> Trait {
        match self {
            Value::Primitive(primitive) => Trait::Primitive(primitive.r#type),
            Value::Runtime(r#trait, _) => r#trait.clone(),
        }
    }

    pub(crate) fn stored_value(&self) -> &Value {
        match self {
            Value::Runtime(_, value) => value,
            _ => self,
        }
    }

    pub(crate) fn is_primitive_of_type(&self, r#type: DynamicType) -> bool {
        match self {
            Value::Primitive(primitive) => primitive.r#type == r#type,
            _ => false,
        }
    }
}
