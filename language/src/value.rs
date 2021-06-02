use crate::*;

pub trait Primitive: TypeInfo {}

#[derive(TypeInfo, Debug, Clone)]
pub enum Value {
    Primitive(Any),
    Runtime(Trait, Box<Value>),
}

impl Value {
    pub fn new(r#trait: Trait, value: Value) -> Self {
        // Flatten primitive values
        match r#trait {
            Trait::Primitive(r#type) if value.is_primitive_of(r#type) => value,
            _ => Value::Runtime(r#trait, Box::new(value)),
        }
    }

    pub fn of<T: Primitive>(value: T) -> Self {
        Value::Primitive(value.into())
    }
}

impl Value {
    pub fn r#trait(&self) -> Cow<Trait> {
        match self {
            Value::Primitive(primitive) => Cow::Owned(Trait::Primitive(primitive.dynamic_type())),
            Value::Runtime(r#trait, _) => Cow::Borrowed(r#trait),
        }
    }

    pub fn direct_value_for(&self, r#trait: &Trait) -> Option<&Value> {
        if self.r#trait().as_ref() == r#trait {
            Some(self.stored_value())
        } else {
            None
        }
    }

    pub fn is_primitive_of(&self, r#type: DynamicType) -> bool {
        match self {
            Value::Primitive(primitive) => primitive.dynamic_type() == r#type,
            _ => false,
        }
    }

    pub fn try_primitive(&self) -> Option<&Any> {
        match self {
            Value::Primitive(primitive) => Some(primitive),
            _ => None,
        }
    }

    pub fn primitive(&self) -> &Any {
        match self {
            Value::Primitive(primitive) => primitive,
            _ => panic!("Not a primitive value"),
        }
    }

    pub fn try_into_primitive(self) -> Option<Any> {
        match self {
            Value::Primitive(primitive) => Some(primitive),
            _ => None,
        }
    }

    pub fn into_primitive(self) -> Any {
        match self {
            Value::Primitive(primitive) => primitive,
            _ => panic!("Not a primitive value"),
        }
    }
}

impl Value {
    pub(crate) fn stored_value(&self) -> &Value {
        match self {
            Value::Runtime(_, value) => value,
            _ => self,
        }
    }
}
