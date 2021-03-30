use crate::*;

pub trait Primitive: Clone + 'static {}

#[derive(Clone)]
pub struct Value {
    /// ID for this specific value — persists across clones but does not persist
    /// in, for example, a situation like this:
    ///
    /// ```wipple
    /// x : 3 -- id "1"
    /// y : x -- id "1"
    /// z : Number for x -- id "2"
    /// ```
    pub id: Id,
    pub r#trait: Trait,
    pub raw_value: Dynamic,
}

impl Value {
    pub fn new(r#trait: Trait, raw_value: Dynamic) -> Self {
        Value {
            id: Id::new(),
            r#trait,
            raw_value,
        }
    }

    pub fn of<T: Primitive>(primitive: T) -> Self {
        Value::new(Trait::of::<T>(), Dynamic::new(primitive))
    }

    pub fn into_primitive<T: Primitive>(self) -> T {
        self.raw_value.cast::<T>().clone()
    }
}
