use crate::fundamentals::*;

type V = Validation<Value, Value>;

simple_trait! {
    name: validation_container,
    type: V,
    label: "Validation",
}
