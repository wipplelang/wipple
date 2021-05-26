use std::collections::HashMap;

use crate::*;
use wipple::*;

thread_local! {
    static LINKED_VALUES: Rc<RefCell<HashMap<String, Value>>> = Default::default();
}

pub(crate) fn get_linked_value(name: &str) -> Value {
    LINKED_VALUES
        .with(Clone::clone)
        .borrow()
        .get(name)
        .unwrap()
        .clone()
}

pub(crate) fn set_linked_value(name: String, value: Value) {
    LINKED_VALUES.with(Clone::clone).borrow_mut().insert(name, value);
}

// Booleans

#[ext(pub, name = VariantConditionExt)]
impl Variant {
    fn condition(b: bool) -> Self {
        get_linked_value(if b { "True" } else { "False" })
            .primitive()
            .unwrap()
            .cast::<Variant>()
            .clone()
    }
}

#[ext(pub, name = VariantSetConditionExt)]
impl VariantSet {
    fn condition() -> Self {
        get_linked_value("Condition")
            .primitive()
            .unwrap()
            .cast::<VariantSet>()
            .clone()
    }
}

// Maybes

#[ext(pub, name = VariantMaybeExt)]
impl Variant {
    fn maybe(value: Option<Value>) -> Self {
        match value {
            Some(value) => get_linked_value("Some")
                .call_with(value, &Env::new(), &Stack::default())
                .expect("Call to 'Some' should never fail")
                .into_primitive()
                .unwrap()
                .into_cast::<Variant>(),
            None => get_linked_value("None")
                .primitive()
                .unwrap()
                .cast::<Variant>()
                .clone(),
        }
    }
}

#[ext(pub, name = VariantSetMaybeExt)]
impl VariantSet {
    fn maybe() -> Self {
        get_linked_value("Maybe")
            .primitive()
            .unwrap()
            .cast::<VariantSet>()
            .clone()
    }
}
