use std::collections::HashMap;

use crate::*;
use wipple::*;

#[derive(TypeInfo, Clone, Default)]
struct LinkedValues(HashMap<String, Value>);

env_key!(linked_values: LinkedValues {
    visibility: EnvKeyVisibility::Private,
});

pub(crate) fn linked_value(name: &str) -> Value {
    Env::global()
        .linked_values()
        .0
        .remove(name)
        .unwrap_or_else(|| panic!("'{}' is not linked", name))
}

pub(crate) fn set_linked_value(name: String, value: Value) {
    Env::global().update_linked_values(|linked_values| linked_values.0.insert(name, value));
}

// Booleans

#[ext(pub, name = VariantConditionExt)]
impl Variant {
    fn condition(b: bool) -> Self {
        linked_value(if b { "True" } else { "False" })
            .primitive()
            .cast::<Variant>()
            .clone()
    }
}

#[ext(pub, name = VariantSetConditionExt)]
impl VariantSet {
    fn condition() -> Self {
        linked_value("Condition")
            .primitive()
            .cast::<VariantSet>()
            .clone()
    }
}

// Maybes

#[ext(pub, name = VariantMaybeExt)]
impl Variant {
    fn maybe(value: Option<Value>) -> Self {
        match value {
            Some(value) => linked_value("Some")
                .call_with(value, &Env::new(), &Stack::default())
                .expect("Call to 'Some' should never fail")
                .into_primitive()
                .into_cast::<Variant>(),
            None => linked_value("None").primitive().cast::<Variant>().clone(),
        }
    }
}

#[ext(pub, name = VariantSetMaybeExt)]
impl VariantSet {
    fn maybe() -> Self {
        linked_value("Maybe")
            .primitive()
            .cast::<VariantSet>()
            .clone()
    }
}
