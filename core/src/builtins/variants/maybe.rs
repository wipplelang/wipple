use crate::*;
use std::collections::HashMap;

#[derive(Clone)]
struct MaybeVariantMarker;

impl Primitive for MaybeVariantMarker {}

fn maybe_variant() -> Module {
    Module::for_variant_of(ID::of::<MaybeVariantMarker>(), {
        let mut h = HashMap::new();
        h.insert(String::from("some"), vec![Validation::any()]);
        h.insert(String::from("none"), vec![]);
        h
    })
}

fn some_variant(value: Value) -> Variant {
    Variant::new(ID::of::<MaybeVariantMarker>(), "some", &[value])
}

fn none_variant() -> Variant {
    Variant::new(ID::of::<MaybeVariantMarker>(), "none", &[])
}

impl Value {
    pub fn some(value: Value) -> Self {
        Value::of(some_variant(value))
    }

    pub fn none() -> Self {
        Value::of(none_variant())
    }

    pub fn from_option(value: Option<Value>) -> Self {
        match value {
            Some(value) => Value::some(value),
            None => Value::none(),
        }
    }

    pub fn as_option_if_present(
        &self,
        env: &EnvironmentRef,
        stack: Stack,
    ) -> Result<Option<Option<Value>>> {
        let variant = match self.get_trait_if_present(ID::of::<MaybeVariantMarker>(), env, stack)? {
            Some(value) => value.cast_primitive::<Variant>(),
            _ => return Ok(None),
        };

        Ok(Some(match variant.name.as_str() {
            "some" => Some(variant.associated_values[0].clone()),
            "none" => None,
            _ => unreachable!(),
        }))
    }

    pub fn as_option(&self, env: &EnvironmentRef, stack: Stack) -> Result<Option<Value>> {
        self.as_option_if_present(env, stack.clone())?
            .ok_or_else(|| ReturnState::Error(Error::new("Expected Maybe", stack)))
    }
}

pub(crate) fn setup(env: &mut Environment) {
    let maybe_variant = maybe_variant();

    env.set_variable("maybe", Value::of(maybe_variant.clone()));

    env.set_variable(
        "Maybe",
        Value::of(TraitConstructor::of::<MaybeVariantMarker>()),
    );

    env.r#use(&maybe_variant.env.borrow());
}
