use crate::*;
use std::collections::HashMap;

#[derive(Clone)]
struct ResultVariantMarker;

impl Primitive for ResultVariantMarker {}

fn result_variant() -> Module {
    Module::for_variant_of(Id::of::<ResultVariantMarker>(), {
        let mut h = HashMap::new();
        h.insert(String::from("ok"), vec![Validation::any()]);
        // FIXME: Require errors conform to an Error trait
        h.insert(String::from("error"), vec![Validation::any()]);
        h
    })
}

fn ok_variant(value: Value) -> Variant {
    Variant::new(Id::of::<ResultVariantMarker>(), "ok", &[value])
}

fn error_variant(value: Value) -> Variant {
    Variant::new(Id::of::<ResultVariantMarker>(), "error", &[value])
}

impl Value {
    pub fn ok(value: Value) -> Self {
        Value::of(ok_variant(value))
    }

    pub fn error(value: Value) -> Self {
        Value::of(error_variant(value))
    }

    pub fn from_result(value: std::result::Result<Value, Value>) -> Self {
        match value {
            Ok(value) => Value::ok(value),
            Err(value) => Value::error(value),
        }
    }

    pub fn as_result_if_present(
        &self,
        env: &EnvironmentRef,
        stack: Stack,
    ) -> Result<Option<std::result::Result<Value, Value>>> {
        let variant =
            match self.get_trait_if_present(&Trait::of::<ResultVariantMarker>(), env, stack)? {
                Some(value) => value.into_primitive::<Variant>(),
                _ => return Ok(None),
            };

        Ok(Some(match variant.name.as_str() {
            "ok" => Ok(variant.associated_values[0].clone()),
            "error" => Err(variant.associated_values[0].clone()),
            _ => unreachable!(),
        }))
    }

    pub fn as_result(
        &self,
        env: &EnvironmentRef,
        stack: Stack,
    ) -> Result<std::result::Result<Value, Value>> {
        self.as_result_if_present(env, stack.clone())?
            .ok_or_else(|| ReturnState::Error(Error::new("Expected Result", stack)))
    }
}

pub(crate) fn setup(env: &mut Environment) {
    let result_variant = result_variant();

    env.set_variable("result", Value::of(result_variant.clone()));

    env.set_variable(
        "Result",
        Value::of(TraitConstructor::of::<ResultVariantMarker>()),
    );

    env.r#use(&result_variant.env.borrow());
}
