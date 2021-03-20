use crate::*;
use std::collections::HashMap;

#[derive(Clone)]
struct BooleanVariantMarker;

impl Primitive for BooleanVariantMarker {}

fn boolean_variant() -> Module {
    Module::for_variant_of(TraitID::of::<BooleanVariantMarker>(), {
        let mut h = HashMap::new();
        h.insert(String::from("true"), vec![]);
        h.insert(String::from("false"), vec![]);
        h
    })
}

impl Value {
    pub fn r#true() -> Self {
        let env = boolean_variant().env;
        env.borrow_mut().parent = Some(Environment::global());

        Value::of(Name::new("true"))
            .evaluate(&env, &Stack::new())
            .unwrap()
    }

    pub fn r#false() -> Self {
        let env = boolean_variant().env;
        env.borrow_mut().parent = Some(Environment::global());

        Value::of(Name::new("false"))
            .evaluate(&env, &Stack::new())
            .unwrap()
    }

    pub fn from_bool(value: bool) -> Self {
        if value {
            Value::r#true()
        } else {
            Value::r#false()
        }
    }

    pub fn as_bool_if_present(&self, env: &EnvironmentRef, stack: &Stack) -> Result<Option<bool>> {
        let variant =
            match self.get_trait_if_present(TraitID::of::<BooleanVariantMarker>(), env, stack)? {
                Some(value) => value.cast_primitive::<Variant>(),
                _ => return Ok(None),
            };

        Ok(Some(match variant.name.as_str() {
            "true" => true,
            "false" => false,
            _ => unreachable!(),
        }))
    }

    pub fn as_bool(&self, env: &EnvironmentRef, stack: &Stack) -> Result<bool> {
        self.as_bool_if_present(env, stack)?
            .ok_or_else(|| ReturnState::Error(Error::new("Expected Boolean", stack)))
    }
}

pub(crate) fn setup(env: &mut Environment) {
    let boolean_variant = boolean_variant();

    env.set_variable("boolean", Value::of(boolean_variant.clone()));

    env.set_variable(
        "Boolean",
        Value::of(TraitConstructor::of::<BooleanVariantMarker>()),
    );

    env.r#use(&boolean_variant.env.borrow());
}
