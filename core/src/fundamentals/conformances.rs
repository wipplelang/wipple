use crate::*;
use std::rc::Rc;

pub type Conformances = Vec<Conformance>;

fundamental_env_key!(pub conformances for Conformances {
    visibility: EnvironmentVisibility::Public(UseFn::from(|parent: &Conformances, new| {
        parent.clone().into_iter().chain(new.clone()).collect()
    })),
});

#[derive(Clone)]
pub struct Conformance {
    pub derived_trait_id: ID,
    pub validation: Validation,
    pub derive_trait_value: Rc<dyn Fn(&Value, &EnvironmentRef, Stack) -> Result>,
}

impl Environment {
    pub fn add_conformance(
        &mut self,
        derived_trait_id: ID,
        validation: Validation,
        derive_trait_value: impl Fn(&Value, &EnvironmentRef, Stack) -> Result + 'static,
    ) {
        self.conformances().push(Conformance {
            derived_trait_id,
            validation,
            derive_trait_value: Rc::new(derive_trait_value),
        })
    }

    pub fn add_primitive_conformance<A: Primitive, B: Primitive>(
        &mut self,
        derive_trait_value: impl Fn(A) -> B + 'static,
    ) {
        self.add_conformance(ID::of::<B>(), Validation::of::<A>(), move |value, _, _| {
            let a = value.clone().cast_primitive::<A>();
            let b = derive_trait_value(a);
            Ok(Value::of(b))
        });
    }

    pub fn add_text_conformance(&mut self, id: ID, value_name: &'static str) {
        self.add_conformance(
            ID::text(),
            Validation::new(move |value, env, stack| {
                Ok(if value.has_trait(id, env, stack)? {
                    Validated::Valid(value.clone())
                } else {
                    Validated::Invalid
                })
            }),
            move |value, env, stack| {
                let named = value.get_primitive_if_present::<Named>(env, stack)?;

                Ok(Value::of(Text::new(&match named {
                    Some(named) => format!("<{} '{}'>", value_name, named.name),
                    None => format!("<{}>", value_name),
                })))
            },
        );
    }
}
