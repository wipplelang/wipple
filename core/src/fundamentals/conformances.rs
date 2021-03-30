use crate::*;
use std::rc::Rc;

pub type Conformances = Vec<Conformance>;

fundamental_env_key!(pub conformances for Conformances {
    visibility: EnvironmentVisibility::Public(UseFn::from(|parent: &Conformances, new| {
        parent.clone().into_iter().chain(new.clone()).collect()
    })),
});

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConformanceMatch {
    Trait(Trait),
    Kind(Kind),
}

#[derive(Clone)]
pub struct Conformance {
    pub matching: ConformanceMatch,
    pub derived_trait: Trait,
    pub derive_value: Rc<dyn Fn(&Value, &EnvironmentRef, Stack) -> Result>,
}

impl Conformance {
    pub fn new(
        matching: ConformanceMatch,
        derived_trait: Trait,
        derive_value: impl Fn(&Value, &EnvironmentRef, Stack) -> Result + 'static,
    ) -> Self {
        Conformance {
            matching,
            derived_trait,
            derive_value: Rc::new(derive_value),
        }
    }

    pub fn matches(&self, value: &Value) -> bool {
        match &self.matching {
            ConformanceMatch::Kind(kind) => &value.r#trait.kind == kind,
            ConformanceMatch::Trait(r#trait) => &value.r#trait == r#trait,
        }
    }
}

impl Environment {
    pub fn add_conformance(
        &mut self,
        matching_trait: Trait,
        derived_trait: Trait,
        derive_value: impl Fn(&Value, &EnvironmentRef, Stack) -> Result + 'static,
    ) {
        self.conformances().push(Conformance::new(
            ConformanceMatch::Trait(matching_trait),
            derived_trait,
            derive_value,
        ))
    }

    pub fn add_primitive_conformance<A: Primitive, B: Primitive>(
        &mut self,
        derive_trait_value: impl Fn(A) -> B + 'static,
    ) {
        self.add_conformance(Trait::of::<A>(), Trait::of::<B>(), move |value, _, _| {
            let a = value.clone().into_primitive::<A>();
            let b = derive_trait_value(a);
            Ok(Value::of(b))
        });
    }

    pub fn add_text_conformance(&mut self, r#trait: Trait, value_name: &'static str) {
        self.add_conformance(r#trait, Trait::text(), move |value, env, stack| {
            let named = value.get_primitive_if_present::<Named>(env, stack)?;

            Ok(Value::of(Text::new(&match named {
                Some(named) => format!("<{} '{}'>", value_name, named.name),
                None => format!("<{}>", value_name),
            })))
        });
    }
}
