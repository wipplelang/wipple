use crate::*;
use std::rc::Rc;

pub type Conformances = Vec<Conformance>;

core_env_key!(pub conformances for Conformances {
    visibility: EnvironmentVisibility::Public(UseFn::from(|parent: &Conformances, new| {
        parent.clone().into_iter().chain(new.clone()).collect()
    })),
});

#[derive(Clone)]
pub struct Conformance {
    pub matching_trait: Trait,
    pub derived_trait: Trait,
    pub derive_value: Rc<dyn Fn(&Value, &EnvironmentRef, &Stack) -> Result>,
}

impl Conformance {
    pub fn new(
        matching_trait: Trait,
        derived_trait: Trait,
        derive_value: impl Fn(&Value, &EnvironmentRef, &Stack) -> Result + 'static,
    ) -> Self {
        Conformance {
            matching_trait,
            derived_trait,
            derive_value: Rc::new(derive_value),
        }
    }
}

impl std::fmt::Debug for Conformance {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "(Conformance {:?} == {:?})",
            self.matching_trait, self.derived_trait
        )
    }
}

impl Conformance {
    pub fn matches(&self, value: &Value) -> bool {
        value.r#trait == self.matching_trait
    }
}

impl Environment {
    pub fn add_conformance(
        &mut self,
        matching_trait: Trait,
        derived_trait: Trait,
        derive_value: impl Fn(&Value, &EnvironmentRef, &Stack) -> Result + 'static,
    ) {
        self.conformances().push(Conformance::new(
            matching_trait,
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

    pub fn add_text_conformance<T: Primitive>(&mut self, value_name: &'static str) {
        self.add_primitive_conformance(move |_: T| Text::new(&format!("<{}>", value_name)));
    }
}
