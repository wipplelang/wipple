use crate::*;
use std::rc::Rc;

#[derive(Clone)]
pub struct Conformance {
    pub validation: Validation,
    pub derived_trait: Trait,
    pub derive_value: Rc<dyn Fn(Value, &Environment, &Stack) -> Result>,
}

impl Conformance {
    pub fn new(
        validation: Validation,
        derived_trait: Trait,
        derive_value: impl Fn(Value, &Environment, &Stack) -> Result + 'static,
    ) -> Self {
        Conformance {
            validation,
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
            self.validation, self.derived_trait
        )
    }
}

#[derive(TypeInfo, Debug, Clone, Default)]
pub struct Conformances(pub Vec<Conformance>);

core_env_key!(pub conformances for Conformances {
    visibility: EnvironmentVisibility::Public(UseFn::from(|parent: &Conformances, new| {
        Conformances(parent.0.clone().into_iter().chain(new.0.clone()).collect())
    })),
});

impl EnvironmentInner {
    pub fn add_conformance(
        &mut self,
        validation: Validation,
        derived_trait: Trait,
        derive_value: impl Fn(Value, &Environment, &Stack) -> Result + 'static,
    ) {
        // New conformances always have a lower precedence than older conformances
        self.conformances()
            .0
            .push(Conformance::new(validation, derived_trait, derive_value))
    }

    pub fn add_primitive_conformance<A: TypeInfo, B: TypeInfo>(
        &mut self,
        derive_trait_value: impl Fn(A) -> B + 'static,
    ) {
        self.add_conformance(
            Validation::for_trait(Trait::of::<A>()),
            Trait::of::<B>(),
            move |value, _, _| {
                let a = value.into_primitive::<A>().unwrap();
                let b = derive_trait_value(a);
                Ok(Value::of(b))
            },
        );
    }

    pub fn add_text_conformance<T: TypeInfo>(&mut self, value_name: &'static str) {
        self.add_primitive_conformance(move |_: T| Text::new(&format!("<{}>", value_name)));
    }
}
